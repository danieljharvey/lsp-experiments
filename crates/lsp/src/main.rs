use dashmap::DashMap;
use frame::{
    parser::Annotation,
    typecheck::{get_outer_type_annotation, TypeError},
    types::{Expr, Type},
};
use std::sync::Arc;
use tower_lsp::jsonrpc::Result as LspResult;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

#[derive(Debug)]
struct Backend {
    client: Client,
    session: Arc<Session>,
}

#[derive(Debug)]
struct Session {
    input_text: DashMap<(), String>,
}

impl Backend {
    fn update_text(&self, text: &str) {
        let session = self.session.clone();
        let _ = session.input_text.insert((), text.to_string());
    }

    fn get_text(&self) -> Option<String> {
        let session = self.session.clone();
        session.input_text.get(&()).map(|a| a.clone())
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> LspResult<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                ..ServerCapabilities::default()
            },

            server_info: None,
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "server initialized!")
            .await;
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let new_input = &params.text_document.text;

        self.update_text(new_input);
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        if let Some(new_input) = &params.content_changes.first() {
            self.update_text(&new_input.text)
        }
    }

    async fn hover(&self, params: HoverParams) -> LspResult<Option<Hover>> {
        let Position { line, character } = params.text_document_position_params.position;

        if let Some(input) = self.get_text() {
            self.client
                .log_message(
                    MessageType::INFO,
                    format!("Looking up {}:{} in {}", line, character, input),
                )
                .await;

            match compile(&input) {
                Ok(expr) => Ok(find_most_specific_type(&expr, line, character).map(|ty| {
                    let hover = Hover {
                        contents: HoverContents::Scalar(MarkedString::String(ty.to_string())),
                        range: Some(range_from_annotation(get_outer_type_annotation(ty))),
                    };
                    hover
                })),
                Err(_) => {
                    // for now an error is the end
                    let hover = Hover {
                        contents: HoverContents::Scalar(MarkedString::String("Error".into())),
                        range: None,
                    };

                    Ok(Some(hover))
                }
            }
            // now parse the file and typecheck it and all that shit
        } else {
            Ok(None)
        }
    }

    async fn shutdown(&self) -> LspResult<()> {
        Ok(())
    }
}

enum CompileError<'a> {
    ParseError,
    TypeError(TypeError<Annotation<'a>>),
}

fn range_from_annotation(ann: &Annotation) -> Range {
    Range {
        start: Position {
            character: ann.start.get_utf8_column().try_into().unwrap(),
            line: ann.start.location_line(),
        },
        end: Position {
            character: ann.end.get_utf8_column().try_into().unwrap(),
            line: ann.end.location_line(),
        },
    }
}

// do line/char sit within this annotation?
// this is so fucked
fn annotation_matches(ann: &Annotation, line: u32, character: u32) -> bool {
    let start_col: u32 = ann.start.get_utf8_column().try_into().unwrap();
    let start_line: u32 = ann.start.location_line();

    let end_col: u32 = ann.end.get_utf8_column().try_into().unwrap();
    let end_line: u32 = ann.end.location_line();

    let after_start = (line == start_col && character >= start_line) || line < start_line;

    let before_end = (line == end_col && character < end_line) || line < end_line;

    after_start && before_end
}

fn find_most_specific_type<'a>(
    expr: &'a Expr<Type<Annotation>>,
    line: u32,
    character: u32,
) -> Option<&'a Type<Annotation<'a>>> {
    match expr {
        Expr::EPrim { ann, .. } => {
            if annotation_matches(get_outer_type_annotation(ann), line, character) {
                Some(ann)
            } else {
                None
            }
        }
        Expr::EAnn { ann, expr, .. } => {
            if annotation_matches(get_outer_type_annotation(ann), line, character) {
                // the whole thing matches
                // maybe we can do better though
                find_most_specific_type(expr, line, character).or(Some(ann))
            } else {
                None
            }
        }
        Expr::EIf {
            ann,
            pred_expr,
            then_expr,
            else_expr,
        } => {
            if annotation_matches(get_outer_type_annotation(ann), line, character) {
                find_most_specific_type(pred_expr, line, character)
                    .or(find_most_specific_type(then_expr, line, character))
                    .or(find_most_specific_type(else_expr, line, character))
                    .or(Some(ann))
            } else {
                None
            }
        }
    }
}

fn compile(input: &str) -> Result<Expr<Type<Annotation>>, CompileError> {
    let (_, input_expr) =
        frame::parser::parse_expr(input.into()).map_err(|_| CompileError::ParseError)?;

    frame::typecheck::infer(&input_expr).map_err(CompileError::TypeError)
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let session = Session {
        input_text: DashMap::new(),
    };

    let (service, socket) = LspService::new(|client| Backend {
        client,
        session: Arc::new(session),
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
