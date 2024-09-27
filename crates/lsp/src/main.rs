mod diagnostic;

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

        self.client
            .publish_diagnostics(
                params.text_document.uri,
                diagnostic::get_diagnostics(compile(new_input))
                    .iter()
                    .map(diagnostic::to_diagnostic)
                    .collect(),
                None,
            )
            .await
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        if let Some(new_input) = &params.content_changes.first() {
            self.update_text(&new_input.text);

            self.client
                .publish_diagnostics(
                    params.text_document.uri,
                    diagnostic::get_diagnostics(compile(&new_input.text))
                        .iter()
                        .map(diagnostic::to_diagnostic)
                        .collect(),
                    None,
                )
                .await
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
                Ok(expr) => Ok(
                    find_most_specific_type(&expr, line, character).map(|inner_expr| {
                        let hover = Hover {
                            contents: HoverContents::Scalar(MarkedString::String(
                                to_popup_message(inner_expr),
                            )),
                            range: Some(range_from_annotation(get_outer_type_annotation(
                                frame::typecheck::get_outer_expr_annotation(inner_expr),
                            ))),
                        };
                        hover
                    }),
                ),
                Err(_) => Ok(None),
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
    let start_col_raw: u32 = ann.start.get_utf8_column().try_into().unwrap();
    let end_col_raw: u32 = ann.end.get_utf8_column().try_into().unwrap();

    Range {
        start: Position {
            character: start_col_raw - 1,
            line: ann.start.location_line(),
        },
        end: Position {
            character: end_col_raw - 1,
            line: ann.end.location_line(),
        },
    }
}

// do line/char sit within this annotation?
// this is so fucked
fn annotation_matches(ann: &Annotation, cursor_line: u32, cursor_col: u32) -> bool {
    let start_col_raw: u32 = ann.start.get_utf8_column().try_into().unwrap();
    let start_col: u32 = start_col_raw - 1;
    let start_line: u32 = ann.start.location_line() - 1;

    let end_col_raw: u32 = ann.end.get_utf8_column().try_into().unwrap();
    let end_col: u32 = end_col_raw - 1;
    let end_line: u32 = ann.end.location_line() - 1;

    let after_start =
        (cursor_line == start_line && cursor_col >= start_col) || cursor_line < start_line;

    let before_end = (cursor_line == end_line && cursor_col < end_col) || cursor_line < end_line;

    after_start && before_end
}

#[test]
fn test_matches() {
    let (_, expr) = frame::parser::parse_expr("if True then (1: Int64) else 2".into()).unwrap();

    let typed_expr = frame::typecheck::infer(&expr).unwrap();

    let tests = vec![
        (0, 2, None),
        (1, 29, None),
        (0, 13, Some("1: Int64".into())),
        (0, 18, Some("1: Int64".into())),
        (0, 29, Some("2: Int64".into())),
        (0, 3, Some("True: Boolean".into())),
    ];

    for (x, y, result) in tests {
        assert_eq!(
            find_most_specific_type(&typed_expr, x, y).map(to_popup_message),
            result
        );
    }
}

fn to_popup_message<Ann>(expr: &Expr<Type<Ann>>) -> String {
    let ty = frame::typecheck::get_outer_expr_annotation(expr);
    format!("{}: {}", expr, ty)
}

fn find_most_specific_type<'a>(
    expr: &'a Expr<Type<Annotation>>,
    line: u32,
    character: u32,
) -> Option<&'a Expr<Type<Annotation<'a>>>> {
    match expr {
        Expr::EPrim { ann, .. } => {
            if annotation_matches(get_outer_type_annotation(ann), line, character) {
                Some(expr)
            } else {
                None
            }
        }
        Expr::EAnn { ann, expr, .. } => {
            if annotation_matches(get_outer_type_annotation(ann), line, character) {
                // the whole thing matches
                // maybe we can do better though
                find_most_specific_type(expr, line, character).or(Some(expr))
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
