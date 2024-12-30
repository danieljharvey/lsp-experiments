mod diagnostic;
mod hover;
use dashmap::DashMap;
use frame::{
    parser::Annotation,
    typecheck::{Env, TypeError, Warning},
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
                CompileResult {
                    expr_result: Ok(expr),
                    type_warnings: _,
                } => Ok(hover::find_most_specific_type(&expr, line, character)
                    .map(hover::hover_from_expr)),
                CompileResult {
                    expr_result: Err(_),
                    type_warnings: _,
                } => Ok(None),
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

enum CompileError {
    ParseError(Vec<frame::parser::ParseError>),
    TypeError(Box<TypeError<Annotation>>),
}

struct CompileResult {
    pub expr_result: Result<Expr<Type<Annotation>>, CompileError>,
    pub type_warnings: Vec<Warning<Annotation>>,
}

fn compile(input: &str) -> CompileResult {
    let (parse_result, errors) = frame::parser::parse_expr(input);
    let result = frame::parser::parse_block_to_expr(parse_result);

    match result {
        Ok(input_expr) => {
            // todo, also add parser `errs` here too
            // as we may partially parse but still be able to typecheck some stuff
            let mut env = Env::new();
            let expr_result = frame::typecheck::infer(&input_expr, &mut env)
                .map_err(|e| CompileError::TypeError(Box::new(e)));

            CompileResult {
                expr_result,
                type_warnings: env.warnings,
            }
        }
        Err(_) => CompileResult {
            expr_result: Err(CompileError::ParseError(errors)),
            type_warnings: vec![],
        },
    }
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
