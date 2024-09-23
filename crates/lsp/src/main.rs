use std::sync::Arc;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

use dashmap::DashMap;

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

    fn get_text(&self) -> String {
        let session = self.session.clone();
        let result = session.input_text.get(&()).unwrap().clone();
        result
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
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

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let new_input = &params.content_changes.first().unwrap().text;

        self.update_text(new_input);
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let Position { line, character } = params.text_document_position_params.position;

        let input = self.get_text();

        self.client
            .log_message(
                MessageType::INFO,
                format!("Looking up {}:{} in {}", line, character, input),
            )
            .await;

        // now parse the file and typecheck it and all that shit

        let hover = Hover {
            contents: HoverContents::Scalar(MarkedString::String("Dog".into())),
            range: None,
        };
        Ok(Some(hover))
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
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
