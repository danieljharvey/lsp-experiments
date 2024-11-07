# lsp-experiments

How does the design of a compiler change when we're designing it to have LSP support from the ground up.

Implemented so far:

- Parsing must always succeed (but create a partial AST). This allows us to shows multiple parse errors at once ala [this excellent blog](https://eyalkalderon.com/blog/nom-error-recovery/).

Next to come:

- Given a bunch of functions, where some of them don't parse, we can at least try to typecheck others using the top-level function type signatures etc
