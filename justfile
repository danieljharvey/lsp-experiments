watch:
    cargo watch -i "**/*.snap.new" \
      -x test \
      -x clippy \
      -s 'cargo install --path ./crates/lsp' \
      -s 'cargo install --path ./crates/compiler'

run-watch:
    watchexec -e .frame 'clear && frame --file-path ./static/test.frame'

format:
    cargo fmt --all

machete:
    cargo machete

fix:
    cargo clippy --fix --no-deps --allow-dirty
