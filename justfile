watch:
    cargo watch -x test -x clippy

format:
    cargo fmt --all

machete:
    cargo machete

fix:
    cargo clippy --fix --no-deps --allow-dirty
