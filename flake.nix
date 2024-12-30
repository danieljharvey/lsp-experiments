{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?branch=nixos-unstable";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, utils }:
    utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
      {
        devShell = with pkgs; mkShell {
          buildInputs = [
            rustc
            rust-analyzer
            cargo
            clippy
            cargo-watch
            rustfmt
            nixd
            just
            git
          ];
        };
      }
    );
}
