{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils, ... }@inputs: (
    flake-utils.lib.eachDefaultSystem (system: {
      lib = import ./lib.nix system inputs;

      # `nix flake check` now checks format!
      checks.format = self.lib.${system}.mkCheck self;

      # `nix run .#format` formats in current directory!
      apps.format = self.lib.${system}.mkFormatApp self;
    }));
}
