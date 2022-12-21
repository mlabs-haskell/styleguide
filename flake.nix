{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils, ... }@inputs: (
    { herculesCI.ciSystems = [ "x86_64-linux" ]; } //
    flake-utils.lib.eachDefaultSystem (system: {
      lib = import ./lib.nix system inputs;

      # `nix flake check` now checks format!
      checks.format = self.lib.${system}.mkCheck self;

      # `nix run .#format` formats in current directory!
      apps.format = self.lib.${system}.mkFormatApp self;

      # `nix fmt` formats in current directory
      formatter = self.lib.${system}.mkFormatter self;
    })
  );
}
