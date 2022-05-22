system:

{ self, nixpkgs, ... } @ inputs:

let
  pkgs = nixpkgs.legacyPackages.${system};
  lib = pkgs.lib;

  inherit (lib) getExe escapeShellArg;
in

rec {
  mkTreefmtToml = srcTree:
    let
      filePresent = pattern:
        import
          (
            pkgs.runCommandLocal "pattern-match.nix"
              {
                # Can be enabled once we recommend content addressing
                # -- since we only generate one of two files.
                # __contentAddressed = true;
              }
              ''
                find ${escapeShellArg srcTree} -name ${escapeShellArg pattern} \
                  | (grep . > /dev/null && echo "true" || echo "false") > $out
              ''
          );

      formatters = {
        terraform = {
          command = getExe pkgs.bash;
          options = [
            "-euc"
            ''
              for f in "$@"; do
                ${getExe pkgs.terraform} fmt "$f"
              done
            ''
            "--"
          ];
          includes = [ "*.tf" ];
        };

        haskell = {
          command = getExe pkgs.bash;
          options =
            # Fourmolu not having an option to specify the config is so dumb...
            let fourmoluConfig = pkgs.runCommandLocal "fourmoluConfigHome" { } ''
              mkdir $out
              cp ${./fourmolu.yaml} $out/
            '';
            in
            [
              "-euc"
              ''
                export XDG_CONFIG_HOME=${fourmoluConfig}
                ${getExe pkgs.haskellPackages.fourmolu} \
                  -o-XImportQualifiedPost \
                  "$@"
              ''
              "--"
            ];
          includes = [ "*.hs" ];
        };

        nix = {
          command = getExe pkgs.nixpkgs-fmt;
          includes = [ "*.nix" ];
        };
      };

      filteredFormatters = lib.filterAttrs
        (k: v: lib.any filePresent v.includes)
        formatters;

      treefmtStruct = {
        formatter = formatters;
      };

      treefmtJson = pkgs.writeTextFile {
        name = "treefmt.json";
        text = builtins.toJSON treefmtStruct;
      };

      treefmtToml = pkgs.runCommandLocal "treefmt.toml" { } ''
        ${getExe pkgs.remarshal} --if json --of toml ${treefmtJson} $out
      '';
    in
    treefmtToml;
  mkCheck = src:
    pkgs.runCommandLocal "checked-src"
      {
        nativeBuildInputs = with pkgs; [ treefmt ];
      }
      ''
        mkdir -p /tmp/home
        export HOME=/tmp/home

        # set -euxo pipefail
        cp -r ${src} ./tree/
        chmod -R u=rwx,go=rx tree

        cd tree
        # ls -la

        CONFIG=${mkTreefmtToml src}
        echo CONFIG: $CONFIG

        treefmt --version
        treefmt \
          --config-file $CONFIG \
          --tree-root . \
          --fail-on-change \
          -vv

        touch $out
      '';

  mkFormatApp = src:
    let
      script = pkgs.writeScript "format" ''
        set -euo pipefail

        ROOT=./$(${getExe pkgs.git} rev-parse --show-cdup)
        ${getExe pkgs.treefmt} --config-file ${mkTreefmtToml src} --tree-root $ROOT "$@"
      ''; in
    {
      type = "app";
      program = "${script}";
    };
}
