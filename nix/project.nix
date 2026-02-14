{ indexState, pkgs, ... }:

let
  shell =
    { pkgs, ... }:
    {
      tools = {
        cabal = {
          index-state = indexState;
        };
        haskell-language-server = {
          index-state = indexState;
        };
        hoogle = {
          index-state = indexState;
        };
        fourmolu = {
          index-state = indexState;
        };
        hlint = {
          index-state = indexState;
        };
      };
      withHoogle = true;
      buildInputs = [
        pkgs.just
        pkgs.nixfmt-classic
        pkgs.shellcheck
      ];
    };

  mkProject =
    { lib, pkgs, ... }:
    {
      name = "cardano-coin-selection";
      src = ./..;
      compiler-nix-name = "ghc9122";
      shell = shell { inherit pkgs; };
    };

  project = pkgs.haskell-nix.cabalProject' mkProject;

in
{
  devShells.default = project.shell;
  inherit project;
  packages.lib = project.hsPkgs.cardano-coin-selection.components.library;
  packages.unit-tests = project.hsPkgs.cardano-coin-selection.components.tests.unit;
}
