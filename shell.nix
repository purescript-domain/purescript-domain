let
  pkgs = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/22.11.tar.gz";
  }) {};

  # To update to a newer version of easy-purescript-nix:
  # 1. Obtain the commit hash <rev> via `curl https://api.github.com/repos/justinwoo/easy-purescript-nix/commits/master`.
  # 2. Obtain the sha256 hash <sha256> via `nix-prefetch-url --unpack https://github.com/justinwoo/easy-purescript-nix/archive/<rev>.tar.gz`.
  # 3. Update the <rev> and <sha256> below.
  pursPkgs = import (pkgs.fetchFromGitHub {
    owner = "f-f";
    repo = "easy-purescript-nix";
    rev = "b02cff3db1671fc8fb76a680597a216a9c9b2d03";
    sha256 = "0ihh6h29jy9gc9kfal5jgd98cmb24ccxl8kkvsl7ddb701bypz5x";
  }) {inherit pkgs;};
in
  pkgs.stdenv.mkDerivation {
    name = "purescript-domain";
    buildInputs = with pursPkgs;
      [
        purs
        spago-next
        pulp
        purs-tidy
      ]
      ++ (with pkgs; [
        esbuild
        nodejs-16_x
        nodePackages.bower
        watchexec
      ]);
  }
