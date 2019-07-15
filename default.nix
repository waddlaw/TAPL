let
  pkgs = import ./nix/nixpkgs;
  gitignoreSource = import ./nix/gitignore { inherit (pkgs) lib; };
  compiler = "ghc865";
  source = gitignoreSource ./.;
  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = (self: super:
    super // {
      "tapl" = super.callCabal2nix "tapl" source { };
    });
  };
in {
  tapl = haskellPackages.tapl;
  tapl-shell = haskellPackages.shellFor {
    packages = ps: [ ps.tapl ];
    buildInputs = [ pkgs.cabal-install ];
  };
}