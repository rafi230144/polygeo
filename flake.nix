{
  description = ''
    polygeo:
    Summing products of multivariate polynomials and exponentials over tuples of naturals
  '';

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";

  outputs = { self, nixpkgs }:
    let
      systems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-linux"
        "aarch64-darwin"
        "i686-linux"
      ];

      ghcVersion = "914";

      devShellOf = system:
        let
          pkgs = import nixpkgs { inherit system; };
          hask = pkgs.haskell.packages."ghc${ghcVersion}";
        in
          {
            default = pkgs.mkShell {
              buildInputs = [
                hask.ghc
                pkgs.cabal-install
              ];
            };
          };
    in {
      devShells = nixpkgs.lib.genAttrs systems devShellOf;
    };
}