{
    description = "Homepage nix flake";
    inputs = {
        nixpkgs.url = "nixpkgs/nixos-24.05";
    };
    outputs = { self, nixpkgs }: let
        system = "x86_64-linux";
        pkgs = import nixpkgs { inherit system; };
    in {
        devShells.${system}.default = pkgs.mkShell {
            packages = with pkgs; [(haskellPackages.ghcWithPackages (hs: with hs; [
                cabal-install
                ghcide
            ]))];
        };
    };
}
