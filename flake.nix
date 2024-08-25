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
        packages.${system}.default = pkgs.haskellPackages.mkDerivation {
            pname = "homepage";
            version = "0.1.0.0";
            src = ./.;
            isLibrary = false;
            isExecutable = true;
            executableHaskellDepends = with pkgs.haskellPackages; [
                base blaze-builder blaze-html bytestring http-types ihp-hsx
                sqlite-simple text time utf8-string uuid wai warp
            ];
            license = "unknown";
            mainProgram = "homepage";
        };
    };
}
