{
    description = "Homepage nix flake";
    inputs = {
        nixpkgs.url = "nixpkgs/nixos-unstable";
    };
    outputs = { self, nixpkgs } @ _inputs: let
        system = "x86_64-linux";
        pkgs = import nixpkgs { inherit system; };
        version = "0.1.2";
        inputs = _inputs // { inherit system version; };
    in {
        devShells.${system}.default = pkgs.mkShell {
            packages = with pkgs; [
                (haskellPackages.ghcWithPackages (hs: with hs; [
                    cabal-install
                    matplotlib
                    haskell-language-server
                    (python311.withPackages (py: with py; [
                        matplotlib
                        ipython
                    ]))
                ] ++ ((pkgs.callPackage ./nix-support/package.nix { inherit version; }).buildInputs)))
                (python311.withPackages (py: with py; [
                    matplotlib
                    ipython
                    scipy
                ]))
                sqlite-interactive
 
            ];
        };
        packages.${system} = {
            homepage = pkgs.callPackage ./nix-support/package.nix { inherit version; };

            default = pkgs.stdenv.mkDerivation {
                inherit version;
                name = "homepage";
                src = ./.;

                installPhase = ''
                    mkdir -p $out/bin
                    ln -s ${self.packages.${system}.homepage}/bin/homepage $out/bin/homepage
                    ln -s ${self.packages.${system}.homepage}/bin/repl-homepage $out/bin/repl-homepage
                    cp -r $src/static $out
                '';
            };

            vm = import ./nix-support/vm.nix inputs;
        };
        nixosModules.default = import ./nix-support/module.nix inputs;
    };
}
