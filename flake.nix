{
    description = "Homepage nix flake";
    inputs = {
        nixpkgs.url = "nixpkgs/nixos-24.05";
    };
    outputs = { self, nixpkgs }: let
        system = "x86_64-linux";
        pkgs = import nixpkgs { inherit system; };
        lib = pkgs.lib;
    in rec {
        devShells.${system}.default = pkgs.mkShell {
            packages = with pkgs.haskellPackages; [(ghcWithPackages (hs: with hs; [
                cabal-install
            ]))];
        };
        packages.${system} = let 
            version = "0.1.0.0";
        in {
            homepage = pkgs.haskellPackages.mkDerivation {
                pname = "homepage-bin";
                version = version;
                src = ./.;
                isLibrary = false;
                isExecutable = true;
                executableHaskellDepends = with pkgs.haskellPackages; [
                    base blaze-builder blaze-html bytestring http-types ihp-hsx
                    sqlite-simple text time utf8-string uuid wai warp directory
                    aeson
                ];
                license = "unknown";
                mainProgram = "homepage";
            };

            default = pkgs.stdenv.mkDerivation {
                name = "homepage";
                version = version;
                src = ./.;

                installPhase = ''
                    mkdir -p $out/bin
                    ln -s ${packages.${system}.homepage}/bin/homepage $out/bin/homepage
                    cp -r $src/static $out
                '';
            };

            vm = (nixpkgs.lib.nixosSystem {
                inherit system;
                modules = [nixosModules.${system}.default {
                    system.stateVersion = "24.05";
                    users.users.root.password = "1234";
                    virtualisation.vmVariant = {
                        virtualisation.graphics = false;
                        virtualisation.forwardPorts = (map (port: {
                            from = "host";
                            host.port = port;
                            guest.port = port;
                        }) [8000 2222]);
                    };
                    services.getty.autologinUser = "root";
                    networking.firewall.enable = false;
                    services.openssh.enable = true;
                    services.openssh.ports = [2222];
                    services.openssh.settings.PermitRootLogin = "yes";
                    environment.systemPackages = with pkgs; [
                        sqlite-interactive  
                    ];

                    services.homepage = {
                        enable = true;
                        port = 8000;
                    };
                }];
            }).config.system.build.vm;
        };
        nixosModules.${system}.default = {config, pkgs, lib, ...}: let
            cfg = config.services.homepage;
        in {
            options.services.homepage = {
                enable = lib.mkOption {
                    type = lib.types.bool;
                    default = false;
                };
                port = lib.mkOption {
                    type = lib.types.int;
                    default = 8000;
                };
                db.path = lib.mkOption {
                    type = lib.types.str;
                    default = "/var/db/homepage.db3";
                };
            };

            config.systemd.services.website = {
                enable = cfg.enable;
                environment.HOMEPAGE_PORT = builtins.toString cfg.port;
                environment.HOMEPAGE_DB = cfg.db.path;
                serviceConfig = {
                    WorkingDirectory = "${packages.${system}.default}";
                    ExecStart = "${packages.${system}.default}/bin/homepage";
                    StandardOutput = "syslog";
                    StandardError = "syslog";
                };
                wantedBy = ["default.target"];

            };
        };
    };
}
