{
    description = "Homepage nix flake";
    inputs = {
        nixpkgs.url = "nixpkgs/nixos-unstable";
    };
    outputs = { self, nixpkgs }: let
        system = "x86_64-linux";
        pkgs = import nixpkgs { inherit system; };
        lib = pkgs.lib;
    in rec {
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
                ] ++ ((pkgs.callPackage ./nix-support/package.nix {}).buildInputs)))
                (python311.withPackages (py: with py; [
                    matplotlib
                    ipython
                    scipy
                ]))
 
            ];
        };
        packages.${system} = let 
            version = "0.1.0.0";
        in {
            homepage = pkgs.callPackage ./nix-support/package.nix {};

            default = pkgs.stdenv.mkDerivation {
                name = "homepage";
                version = version;
                src = ./.;

                installPhase = ''
                    mkdir -p $out/bin
                    ln -s ${packages.${system}.homepage}/bin/homepage $out/bin/homepage
                    ln -s ${packages.${system}.homepage}/bin/repl-homepage $out/bin/repl-homepage
                    cp -r $src/static $out
                '';
            };

            vm = (nixpkgs.lib.nixosSystem {
                inherit system;
                modules = [nixosModules.${system}.default ({config, ...}: {
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
                        mariadb
                        self.packages.${system}.default
                    ];

                    services.homepage = {
                        enable = true;
                        port = 8000;
                    };
                    services.mysql = let
                      cfg = config.services.homepage;
                    in {
                        enable = true;
                        package = pkgs.mariadb;
                        ensureUsers = [
                            {
                                name = cfg.db.user;
                                ensurePermissions."${cfg.db.name}.*" = "ALL PRIVILEGES";
                            }
                            {
                                name = "${cfg.db.user}@localhost";
                                ensurePermissions."${cfg.db.name}.*" = "ALL PRIVILEGES";
                            }
                        ];
                        ensureDatabases = [cfg.db.name];
                    };


                })];
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
                db.name = lib.mkOption {
                    type = lib.types.str;
                    default = "homepage";
                };
                db.user = lib.mkOption {
                    type = lib.types.str;
                    default = "homepage";
                };
                editor.root = lib.mkOption {
                    type = lib.types.str;
                    default = "/var/run/website/editor";
                };
            };

            config.systemd.services.homepage = {
                enable = cfg.enable;
                environment.HOMEPAGE_PORT = builtins.toString cfg.port;
                environment.HOMEPAGE_DB = cfg.db.name;
                environment.HOMEPAGE_DB_USER = cfg.db.user;
                environment.HOMEPAGE_EDITOR_ROOT = cfg.editor.root;
                serviceConfig = {
                    User = cfg.db.user;
                    WorkingDirectory = "${packages.${system}.default}";
                    #ExecStart = "/usr/bin/env echo test";
                    ExecStart = "${packages.${system}.default}/bin/homepage";
                    StandardOutput = "syslog";
                    StandardError = "syslog";
                };
                wantedBy = ["default.target"];
                after = ["mysql.service"];

            };
            config.users = if cfg.enable then {
                users.${cfg.db.user} = {
                    isNormalUser = true;
                    group = "homepage";
                };
                groups.homepage = {};
            } else {};

        };
    };
}
