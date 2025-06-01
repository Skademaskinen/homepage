{ nixpkgs, self, system, ... }:

let
    pkgs = import nixpkgs { inherit system; };
in

(nixpkgs.lib.nixosSystem {
    inherit system;
    modules = [self.nixosModules.default ({config, ...}: {
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
}).config.system.build.vm
