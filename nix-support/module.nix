{ self, system, ... }:

{config, pkgs, lib, ...}: let
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
        db.host = lib.mkOption {
            type = lib.types.str;
            default = "localhost";
        };
        db.port = lib.mkOption {
            type = lib.types.int;
            default = 3306;
        };
        db.password = lib.mkOption {
            type = lib.types.str;
            default = "";
        };
        db.dialect = lib.mkOption {
            type = lib.types.str;
            default = "mysql";
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
        environment.HOMEPAGE_DB_HOST = cfg.db.host;
        environment.HOMEPAGE_DB_PORT = toString cfg.db.port;
        environment.HOMEPAGE_DB_PASSWORD = cfg.db.password;
        environment.HOMEPAGE_DIALECT = "mysql";
        environment.HOMEPAGE_EDITOR_ROOT = cfg.editor.root;
        path = with pkgs; [(python311.withPackages (py: with py; [
            matplotlib
            scipy
        ]))];
        serviceConfig = {
            User = cfg.db.user;
            WorkingDirectory = "${self.packages.${system}.default}";
            #ExecStart = "/usr/bin/env echo test";
            ExecStart = "${self.packages.${system}.default}/bin/homepage";
            StandardOutput = "syslog";
            StandardError = "syslog";
        };
        wantedBy = ["default.target"];

    };
    config.users = if cfg.enable then {
        users.${cfg.db.user} = {
            isNormalUser = true;
            group = "homepage";
            packages = with pkgs; [
                (python311.withPackages (py: with py; [
                    matplotlib
                    scipy
                ]))
            ];
        };
        groups.homepage = {};
    } else {};

}
