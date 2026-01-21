# SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io/>
#
# SPDX-License-Identifier: CC0-1.0

{
  description = "An application for shortening URLs";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskellPackages;

        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));

        # DON'T FORGET TO PUT YOUR PACKAGE NAME HERE, REMOVING `throw`
        packageName = "url-shortener";
        exeName = "url-shortener";
        cliName = "urlsh";

        mainPackage =
          haskellPackages.callCabal2nix packageName self rec {
            # Dependency overrides go here
          };

      in {
        packages.${packageName} = mainPackage.overrideAttrs (oldAttrs: {
          buildInputs = (oldAttrs.buildInputs or []) ++ [ pkgs.hpack ];
          preConfigure = (oldAttrs.preConfigure or "") + "\nhpack .";
          DB_MAIN_USER = "urlsh";
          DB_TEST_USER = "urlsh_test";
          R_PORT = 2626;

          doCheck = false;
        });

        packages.default = self.packages.${system}.${packageName};
        defaultPackage = self.packages.${system}.default;

        apps.default = {
          type = "app";
          meta.description = "scotty server";
          program = "${self.packages.${system}.default}/bin/${exeName}";
        };
        apps.cli = {
          type = "app";
          meta.description = "cli app";
          program = "${self.packages.${system}.default}/bin/${cliName}";
        };
        apps.startPostgres = {
          type = "app";
          meta.description = "run to start postgres server";
          program =
            let
              script = pkgs.writeShellApplication {
                name = "startDb";
                runtimeInputs = [ pkgs.postgresql ];
                text = ''
                  # Initialize database
                  [ ! -d "./data/db" ] && initdb --no-locale -D ./data/db

                  # Start postgres
                  # -D database directory
                  # -k unix-domain socket location
                  # -l logfile
                  pg_ctl start -l "$PWD"/data/postgres.log -D "$PWD"/data/db -o "-k $PWD/data"
                '';
              };
            in "${script}/bin/startDb";
        };
        apps.startRedis = {
          type = "app";
          meta.description = "run to start redis server";
          program =
            let
              script = pkgs.writeShellApplication {
                name = "startDb";
                runtimeInputs = [ pkgs.postgresql ];
                text = ''
                  # Redis conf
                  ./create-redis-conf.sh

                  # Start redis
                  redis-server redis.conf &
                '';
              };
            in "${script}/bin/startDb";
        };
        apps.createDB = {
          type = "app";
          meta.description = "run once to setup the tables";
          program =
            let
              script = pkgs.writeShellApplication {
                name = "createDB";
                runtimeInputs = [ pkgs.postgresql ];
                text = ''
                  # Create a database of your current user
                  # -h database server host or socket directory
                  # -l list available databases, then exit
                  # -q run quietly (no messages, only query output)
                  # -t print rows only
                  if ! psql -h "$PWD"/data -lqt | cut -d\| -f1 | grep -qw "$DB_MAIN_USER"; then
                    createuser -h "$PWD"/data -dls "$DB_MAIN_USER"
                    createdb -h "$PWD"/data -U "$DB_MAIN_USER" "$DB_MAIN_USER"
                  fi
                  if ! psql -h "$PWD"/data -lqt | cut -d\| -f1 | grep -qw "$DB_TEST_USER"; then
                    createuser -h "$PWD"/data -dls "$DB_TEST_USER"
                    createdb -h "$PWD"/data -U "$DB_TEST_USER" "$DB_TEST_USER"
                  fi

                  # Load DB dump
                  psql -h "$PWD"/data -d "$DB_MAIN_USER" -U "$DB_MAIN_USER" < data/db.sql
                  psql -h "$PWD"/data -d "$DB_TEST_USER" -U "$DB_TEST_USER" < data/db.sql
                '';
              };
            in "${script}/bin/createDB";
        };
        apps.test = {
          type = "app";
          meta.description = "run to start redis server";
          program =
            let
              script = pkgs.writeShellApplication {
                name = "test";
                runtimeInputs = [ pkgs.postgresql ];
                text = ''
                  hpack . && cabal test ; rm url-shortener.cabal
                '';
              };
            in "${script}/bin/test";
        };


        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            haskellPackages.haskell-language-server # you must build it with your ghc to work
            ghcid
            cabal-install
            postgresql
            redis
          ];
          DB_MAIN_USER = "urlsh";
          DB_TEST_USER = "urlsh_test";
          R_PORT = 2626;
          inputsFrom = map (__getAttr "env") (__attrValues self.packages.${system});
        };
        devShell = self.devShells.${system}.default;
      });
}
