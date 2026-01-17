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
        packages.${packageName} = mainPackage.overrideAttrs (_: {
          buildInputs = [ pkgs.hpack ];
          preConfigure = "hpack .";
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


        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            haskellPackages.haskell-language-server # you must build it with your ghc to work
            ghcid
            cabal-install
            postgresql
            redis
          ];
          inputsFrom = map (__getAttr "env") (__attrValues self.packages.${system});
        };
        devShell = self.devShells.${system}.default;
      });
}
