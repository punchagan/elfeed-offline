{
  description = "";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            (final: prev: {
              ocamlPackages = prev.ocamlPackages.overrideScope (ocamlFinal: ocamlPrev: {
                uri = ocamlPrev.uri.overrideAttrs (prev': {
                  patches = (prev.patches or []) ++ [
                    (final.fetchpatch {
                      url = "https://github.com/punchagan/ocaml-uri/commit/ba6c3e6c36cfaece956190be9469f894f56d3124.patch";
                      hash = "sha256-LbyrC2npmB1jDUG4okM/hpA5+eNQv+PuExhbFK9gxF8=";
                    })
                  ];
                });
              });
            })
          ];
        };
      in
      {
        packages = rec {
          syndic = with pkgs; with ocamlPackages; buildDunePackage (finalAttrs: {
            pname = "syndic";
            version = "1.8.0";

            src = fetchFromGitHub {
              owner = "Cumulus";
              repo = "Syndic";
              rev = "v${finalAttrs.version}";
              hash = "sha256-UQ7EN+pvRjZ3qcdMilUBTuRMehe0ElyIPmqXmB7VN6o=";
            };

            propagatedBuildInputs = [
              ptime
              xmlm
              uri
            ];

            meta = {
              homepage = "https://github.com/Cumulus/Syndic";
              description = "RSS and Atom feed parsing";
              license = lib.licenses.mit;
            };
          });
          brr-lwd = with pkgs; with ocamlPackages; buildDunePackage (finalAttrs: {
            pname = "brr-lwd";
            inherit (lwd) version src;

            propagatedBuildInputs = [
              lwd
              brr
            ];

            meta = {
              description = "Make reactive webpages in Js_of_ocaml using Brr and Lwd";
              license = lib.licenses.mit;
              homepage = "https://github.com/let-def/lwd";
            };
          });

          elfeed-offline = pkgs.ocamlPackages.callPackage ./elfeed-offline.nix { inherit syndic brr-lwd; };
          default = elfeed-offline;
        };

        devShells.default = with pkgs; mkShell {
          packages = [
            pkg-config
            openssl
            gmp
            libev
            mkcert
            nss
            dune
          ];
        };
      }
    );
}
