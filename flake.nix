{
  description = "REPLACEME";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    cf.url = "github:jzbor/cornflakes";
  };

  outputs = { self, nixpkgs, cf, ... }: (cf.mkLib nixpkgs).flakeForDefaultSystems (system:
  let
    pkgs = nixpkgs.legacyPackages.${system};
    buildSML = args: pkgs.stdenv.mkDerivation ({
      buildPhase = ''
        cd "$(dirname "${args.entryFile}")"
        filename=
        ${args.polymlPkg or pkgs.polyml}/bin/polyc "$(basename "${args.entryFile}")" -o "${args.name}"
        cd -
      '';
      installPhase = ''
        cd "$(dirname "${args.entryFile}")"
        mkdir -p $out/bin
        cp "${args.name}" $out/bin/
      '';
    } // args);
  in {
    packages = rec {
      default = task01;
      task01 = buildSML {
        name = "task01";
        entryFile = "01/01.sml";
        src = ./.;
      };
    };

    devShells.default = pkgs.mkShellNoCC {
      inherit (self.packages.${system}.default) name;
      nativeBuildInputs = [ pkgs.hello ];
    };
  });
}
