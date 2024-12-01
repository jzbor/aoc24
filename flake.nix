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
    days = [
      "01"
    ];
  in {
    packages = {
      default = self.packages.${system}.aoc24;
    } // (builtins.listToAttrs (map (day: {
      name = "task${day}";
      value = buildSML {
        name = "task${day}";
        entryFile = "${day}/${day}.sml";
        src = ./.;
      };
    }) days)) // {
      aoc24 = pkgs.symlinkJoin {
        name = "aoc24";
        paths = map (day: self.packages.${system}."task${day}") days;
      };
    };

    devShells.default = pkgs.mkShellNoCC {
      inherit (self.packages.${system}.default) name;
      nativeBuildInputs = with pkgs; [
        smlnj
        polyml
        gcc
      ];
    };
  });
}
