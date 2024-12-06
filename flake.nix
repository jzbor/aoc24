{
  description = "REPLACEME";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    cf.url = "github:jzbor/cornflakes";
    parcels.url = "github:jzbor/nix-parcels";
  };

  outputs = { self, nixpkgs, cf, parcels, ... }: (cf.mkLib nixpkgs).flakeForDefaultSystems (system:
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
      "02"
      "03"
      "04"
      "05"
      "06"
    ];
    wrappedSmlnj = pkgs.symlinkJoin {
      name = "wrapped-smlnj";
      paths = with pkgs; [
        (pkgs.writeShellApplication {
          name = "sml";
          text = "${rlwrap}/bin/rlwrap ${smlnj}/bin/sml \"$@\"";
        })
        smlnj
      ];
    };
    fetchInput = pkgs.writeShellApplication {
      name = "aoc-fetch-input";
      text = ''
        if [ "$#" != 2 ]; then
          echo "Usage: $0 <day> <file>"
          exit 1
        fi

        curl "https://adventofcode.com/2024/day/$1/input" --cookie "session=$(cat cookie)" > "$2"
      '';
    };
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
        gcc
        mlton
        polyml
        parcels.packages.${system}.millet

        fetchInput
        wrappedSmlnj
      ];
    };

    apps.fetch-input = {
      type = "app";
      program = "${fetchInput}/bin/aoc-fetch-input";
    };
  });
}
