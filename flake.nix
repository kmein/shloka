{
  description = "Analysing the anushtubh verses of India's great epics";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs";

  outputs = { self, nixpkgs }:
  let
    system = "x86_64-linux";
    pkgs = import nixpkgs { inherit system; };

    pythonInstallation = pkgs.python3.withPackages (p: [
      p.pandas
      p.jupyter
      p.matplotlib
      p.seaborn
      p.tabulate
      p.papermill
      p.scipy
      indic-transliteration
    ]);

    indic-transliteration = with pkgs.python3Packages; buildPythonPackage {
      pname = "indic_transliteration";
      version = "2.3.40";
      format = "wheel";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/fa/0d/d917def2e12fcf906253aca886b1705297a5e723cffd345ed5114ae5dba4/indic_transliteration-2.3.40-py3-none-any.whl";
        hash = "sha256-RHc02GVWBLxEsrPeaGMJ5KXTZ9/8HEm+j1qHB5ZssPw=";
      };
      propagatedBuildInputs = [ regex toml typer roman backports_functools_lru_cache ];
    };

    mahabharata = let
      fetchMbh = book: fetchurlArgs: rec {
        name = "MBh${nixpkgs.lib.strings.fixedWidthNumber 2 book}.txt";
        path = pkgs.fetchurl ({
          url = "https://bombay.indology.info/mahabharata/text/ASCII/${name}";
        } // fetchurlArgs);
      };
    in pkgs.linkFarm "mahabharata" [
      (fetchMbh 01 { hash = "sha256-tqN9IpafFzFsQB/Qdv8FQfRhNafIFqB1/oa+ske/47E="; postFetch = "${pkgs.gnused}/bin/sed -i s/eLako/eDako/g $out"; })
      (fetchMbh 02 { hash = "sha256-DgRxBbpZ3F94e4YrqiuGpk9JNfVszJACWX9VdYGqUNk="; })
      (fetchMbh 03 { hash = "sha256-ekaoFq+SkSHnBi4Y3bjsyXBmBZ2Zzr8364wVfxf+mH0="; })
      (fetchMbh 04 { hash = "sha256-e0Dt2bppAhIvpNEnVV0aUrnjeCtewty97wd++Y93hEQ="; })
      (fetchMbh 05 { hash = "sha256-AaBQ+v1z8uoTR9qtwVO9BCqM+dsNq5Zr1s/4GNelBcs="; })
      (fetchMbh 06 { hash = "sha256-qe9NecSgOSjlcZg9GEIZwGrntP33Tg4/GvDLKrQ8lKw="; })
      (fetchMbh 07 { hash = "sha256-i7/BZ6U1E7MvU5jBuSGDLz7kRfRwqxaSQ3/tTvQCLi4="; })
      (fetchMbh 08 { hash = "sha256-6GByL9XATWpqY8BmO4kMNyhfWZTz4jUUySZJwI3u9Rg="; })
      (fetchMbh 09 { hash = "sha256-ODGgElAXz97bmEZw4ZsECTUkitlmKqo+FmoaE+IrgjE="; })
      (fetchMbh 10 { hash = "sha256-wyoJYc4iEQeNQaWm0Y7bcVkRFvIwHt7bF02SFf2MVt0="; })
      (fetchMbh 11 { hash = "sha256-kzn/lHchLcHh4i4zUWaDOoorJuFsctstbZzRbIoFFhc="; })
      (fetchMbh 12 { hash = "sha256-4z7WTHTWuAs/uc2umsZ302WF+E+unkJFI3tDmq1jOLo="; })
      (fetchMbh 13 { hash = "sha256-pYTb38TGQBHhWKaczQcEx0d799mTaG7sS74WTMTFr+Y="; postFetch = "${pkgs.gnused}/bin/sed -i s/dramiLAz/dramiDAz/g $out"; })
      (fetchMbh 14 { hash = "sha256-Vp+yDEmshVwD/3mzVB7UqebIJAHTdyUMShSPY0TU7B4="; })
      (fetchMbh 15 { hash = "sha256-LryOEp3VnRuruVQ63HsmUxq6HFfHC5sp6iFvxLUoaSc="; })
      (fetchMbh 16 { hash = "sha256-X1MLW7AdzVUliKC6+TMt93BVJAMTgiKPvu1YeienCwU="; })
      (fetchMbh 17 { hash = "sha256-+00bD4Q1BDfwCmewUXBSivn6kZKZUX+ZDAXrDt7sUjE="; })
      (fetchMbh 18 { hash = "sha256-GNj+/DBXa4BioO7K1Wd6qp4gYC9RBu8/j7jjqf2+RAo="; })
    ];

    ramayana = let
      fetchRam = book: fetchurlArgs: rec {
        name = "Ram${nixpkgs.lib.strings.fixedWidthNumber 2 book}.txt";
        path = pkgs.fetchurl ({
          url = "https://bombay.indology.info/ramayana/text/ASCII/${name}";
        } // fetchurlArgs);
      };
    in pkgs.linkFarm "ramayana" [
      (fetchRam 01 { hash = "sha256-VWPdOhT9KSFbGLaW/BK0MUy9MpL413bC4IcantI8Zvc="; })
      (fetchRam 02 { hash = "sha256-sjxEZlWnXzJ5kHCDgaIX5iec2ic/ZnpwlbTtn/d6QMo="; })
      (fetchRam 03 { hash = "sha256-fIiCAFY3TpFLPx7AaI1Gt+dp+roQUeTj/nwbIJcVzvU="; })
      (fetchRam 04 { hash = "sha256-g/PG2dYdrIDzQwbIRZrjjXSZXMBP4ylP5u8X0rs9y2M="; })
      (fetchRam 05 { hash = "sha256-avo8q2rx9mP83hjM2vz6wl7W4inYFvZSYgCbL7+w0a4="; })
      (fetchRam 06 { hash = "sha256-9O4XbA8bg8whURRt1MqOu6AQYu2pJIy+W37QYDoWerc="; })
      (fetchRam 07 { hash = "sha256-lwh7zlhCgxOx5ZQnj8Mhc5SSNpTGzSHsCWE1pOPUy1w="; })
    ];

  in {
    packages.${system} = {
      default = self.pakages.${system}.shloka;
      shloka = pkgs.haskellPackages.callCabal2nix "shloka" ./. {};
      mahabharata = mahabharata;
      ramayana = ramayana;
      mahabharata-csv = pkgs.runCommand "mahabharata.csv" {} ''
        cat ${mahabharata}/* | EPIC=mahabharata ${self.packages.${system}.shloka}/bin/shloka > $out
      '';
      ramayana-csv = pkgs.runCommand "ramayana.csv" {} ''
        cat ${ramayana}/* | EPIC=ramayana ${self.packages.${system}.shloka}/bin/shloka > $out
      '';
    };

    apps.${system} = {
      jupyter = {
        type = "app";
        program = toString (pkgs.writers.writeDash "jupyter" ''
          PATH=${nixpkgs.lib.makeBinPath [pythonInstallation]} \
          EPIC_CSV=${self.packages.${system}.mahabharata-csv} \
          jupyter notebook
        '');
      };
      generate-assets = {
        type = "app";
        program = toString (pkgs.writers.writeDash "generate-assets" ''
          PATH=${nixpkgs.lib.makeBinPath [pythonInstallation]} \
          EPIC_CSV=${self.packages.${system}.mahabharata-csv} \
          papermill ${./statistics.ipynb} /dev/null
        '');
      };
    };

    devShells.${system}.default = with pkgs; haskellPackages.developPackage {
      root = ./.;
      modifier = drv: haskell.lib.addBuildTools drv (with haskellPackages; [
        cabal-install
        ghcid
        (hoogleWithPackages (_: drv.propagatedBuildInputs))
        fourmolu
        pythonInstallation
      ]);
    };
  };
}
