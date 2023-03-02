{
  description = "A very basic flake";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs";

  outputs = { self, nixpkgs }:
  let
    system = "x86_64-linux";
    pkgs = import nixpkgs { inherit system; };
    indic-transliteration = with pkgs.python3Packages; buildPythonPackage rec {
      pname = "indic_transliteration";
      version = "2.3.40";
      format = "wheel";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/fa/0d/d917def2e12fcf906253aca886b1705297a5e723cffd345ed5114ae5dba4/indic_transliteration-2.3.40-py3-none-any.whl";
        hash = "sha256-RHc02GVWBLxEsrPeaGMJ5KXTZ9/8HEm+j1qHB5ZssPw=";
      };
      propagatedBuildInputs = [ regex toml typer roman backports_functools_lru_cache ];
    };
  in {
    devShells.${system}.default = with pkgs; haskellPackages.developPackage {
      root = ./.;
      modifier = drv: haskell.lib.addBuildTools drv (with haskellPackages; [
        cabal-install
        ghcid
        (hoogleLocal { packages = drv.propagatedBuildInputs; })
        fourmolu

        (pkgs.python3.withPackages (p: [
          p.pandas
          p.jupyter
          p.matplotlib
          p.seaborn
          p.tabulate
          p.papermill
          p.scipy
          indic-transliteration
        ]))

        (pkgs.writers.writeDashBin "fetch-mbh" ''
          for book in $(seq 1 18); do
          ${pkgs.wget}/bin/wget "$(printf "https://bombay.indology.info/mahabharata/text/ASCII/MBh%02d.txt" "$book")" -P ${toString ./text}
          done

      # in two instances, the retroflex ḷ = ḍ is used, this conflicts with the vocalic ḷ
            sed -i s/eLako/eDako/g ${toString ./text}/MBh01.txt
            sed -i s/dramiLAz/dramiDAz/g ${toString ./text}/MBh13.txt
        '')
        (pkgs.writers.writeDashBin "fetch-ram" ''
          for book in $(seq 1 7); do
          ${pkgs.wget}/bin/wget "$(printf "https://bombay.indology.info/ramayana/text/ASCII/Ram%02d.txt" "$book")" -P ${toString ./text}
          done
        '')
      ]);
    };
  };
}
