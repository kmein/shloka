{ pkgs ? import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/d2caa9377539e3b5ff1272ac3aa2d15f3081069f.tar.gz") {} }:
with pkgs;
haskellPackages.developPackage {
  root = ./.;
  modifier = drv: haskell.lib.addBuildTools drv (with haskellPackages; [
    cabal-install
    ghcid
    (hoogleLocal { packages = drv.propagatedBuildInputs; })
    fourmolu
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
}
