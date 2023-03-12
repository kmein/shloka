# Shloka (श्लोकः)

Analysing the anushtubh verses of India's great epics.

To reproduce the metrical analysis, do the following:

1. Install [Nix](https://nixos.org/download.html#nix-install-linux)
2. Run `nix build .#mahabharata-csv` or `nix build .#ramayana-csv`
3. The results CSV of the analysis can be found in the `result` symlink / file

To reproduce the statistical analysis, do the following:

1. Install [Nix](https://nixos.org/download.html#nix-install-linux).
2. Run `nix run .#jupyter`.
3. Wait for the analysis to build and open the link in your browser of choice.
