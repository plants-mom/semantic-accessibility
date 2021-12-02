let
  pkgs = import (builtins.fetchGit {
    # Descriptive name to make the store path easier to my_identify
    name = "my-R-402-revision";
    url = "https://github.com/NixOS/nixpkgs/";
    ref = "refs/heads/nixpkgs-unstable";
    rev = "2c162d49cd5b979eb66ff1653aecaeaa01690fcc";
  }) {
    # overlays = [ (import ./rWrapper2.nix) (import ./here-overlay-old.nix) ];
    overlays = [ (import ./rWrapper2.nix) (import ./here-overlay.nix) ];
  };

  rethinking = (import ./rethinking.nix pkgs).rethinking;
  modDesignr = (import ./designr/designr-local.nix pkgs).designr;

in pkgs.rWrapper2.override {
  packages = with pkgs.rPackages; [
    brms
    dplyr
    here
    modDesignr
    readr
    rethinking
    tidyr
    bridgesampling
    fs
  ];
}
