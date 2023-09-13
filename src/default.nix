let
  pkgs = import (builtins.fetchGit {
    # Descriptive name to make the store path easier to identify
    name = "R-with-stan";
    url = "https://github.com/nixos/nixpkgs/";
    ref = "refs/heads/nixos-22.05";
    rev = "47edaa313fc3767ce3026037a5b62352f22f3602";
  }) { };

  rethinking = (import ./rethinking.nix pkgs).rethinking;
  modDesignr = (import ./designr/designr-local.nix pkgs).designr;

in let
  myR = pkgs.rWrapper.override {
    packages = with pkgs.rPackages; [
      MASS
      bayesplot
      brms
      cowplot
      here
      knitr
      lme4
      modDesignr
      rethinking
      rstan
      tidyverse
      xtable
    ];
  };

in pkgs.mkShell {
  nativeBuildInputs = [ myR pkgs.gcc ];
  shellHook = ''
    [ -e "$HOME"/.R/Makevars ] && mv -v "$HOME"/.R/Makevars{,_backup}
    trap ./shellExitHook.sh EXIT
  ''; # this version of brms didn't work with Makevars

}
