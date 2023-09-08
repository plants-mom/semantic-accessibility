{ pkgs ? import <nixpkgs> { }, ... }: {
  rethinking = with pkgs;
    rPackages.buildRPackage rec {
      version = "2.13";
      name = "rethinking-${version}";
      src = fetchurl {
        url =
          "https://github.com/rmcelreath/rethinking/archive/refs/tags/2.13.tar.gz";
        sha256 = "06jndsc6y89ri1rkblqs6dgz1dbnk3iz72k5j68rpw6fbij4f3p4";
      };
      nativeBuildInputs = with rPackages; [
        R
        rstan
        coda
        MASS
        mvtnorm
        loo
        shape
      ];

        propagatedBuildInputs = with rPackages; [ shape ];
    };
}
