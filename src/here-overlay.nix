self: super:

{
  rPackages = super.rPackages.override {
    overrides = {

      rprojroot = super.rPackages.buildRPackage rec {
        name = "rprojroot-${version}";
        version = "2.0.2";
        src = super.fetchurl {
          url =
            "https://github.com/r-lib/rprojroot/archive/refs/tags/v2.0.2.tar.gz";
          sha256 = "1i0s1f7hla91yw1fdx0rn7c18dp6jwmg2mlww8dix1kk7qbxfjww";
        };
        nativeBuildInputs = [ super.R ];
      };

      here = super.rPackages.buildRPackage rec {
        name = "here-${version}";
        version = "1.0.1";
        src = super.fetchurl {
          url = "https://github.com/r-lib/here/archive/refs/tags/v1.0.1.tar.gz";
          sha256 = "0ky6sq6n8px3b70s10hy99sccf3vcjjpdhamql5dr7i9igsf8nqy";
        };
        nativeBuildInputs = [ super.R self.rPackages.rprojroot ];
        propagatedBuildInputs = [ self.rPackages.rprojroot ];
      };
    };
  };
}
