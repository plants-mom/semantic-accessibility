self: super: {
  rWrapper2 = super.callPackage ./r-wrapper2.nix {
    # Copied from the original rWrapper in all-packages.nix
    # If you define this as rWrapper2, you can just use
    inherit (self.rWrapper) recommendedPackages;
    # recommendedPackages = with super.rPackages; [
    #   boot class cluster codetools foreign KernSmooth lattice MASS
    #   Matrix mgcv nlme nnet rpart spatial survival
    # ];
    # Override this attribute to register additional libraries.
    packages = [ ];
  };
}
