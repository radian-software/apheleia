{

  nixpkgs = {
    config.allowUnfree = true;
    overlays = [      (final: prev: {
        unstable = import self.inputs.nixpkgs-unstable {          system = system;
          config.allowUnfree = true;
        };
      })
               ];
  };

}
