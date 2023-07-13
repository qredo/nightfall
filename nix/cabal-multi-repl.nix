let
  nixpkgs-src = builtins.fetchTarball {
    # https://github.com/haskell/cabal/pull/8726 on top of
    # merge of https://github.com/NixOS/nixpkgs/pull/237712 into master
    url = "https://github.com/qrlex/nixpkgs/archive/ee4221cfd9015a768061f1cd155367b47a5af756.tar.gz";
    sha256 = "sha256:1wv9bf71wp2s2xk5yxys7m86ldl9w52mpkihzqkqd5khygkjryxi";
  };

  config = {
    packageOverrides = pkgs: with pkgs.haskell.lib; {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          ghc96 = pkgs.haskell.packages.ghc96.override(old: {
            overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: {})) (self: super: {
              # newer ghc and/or cabal changes the value of `srcLocPackage` in
              # https://github.com/sol/call-stack/blob/1d78c6320f986a2948b2076736b1678e53f862cf/test/Data/CallStackSpec.hs#L16
              call-stack = dontCheck super.call-stack;
            });
          });
        };
      };
    };
  };

  nixpkgs = import nixpkgs-src {inherit config; };

in {
  inherit (nixpkgs.haskell.packages.ghc96) cabal-install;
}
