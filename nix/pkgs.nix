let
  nixpkgs-src = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/0d1b9472176bb31fa1f9a7b86ccbb20c656e6792.tar.gz"; # haskell-updates 23/03/23
    sha256 = "sha256:0j7y0s691xjs2146pkssz5wd3dc5qkvzx106m911anvzd08dbx9f";
  };

  config = {
    packageOverrides = pkgs: with pkgs.haskell.lib;
      let
        # https://github.com/NixOS/nixpkgs/issues/140774#issuecomment-1371565125
        # https://github.com/NixOS/nixpkgs/issues/220647
        fixCyclicReference = drv: overrideCabal drv (_: { enableSeparateBinOutput = false; });
      in {
        haskell = pkgs.haskell // {
          packages = pkgs.haskell.packages // {
            ghc94 = pkgs.haskell.packages.ghc94.override(old: {
              overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: {})) (self: super: {
                ghcid = fixCyclicReference(dontCheck super.ghcid); # some tests are non-reproducible from measuring time
                nightfall = self.callCabal2nix "nightfall" ../. {};
              });
            });
          };
        };
    };
  };

  nixpkgs = import nixpkgs-src { inherit config; };

  shell = nixpkgs.haskell.packages.ghc94.shellFor {
    strictDeps = true;
    packages = p: [ p.nightfall ];
    withHoogle = true;
    nativeBuildInputs =
      let hask = with nixpkgs.haskell.packages.ghc94; [
        (import ./cabal-multi-repl.nix).cabal-install
        ghcid
        (haskell-language-server.overrideAttrs(finalAttrs: previousAttrs: { propagatedBuildInputs = []; buildInputs = previousAttrs.propagatedBuildInputs; }))
      ];
      in with nixpkgs; hask ++ [
        zlib
        rustc
        miden
      ];
  };

  miden = nixpkgs.rustPlatform.buildRustPackage rec {
    pname = "miden";
    version = "0.5.0";
    src = nixpkgs.fetchFromGitHub {
      owner = "0xPolygonMiden";
      repo = "miden-vm";
      rev = "v${version}";
      sha256 = "sha256-bDILZ8vvxjB/JN6tyhTNKxjQB/35M47grxFqToesH9E=";
    };
    cargoLock.lockFile = ./miden_cargo.lock;
    postPatch = ''
      ln -s ${./miden_cargo.lock} Cargo.lock
    '';
    buildType = "release";
    buildFeatures = [ "executable" "concurrent" ];
    nativeBuildInputs = with nixpkgs; [ rustc ];
    doCheck = false;
  };
in

{ inherit nixpkgs shell miden;
  inherit (nixpkgs.haskell.packages.ghc94) nightfall;
}
