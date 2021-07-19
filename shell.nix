let
  pkgs = (import <nixpkgs> {});
  lib = pkgs.haskell.lib;
  hp = pkgs.haskellPackages.extend (pkgs.lib.composeExtensions 
    (lib.packageSourceOverrides {
       brittany = /home/haveo/build/brittany;
     })
    (self: super: {
       brittany = lib.dontCheck super.brittany;
     }));
in hp.developPackage {
  root = ./.;
  modifier = drv:
    let
      addHook = d: lib.overrideCabal d (_: {
        shellHook = # TODO: fix race conditions
          ''
            daemon --name=hoogle -- hoogle server --local
            function finish {
              daemon -n hoogle --signal SIGTERM;
            }
            trap finish EXIT
          '';
      });
      addTools = d:
        lib.addBuildTools d
          (with hp; [cabal-install ghcid haskell-language-server brittany pkgs.daemon]);
    in addTools (addHook drv);
}
