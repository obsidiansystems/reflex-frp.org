(import ./obelisk {}).project ({ pkgs, ... }: {
  packages = {
    common = ./common;
    backend = ./backend;
    frontend = ./frontend;
    obelisk-asset-th = ./obelisk/obelisk-asset/th;
    obelisk-font-awesome = ./obelisk/obelisk-font-awesome;
    obelisk-haskpress-frontend = ./obelisk/obelisk-haskpress/obelisk-haskpress-frontend;
    obelisk-haskpress-backend = ./obelisk/obelisk-haskpress/obelisk-haskpress-backend;
    obelisk-asset-serve = ./obelisk/obelisk-asset/serve; 
    obelisk-snap = ./obelisk/obelisk-snap;
  };

  overrides = self: super: with pkgs.haskell.lib; {
    common = dontHaddock super.common;
    heist = doJailbreak super.heist;
  };

  shells = {
    ghc = ["common" "backend" "frontend"];
    ghcjs = ["common" "frontend"];
  };
})
