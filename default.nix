{}: (import ./focus {}).mkDerivation {
  name = "theProjectName";
  version = "0.1";
  commonDepends = p: with p; [
    data-default
    file-embed
    obelisk-haskpress-frontend
  ];
  frontendDepends = p: with p; [
    data-default
    file-embed
    obelisk-asset-th
    focus-js
    ghcjs-dom
    reflex
    reflex-dom
    safe
    these
    jsaddle
    font-awesome-type
    obelisk-haskpress-frontend
  ];
  backendDepends = p: with p; [
    data-default
    resource-pool
    snap
    snap-core
    snap-loader-static
    snap-server
    obelisk-asset-serve
    obelisk-haskpress-backend
  ];
}
