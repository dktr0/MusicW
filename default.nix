{ reflex-commit ? "716879f16d53c93766e7ed9af17416fccb2edfe1" }:

let reflex-platform = builtins.fetchTarball "https://github.com/reflex-frp/reflex-platform/archive/${reflex-commit}.tar.gz"; in

(import reflex-platform {}).project ({ pkgs, ghc8_4, ... }:

with pkgs.haskell.lib;

{

  name = "musicw";

  packages = {
    musicw = ./.;
  };

  shells = {
    ghcjs = ["musicw"];
  };

  # android = {};

  overrides = self: super: {
    #       lens = self.callHackage "lens" "4.15.4" {}; # saving this example in case we need it later

    base-compat-batteries = dontCheck super.base-compat-batteries;
    text-show = dontCheck super.text-show;

  };

})
