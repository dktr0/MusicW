{ reflex-commit ? "123a6f487ca954fd983f6d4cd6b2a69d4c463d10" }:

let reflex-platform = builtins.fetchTarball "https://github.com/reflex-frp/reflex-platform/archive/${reflex-commit}.tar.gz"; in

(import reflex-platform {}).project ({ pkgs, ... }:

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
