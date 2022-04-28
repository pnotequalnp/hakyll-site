{
  inputs = {
    nixpkgs.url = "nixpkgs/master";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        overlay = final: prev: {
          haskell = prev.haskell // {
            packageOverrides =
              final.lib.composeExtensions prev.haskell.packageOverrides
              (hsFinal: hsPrev: {
                hakyll-site = hsFinal.callCabal2nix "hakyll-site" ./. { };
                posts = hsFinal.callCabal2nix "posts" ./posts { };
              });
          };
        };
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ overlay ];
        };
        inherit (pkgs.lib) optional;
        inherit (pkgs.haskell.packages) ghc8107 ghc921;
        tools = with ghc921; [ cabal-install fourmolu hlint pkgs.nixfmt ];
        devShell = f: lsp: hs:
          hs.shellFor {
            packages = f;
            nativeBuildInputs = tools
              ++ optional lsp [ hs.haskell-language-server ];
          };
        site = p: [ p.hakyll-site ];
        posts = p: [ p.posts ];
        static = hs:
          pkgs.stdenv.mkDerivation {
            name = "static";
            buildInputs = [ hs.hakyll-site ];
            src = ./.;
            LANG = "en_US.UTF-8";
            LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
            buildPhase = ''
              hakyll-site build --verbose
            '';
            installPhase = ''
              mkdir -p "$out/dist"
              cp -r _site/* "$out/dist"
            '';
          };
      in {
        apps.default = flake-utils.lib.mkApp { drv = ghc8107.hakyll-site; };

        packages = {
          hakyll-site = ghc8107.hakyll-site;
          posts = ghc921.posts;
          default = static ghc8107;
        };

        devShells = {
          default = devShell site true ghc8107;
          posts = devShell posts true ghc921;
        };
      });
}
