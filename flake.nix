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
        devShell = lsp: hs:
          hs.shellFor {
            packages = hsPkgs: with hsPkgs; [ hakyll-site ];
            nativeBuildInputs = tools
              ++ optional lsp [ hs.haskell-language-server ];
          };
        hostname = "hakyll.pnotequalnp.com";
        static = hs:
          pkgs.stdenv.mkDerivation {
            name = "static";
            buildInputs = [ hs.hakyll-site ];
            src = ./.;
            LANG = "en_US.UTF-8";
            LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
            buildPhase = ''
              hakyll-site build --verbose
              echo '${hostname}' > _site/CNAME
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
          default = static ghc8107;
        };

        devShells = {
          default = devShell true ghc8107;
          ci = devShell false ghc8107;
        };
      });
}
