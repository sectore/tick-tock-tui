{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = inputs @ {
    self,
    nixpkgs,
    flake-parts,
    ...
  }:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [inputs.haskell-flake.flakeModule];

      perSystem = {
        self',
        pkgs,
        ...
      }: {
        # Typically, you just want a single project named "default". But
        # multiple projects are also possible, each using different GHC version.
        haskellProjects.default = {
          # The base package set representing a specific GHC version.
          # By default, this is pkgs.haskellPackages.
          # You may also create your own. See https://community.flake.parts/haskell-flake/package-set
          basePackages = pkgs.haskell.packages.ghc984;

          # Extra package information. See https://community.flake.parts/haskell-flake/dependency
          #
          # Note that local packages are automatically included in `packages`
          # (defined by `defaults.packages` option).
          #
          # packages = {
          #   hlint.source = "3.8";
          #   # latest hls 2.10
          #   # @see https://github.com/haskell/haskell-language-server/releases/tag/2.10.0.0
          #   haskell-language-server.source = pkgs.fetchFromGitHub {
          #     owner = "haskell";
          #     repo = "haskell-language-server";
          #     rev = "2318c61db3a01e03700bd4b05665662929b7fe8b";
          #     hash = "sha256-q4kDFyJDDeoGqfEtrZRx4iqMVEC2MOzCToWsFY+TOzY=";
          #   };
          # };
          settings = {
            #  aeson = {
            #    check = false;
            #  };
            #  relude = {
            #    haddock = false;
            #    broken = false;
            #  };
          };

          devShell = {
            # Enabled by default
            enable = true;

            # Programs you want to make available in the shell.
            # Default programs can be disabled by setting to 'null'
            # tools = hp: { fourmolu = hp.fourmolu; ghcid = null; };
            tools = hp: {
              inherit
                (pkgs)
                just
                zlib
                pkg-config
                ;
              inherit
                (hp)
                # Note:
                # cabal-install, haskell-language-server, ghcid, hlint
                # are all installed by `haskell-flake` by default:
                # @see https://github.com/srid/haskell-flake/blob/master/nix/modules/project/defaults.nix#L25-L28
                ghcide
                cabal-fmt
                cabal-gild
                fourmolu
                ;
            };
            # Check that haskell-language-server works
            # hlsCheck.enable = true; # Requires sandbox to be disabled
          };
        };

        # haskell-flake doesn't set the default package, but you can do it here.
        packages.default = self'.packages.example;
      };
    };
}
