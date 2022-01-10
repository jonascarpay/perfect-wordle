# This pkgs.nix code differs from the haskell.nix Getting Started section of
# the manual in two ways:
#
# - The manual suggests using `niv` for automatically updating the pinned
#   haskell.nix version. `niv` introduces a fair amount of generated overhead
#   code, though, so it tends to not be worth for a small number of slow-moving
#   dependencies. If you prefer using `niv`, it is a matter of changing the
#   `haskellNix` definition below for `sources.haskellNix`.
#
# - The manual exposes the generated haskell package set `hsPkgs` as the top
#   level of this file. Here, it is instead added back into nixpkgs through an
#   overlay. This allows reusing haskell.nix's pinned nixpkgs version for
#   other, non-Haskell dependencies. The price is that it is less obvious what
#   is happening for users not familiar with overlays.
#
let
  haskellNix =
    let
      # 2021-04-13
      commit = "7421a25fb842e188eff6612f31a766e479f973a5";
      sha256 = "sha256:0879xpcddii5v43r6if298dms8fk0lb433pm2r8mk4k7f2nkjiqc";
    in
    import
      (builtins.fetchTarball {
        url = "https://github.com/input-output-hk/haskell.nix/archive/${commit}.tar.gz";
        inherit sha256;
      })
      { };

  # It might be worth setting this to a more stable channel, but see https://github.com/jonascarpay/template-haskell/issues/9
  pkgsSrc = haskellNix.sources.nixpkgs-unstable;
  pkgsArgs = haskellNix.nixpkgsArgs;

  overlay = self: _: {
    hsPkgs = self.haskell-nix.project {
      src = self.haskell-nix.haskellLib.cleanGit {
        src = ./.;
        name = "perfect-wordle";
      };
      compiler-nix-name = "ghc8107";
    };
  };
in
import pkgsSrc (pkgsArgs // {
  overlays = pkgsArgs.overlays ++ [ overlay ];
})
