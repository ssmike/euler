{
  outputs = { self, nixpkgs }: 
  let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
    myghc = with pkgs; (
      haskellPackages.ghcWithPackages (pkgs: with pkgs; [
          random
          randomgen
          parallel
          split
          HUnit
          QuickCheck
        ]));
  in with pkgs; {

    devShells.${system}.default = mkShell {

      shellHook = ''
      git status
      export ENV_NAME=euler-haskell 
      '';

      packages = [
        neovim
        myghc
        haskell-language-server
      ];
    };

    packages.${system}.default = stdenv.mkDerivation {
      name = "euler";

      buildInputs = [
        myghc
      ];

      unpackPhase = ''
        cp ${./.}/*.hs .
      '';

      buildPhase = ''
        for f in *.hs; do
          ghc $f -o $f.run || echo "failed $f"
        done
      '';

      installPhase = ''
        mkdir -p $out/bin
        for f in *.hs.run; do
          cp $f $out/bin/$f;
        done
      '';

      phases = [ "unpackPhase" "buildPhase" "installPhase" ];
    };
  };
}
