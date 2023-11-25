{
  outputs = { self, nixpkgs }: 
  let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
  in with pkgs; {

    devShells.${system}.default = mkShell {

      shellHook = ''
      git status
      export ENV_NAME=haskell 
      '';

      packages = [
        neovim
        (haskellPackages.ghcWithPackages (pkgs: with pkgs; [
          random
          randomgen
          parallel
        ]))
        haskell-language-server
      ];
    };
  };
}
