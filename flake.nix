{
	description = "Haskell development shell";

	inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

	outputs = {
		self,
		nixpkgs,
	}: let
		systems = ["x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin"];
		forAllSystems = nixpkgs.lib.genAttrs systems;
	in {
		devShells =
			forAllSystems (system: let
					pkgs = import nixpkgs {inherit system;};
				in {
					default =
						pkgs.mkShell {
							packages = with pkgs; [
								ghc
								haskell-language-server
								cabal-install
								hlint
								pkg-config
								zlib
								haskell-language-server
							];

							shellHook = ''
								echo "Haskell shell: $(ghc --numeric-version)"
							'';
						};
				});
	};
}
