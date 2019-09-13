build:
	rm .ghc.environment*
	nix-build

watch:
	nix-shell --run "ghcid --command 'hpack && cabal v2-repl'"

lint:
	nix-shell --run "hlint"
