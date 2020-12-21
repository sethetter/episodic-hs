build:
	rm -f .ghc.environment*
	nix-build

watch:
	nix-shell --run "ghcid --command 'hpack && cabal repl'"

lint:
	nix-shell --run "hlint"

repl:
	nix-shell --run "hpack && cabal repl"
