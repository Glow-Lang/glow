dev:
	ghcid --command "cabal repl" --output ghcid-output.txt

dev-contract:
	ghcid --command "cabal repl" --setup ":set args $(SRC)" --test Parser.parseCommand --output ghcid-output.txt

repl:
	cabal repl

run:
	cabal run

hoogle:
	hoogle generate --local && hoogle server -p 8000 --local
