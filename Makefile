.PHONY: build-nix hoogle nix-build-library nix-build-executables \
        nix-build-test nix-cabal-repl requires_nix_shell ci-build-run

# Generate TOC for README.md
# It has to be manually inserted into the README.md for now.
generate-readme-contents:
	nix shell nixpkgs#nodePackages.npm --command "npx markdown-toc ./README.md --no-firsth1"

# Starts a hoogle Server.
hoogle:
	@ nix develop -c hoogle server --local --port 8008

# Attempt the CI locally
# TODO

# Build the library with nix.
nix-build-library:
	@ nix build .#plutip:lib:plutip

current-system := $(shell nix eval --impure --expr builtins.currentSystem)

# Build the executables with nix (also builds the test suite).
nix-build-executables:
	@ nix build .#check.${current-system}

# Build the tests with nix.
nix-build-test:
	@ nix build .#plutip:test:plutip-tests

# Starts a ghci repl inside the nix environment.
nix-cabal-repl:
	@ nix develop -c cabal new-repl

# Target to use as dependency to fail if not inside nix-shell.
requires_nix_shell:
	@ [ "$(IN_NIX_SHELL)" ] || echo "The $(MAKECMDGOALS) target must be run from inside a nix shell"
	@ [ "$(IN_NIX_SHELL)" ] || (echo "    run 'nix develop' first" && false)

# Add folder locations to the list to be reformatted.
fourmolu-format:
	@ echo "> Formatting all .hs files"
	fourmolu -o -XTypeApplications -o -XImportQualifiedPost -i $$(find src/  -iregex ".*.hs")
	fourmolu -o -XTypeApplications -o -XImportQualifiedPost -i $$(find test/ -iregex ".*.hs")

NIX_SOURCES := $(shell git ls-tree -r HEAD --full-tree --name-only | grep -E '.*\.nix' )

nixfmt: requires_nix_shell
	nixfmt $(NIX_SOURCES)
