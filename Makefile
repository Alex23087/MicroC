.DEFAULT_GOAL := all
EXE=microcc

ARGS := $(wordlist 2,$(words $(MAKECMDGOALS)),$(MAKECMDGOALS))
$(eval $(ARGS):;@:)

.PHONY: all
all: 
	opam exec -- dune build --root .

.PHONY: deps
deps: ## Install development dependencies
	opam install -y dune ocamlformat utop ocaml-lsp-server
	opam install --deps-only --with-test --with-doc -y .

.PHONY: build
build: ## Build the project, including non installable libraries and executables
	opam exec -- dune build --root .

.PHONY: start
start: all ## Run the produced executable
	opam exec -- dune exec --root . bin/$(EXE).exe $(ARGS)
	 
.PHONY: clean-comp
clean-comp: ## Clean microc compilation byproducts
	rm a.bc output.bc output.o rt-support.bc

.PHONY: clean
clean: ## Clean build artifacts and other generated files
	opam exec -- dune clean --root .

.PHONY: doc
doc: ## Generate odoc documentation
	opam exec -- dune build --root . @doc

.PHONY: servedoc
servedoc: doc ## Open odoc documentation with default web browser
	open _build/default/_doc/_html/index.html

.PHONY: fmt
fmt: ## Format the codebase with ocamlformat
	opam exec -- dune build --root . --auto-promote @fmt

.PHONY: watch
watch: ## Watch for the filesystem and rebuild on every change
	opam exec -- dune build --root . --watch

.PHONY: utop
utop: ## Run a REPL and link with the project's libraries
	opam exec -- dune utop --root . lib -- -implicit-bindings

.PHONY: compile
compile: start ## Compile executable and link runtime libraries
	/usr/lib/llvm14/bin/clang -emit-llvm -c "bin/rt-support.c"
	llvm-link rt-support.bc a.bc -o output.bc
	llc -filetype=obj output.bc
	/usr/lib/llvm14/bin/clang output.o -o a.out
	make clean-comp

.PHONY: run
run: compile ## Compile, link and run program
	./a.out
	rm a.out
