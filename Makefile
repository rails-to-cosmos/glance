.PHONY: test typecheck loc major minor patch native elm elm-test browser browser-path browser-check interop sync-renderer run run-native run-wasm wasm-spike check-glue mutate mutate-list mutate-clean

-include .env
GLANCE_DIR ?= ~/sync/views
GLANCE_PORT ?= 7777

# THE GATE; `browser-check', `interop' and `mutate' are each their own sitting.
# The exe is built by name because `cabal test' does not, and the cases that
# probe the CLI would otherwise measure whatever binary the tree still holds.
test:
	cabal build -v0 exe:glance
	GLANCE_BIN="$$(cabal list-bin -v0 exe:glance)" cabal test
	@$(MAKE) --no-print-directory elm-test

# Elm's compiler IS its typechecker.  `--output=/dev/null' because committed
# `assets/elm.js' is a build INPUT and only `make elm' may rewrite it.
typecheck:
	cabal build all
	@$(MAKE) --no-print-directory check-glue
	@if command -v npx >/dev/null 2>&1; then \
	  cd frontend/elm && npx --yes elm make src/Listing.elm src/Doc.elm \
	    --output=/dev/null && echo "typecheck: elm clean"; \
	else echo "typecheck: no npx on PATH -- elm skipped"; fi

loc:
	@tools/loc

# A CUT MOVES EVERY SITE TOGETHER (`AGENTS.hs' versionSites) and promotes the
# changelog's Unreleased section.  PVP: `major' is a breaking change and moves
# A.B, `minor' moves C, `patch' moves D.  Nothing is committed or tagged.
major minor patch:
	@tools/cut $@

RENDERER := ../table-view/web/table-view.js
# Committed like the renderer, so the bytes a build embeds are the bytes in the tree.
elm:
	@if command -v npx >/dev/null 2>&1; then \
	  cd frontend/elm && npx --yes elm make src/Listing.elm src/Doc.elm --optimize --output=../../assets/elm.js; \
	else echo "elm: no npx on PATH -- assets/elm.js left as committed"; fi

# OUT of `cabal test': elm-test fetches its dependency at run time.
elm-test:
	@if command -v npx >/dev/null 2>&1; then \
	  cd frontend/elm && npx --yes -p elm -p elm-test elm-test; \
	else echo "elm-test: no npx on PATH -- skipped"; fi

# SAMPLE=0 takes every site; SAMPLE=N is seeded by the target's own blob digest.
#   make mutate TARGET=src/Data/Org/Edit.hs SAMPLE=0 REV=HEAD
mutate:
	@tools/mutate TARGET=$(TARGET) $(if $(SAMPLE),SAMPLE=$(SAMPLE),) $(if $(REV),REV=$(REV),) $(if $(KEEP),KEEP=$(KEEP),)

mutate-list:
	@tools/mutate TARGET=$(TARGET) LIST=1 $(if $(SAMPLE),SAMPLE=$(SAMPLE),)

mutate-clean:
	@rm -rf $(if $(SCRATCH),$(SCRATCH),$${TMPDIR:-/tmp}/glance-mutate)
	@git worktree prune
	@echo "mutate-clean: scratch removed"

browser:
	@if command -v npx >/dev/null 2>&1; then \
	  npx --yes playwright@1.62.1 install chromium; \
	  echo "browser: $$($(MAKE) -s browser-path)"; \
	else echo "browser: no npx on PATH -- nothing installed"; fi

browser-path:
	@find $(HOME)/.cache/ms-playwright -type f \
	     \( -name headless_shell -o -name chrome \) 2>/dev/null | head -1

# IT SKIPS LOUDLY, and takes ONLY=<name> and BREAK=<rule>.
browser-check:
	@command -v node >/dev/null 2>&1 \
	  || { echo "browser-check: no node on PATH -- SKIPPED"; exit 0; }
	@bin="$$($(MAKE) -s browser-path)"; \
	if [ -z "$$bin" ]; then \
	  echo "browser-check: no browser under ~/.cache/ms-playwright -- SKIPPED (run \`make browser')"; \
	  exit 0; \
	fi; \
	cabal build -v0 exe:glance && \
	CHROME="$$bin" GLANCE_BIN="$$(cabal list-bin -v0 exe:glance)" \
	  ONLY="$(ONLY)" BREAK="$(BREAK)" KEEP="$(KEEP)" node test/browser/drive.mjs

OG_HOME ?= $(CURDIR)/../org-glance
OG_EMACS_VERSION ?= 29.1

# ONE RECIPE LINE: make gives each line its own shell, and a skip must end the TARGET.
interop:
	@command -v node >/dev/null 2>&1 \
	  || { echo "interop: no node on PATH -- SKIPPED"; exit 0; }; \
	test -d "$(OG_HOME)/src/data" \
	  || { echo "interop: no org-glance checkout at $(OG_HOME) -- SKIPPED (set OG_HOME=)"; exit 0; }; \
	ls -d $(OG_HOME)/.eask/*/elpa >/dev/null 2>&1 \
	  || { echo "interop: no dependencies under $(OG_HOME)/.eask -- SKIPPED (run \`eask install-deps' there)"; exit 0; }; \
	if [ "$(EMACS_RUN)" = "podman" ]; then \
	  command -v podman >/dev/null 2>&1 \
	    || { echo "interop: no podman on PATH -- SKIPPED"; exit 0; }; \
	  $(MAKE) -C "$(OG_HOME)" podman-build EMACS_VERSION=$(OG_EMACS_VERSION) \
	    || { echo "interop: org-glance's own podman-build failed -- its image, its target"; exit 1; }; \
	else \
	  command -v emacs >/dev/null 2>&1 \
	    || { echo "interop: no emacs on PATH -- SKIPPED"; exit 0; }; \
	fi; \
	cabal build -v0 exe:glance && \
	GLANCE_BIN="$$(cabal list-bin -v0 exe:glance)" \
	  OG_HOME="$(OG_HOME)" EMACS_RUN="$(EMACS_RUN)" \
	  OG_IMAGE="org-glance-test:emacs-$(OG_EMACS_VERSION)" \
	  BREAK="$(BREAK)" KEEP="$(KEEP)" node test/interop/drive.mjs

# `git diff --no-index' decides and reports at once: non-zero when the two differ.
sync-renderer:
	@if [ ! -f "$(RENDERER)" ]; then \
	  echo "sync-renderer: no sibling checkout at $(RENDERER) -- nothing copied"; \
	elif git diff --stat --no-index -- assets/table-view.js "$(RENDERER)"; then \
	  echo "sync-renderer: assets/table-view.js is already current"; \
	else \
	  cp "$(RENDERER)" assets/table-view.js; \
	  echo "sync-renderer: copied $(RENDERER) -> assets/table-view.js"; \
	fi

# ITS OWN BUILD DIR: both project files name the same package, so without it each
# `make' overwrites the other's binary and a window serves the last build's glue.
NATIVE_BUILD = --project-file=cabal.project.native --builddir=dist-newstyle-native

native:
	HASKELL_GI_GIR_SEARCH_PATH=$(CURDIR)/vendored/gir \
	  cabal build $(NATIVE_BUILD) all

wasm-spike:
	@if [ ! -x "$$HOME/.ghc-wasm/wasm32-wasi-ghc/bin/wasm32-wasi-ghc" ]; then \
	  echo "wasm-spike: no toolchain at ~/.ghc-wasm -- run ghc-wasm-meta's bootstrap first"; \
	else \
	  . "$$HOME/.ghc-wasm/env" && \
	  wasm32-wasi-cabal build --project-file=cabal.project.wasm glance-internal lib:glance; \
	fi

run:
	cabal run glance -- desktop --dir $(GLANCE_DIR) --port $(GLANCE_PORT)

run-native:
	HASKELL_GI_GIR_SEARCH_PATH=$(CURDIR)/vendored/gir \
	  cabal run $(NATIVE_BUILD) glance -- \
	    desktop --dir $(GLANCE_DIR) --port $(GLANCE_PORT)

run-wasm:
	@if [ ! -x "$$HOME/.ghc-wasm/wasm32-wasi-ghc/bin/wasm32-wasi-ghc" ]; then \
	  echo "run-wasm: no toolchain at ~/.ghc-wasm -- run ghc-wasm-meta's bootstrap first"; \
	else \
	  . "$$HOME/.ghc-wasm/env" && \
	  wasm32-wasi-cabal build --project-file=cabal.project.wasm glance-wasm-probe && \
	  wasmtime run --dir $(GLANCE_DIR)::/tree \
	    "$$(. $$HOME/.ghc-wasm/env && wasm32-wasi-cabal list-bin --project-file=cabal.project.wasm glance-wasm-probe)" /tree; \
	fi

check-glue:
	@if command -v npx >/dev/null 2>&1; then \
	  npx --yes -p typescript tsc -p frontend/jsconfig.json --pretty false && \
	    echo "check-glue: clean"; \
	else echo "check-glue: no npx on PATH -- skipped"; fi
