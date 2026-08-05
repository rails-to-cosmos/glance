.PHONY: test native sync-renderer run run-native run-wasm wasm-spike check-glue

# The run targets' knobs: .env carries them (committed, edit to taste), and
# the ?= pair means a missing .env still runs against the defaults.
-include .env
GLANCE_DIR ?= ~/sync/views
GLANCE_PORT ?= 7777

test:
	cabal test

# The renderer is vendored at assets/table-view.js and compiled into the binary,
# so a `glance' started anywhere serves the same page and no path off this repo
# is ever read.  Hacking the renderer happens in the sibling table-view
# checkout, with `--assets ../table-view/web' serving it off disk; this target
# ends that loop by copying the result back, and the copy is committed like any
# other file.  Without the sibling checkout it says so and changes nothing --
# a clone of this repo alone still builds.
#
# `git diff --no-index' answers both halves at once: it prints the summary and
# exits non-zero when the two differ, and prints nothing and exits 0 when they
# do not, so one command decides whether to copy and says what moved.
RENDERER := ../table-view/web/table-view.js
sync-renderer:
	@if [ ! -f "$(RENDERER)" ]; then \
	  echo "sync-renderer: no sibling checkout at $(RENDERER) -- nothing copied"; \
	elif git diff --stat --no-index -- assets/table-view.js "$(RENDERER)"; then \
	  echo "sync-renderer: assets/table-view.js is already current"; \
	else \
	  cp "$(RENDERER)" assets/table-view.js; \
	  echo "sync-renderer: copied $(RENDERER) -> assets/table-view.js"; \
	fi

# The native window (`-f native-window'), which needs three things the default
# build does not: the flag, the WebKit2 4.1 bindings under vendored/, and the
# hand-written GIR files that live in Arch's gobject-introspection package --
# cairo, xlib and freetype2, which gobject-introspection-runtime alone does not
# carry and which every GTK binding here reads at configure time.  The first two
# are cabal.project.native's; the third is this variable, and haskell-gi
# PREPENDS it, so a machine that has the system copies still uses them.
native:
	HASKELL_GI_GIR_SEARCH_PATH=$(CURDIR)/vendored/gir \
	  cabal build --project-file=cabal.project.native all

# The WASM spike (docs/proposal-native-ports.md, host 4): the core compiled by
# the ghc-wasm-meta toolchain, glance-internal alone -- the deliverable is the
# CATALOG of what compiles, not a working module yet.  Needs ~/.ghc-wasm on the
# machine (the bootstrap script installs it); says so when it is not.
wasm-spike:
	@if [ ! -x "$$HOME/.ghc-wasm/wasm32-wasi-ghc/bin/wasm32-wasi-ghc" ]; then \
	  echo "wasm-spike: no toolchain at ~/.ghc-wasm -- run ghc-wasm-meta's bootstrap first"; \
	else \
	  . "$$HOME/.ghc-wasm/env" && \
	  wasm32-wasi-cabal build --project-file=cabal.project.wasm glance-internal lib:glance; \
	fi

# Serve GLANCE_DIR in the ordinary browser flow.
run:
	cabal run glance -- desktop --dir $(GLANCE_DIR) --port $(GLANCE_PORT)

# The same daemon inside its own WebKitGTK window: the flagged build, run
# through its own project file so `make run-native' never rebuilds the
# unflagged binary out from under a running `make run'.
run-native:
	HASKELL_GI_GIR_SEARCH_PATH=$(CURDIR)/vendored/gir \
	  cabal run --project-file=cabal.project.native glance -- \
	    desktop --dir $(GLANCE_DIR) --port $(GLANCE_PORT)

# The WASM probe over GLANCE_DIR: the core running INSIDE wasmtime -- walk,
# parse, count -- which is the daemon-in-the-page milestone's engine half
# proven on a real tree.  The tree is preopened read-only at /tree; WASI sees
# nothing else.
run-wasm:
	@if [ ! -x "$$HOME/.ghc-wasm/wasm32-wasi-ghc/bin/wasm32-wasi-ghc" ]; then \
	  echo "run-wasm: no toolchain at ~/.ghc-wasm -- run ghc-wasm-meta's bootstrap first"; \
	else \
	  . "$$HOME/.ghc-wasm/env" && \
	  wasm32-wasi-cabal build --project-file=cabal.project.wasm glance-wasm-probe && \
	  wasmtime run --dir $(GLANCE_DIR)::/tree \
	    "$$(. $$HOME/.ghc-wasm/env && wasm32-wasi-cabal list-bin --project-file=cabal.project.wasm glance-wasm-probe)" /tree; \
	fi

# The shell's own checker, table-view's discipline over assets/glue.js
# (docs/proposal-glue-extraction.md): tsc --checkJs under assets/jsconfig.json.
# Zero errors is the standing state; a finding here is a finding.
check-glue:
	@if command -v npx >/dev/null 2>&1; then \
	  npx --yes -p typescript tsc -p assets/jsconfig.json --pretty false && \
	    echo "check-glue: clean"; \
	else echo "check-glue: no npx on PATH -- skipped"; fi
