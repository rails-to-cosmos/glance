.PHONY: test native elm elm-test browser browser-path browser-check sync-renderer run run-native run-wasm wasm-spike check-glue mutate mutate-list mutate-clean

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
# The Elm programs, compiled from `assets/elm' and committed like the renderer,
# so the bytes a build embeds are the bytes in the tree.  Ephemeral `npx', the
# same shape as `check-glue' -- nothing installed, no lockfile, no node_modules.
elm:
	@if command -v npx >/dev/null 2>&1; then \
	  cd assets/elm && npx --yes elm make src/Listing.elm src/Doc.elm --optimize --output=../elm.js; \
	else echo "elm: no npx on PATH -- assets/elm.js left as committed"; fi

# THE SCANNER'S OWN TESTS, and they are OUT of `cabal test' on purpose: elm-test
# fetches `elm-explorations/test' at run time, and the Haskell suite must stay
# offline.  That suite is still the contract -- this one asks the pure half of
# the document pane the questions that would otherwise cost a booted page each.
elm-test:
	@if command -v npx >/dev/null 2>&1; then \
	  cd assets/elm && npx --yes -p elm -p elm-test elm-test; \
	else echo "elm-test: no npx on PATH -- skipped"; fi

# THE SUITE'S OWN GRADE (docs/proposal-mutation-runner.done.md).  One rewrite per
# mutant over one file, in a git worktree with its own --builddir at -O0; a
# mutant the suite leaves green names an assertion nobody wrote.  OUT of
# `cabal test' for `elm-test's reason one size up: a check whose unit is MINUTES
# lives behind its own target.  ~30 s a Haskell mutant, ~3 s an Elm one, so a
# 40-mutant sitting is ~20 min.
#
# ONE TARGET PER INVOCATION -- the cold build is paid once, so a target is a
# sitting.  SAMPLE=0 takes every site; SAMPLE=N draws N seeded by the target's
# own blob digest, so an unchanged file repeats its mutants and an EDITED one
# draws a different set.  It reads the COMMITTED revision (REV=, default HEAD)
# and never writes the working tree.
#
#   make mutate TARGET=src/Data/Org/Edit.hs
#   make mutate TARGET=assets/elm/src/Scan.elm SAMPLE=0
#   make mutate-list TARGET=src-web/Glance/Web/Filter.hs
mutate:
	@tools/mutate TARGET=$(TARGET) $(if $(SAMPLE),SAMPLE=$(SAMPLE),) $(if $(REV),REV=$(REV),) $(if $(KEEP),KEEP=$(KEEP),)

# The sites alone -- no worktree kept, no build, no suite.  What a target costs
# before spending the sitting on it.
mutate-list:
	@tools/mutate TARGET=$(TARGET) LIST=1 $(if $(SAMPLE),SAMPLE=$(SAMPLE),)

# `KEEP=1' leaves the worktree and its build dir warm for the next sitting;
# this is how they go.
mutate-clean:
	@rm -rf $(if $(SCRATCH),$(SCRATCH),$${TMPDIR:-/tmp}/glance-mutate)
	@git worktree prune
	@echo "mutate-clean: scratch removed"

# THE BROWSER THIS PROJECT MEASURES GEOMETRY WITH.  Installed by playwright as
# a pure DOWNLOADER -- no import, no `node_modules', no lockfile, the same
# ephemeral `npx --yes' the elm and tsc targets use -- into ~/.cache/ms-playwright,
# outside the repo.  NO ROOT: this machine packages no chromium and the one
# browser it has (firefox) would not start a remote agent in three tries.
# Idempotent: playwright skips a version it already holds.
browser:
	@if command -v npx >/dev/null 2>&1; then \
	  npx --yes playwright@1.62.1 install chromium; \
	  echo "browser: $$($(MAKE) -s browser-path)"; \
	else echo "browser: no npx on PATH -- nothing installed"; fi

# Where it landed, for a driver to exec.  The headless shell where there is one,
# the full browser otherwise; empty and silent when neither is installed, so a
# caller can test for it rather than parse an error.
browser-path:
	@find $(HOME)/.cache/ms-playwright -type f \
	     \( -name headless_shell -o -name chrome \) 2>/dev/null | head -1

# THE ONE CHECK THAT MEASURES A PIXEL (docs/proposal-browser-driver.done.md), and it
# is OUT of `cabal test' for `elm-test's reason one size up: it drives a 150 MB
# browser, spawns a daemon, writes a temp tree and needs the machine's fonts.
# The Haskell suite stays offline and stays the contract; every geometry rule it
# asserts is asserted as CSS SOURCE TEXT, and where a declaration LANDS is what
# this target reads.
#
# IT SKIPS LOUDLY, the idiom `elm-test' and `bootedPage' already use: a check
# that passes having asserted nothing is the failure mode this repo names, so a
# machine with no node and no browser says which and exits 0.
#
#   make browser-check                    every case
#   make browser-check ONLY=flag          the cases whose name carries it
#   make browser-check BREAK=edit-covers  one rule taken out of the page, to
#                                         WATCH the case for it go red
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
#
# ITS OWN BUILD DIR, which is what makes the promise below true: both project
# files name the same package, so without this they write ONE binary and each
# `make' overwrites the other's -- a native window then serves whatever glue
# the last build embedded, which is exactly "native differs from web".
NATIVE_BUILD = --project-file=cabal.project.native --builddir=dist-newstyle-native

native:
	HASKELL_GI_GIR_SEARCH_PATH=$(CURDIR)/vendored/gir \
	  cabal build $(NATIVE_BUILD) all

# The WASM spike (docs/proposal-native-ports.draft.md, host 4): the core compiled by
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

# The same daemon inside its own WebKitGTK window: the flagged build, in its own
# project file AND its own build dir, so `make run-native' never rebuilds the
# unflagged binary out from under a running `make run' and the two can never
# serve different pages from one path.
run-native:
	HASKELL_GI_GIR_SEARCH_PATH=$(CURDIR)/vendored/gir \
	  cabal run $(NATIVE_BUILD) glance -- \
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

# The shell's own checker, table-view's discipline over assets/glue/*.js
# (docs/proposal-glue-extraction.done.md): tsc --checkJs under assets/jsconfig.json.
# Zero errors is the standing state; a finding here is a finding.
check-glue:
	@if command -v npx >/dev/null 2>&1; then \
	  npx --yes -p typescript tsc -p assets/jsconfig.json --pretty false && \
	    echo "check-glue: clean"; \
	else echo "check-glue: no npx on PATH -- skipped"; fi
