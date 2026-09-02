.PHONY: test test-p test-list spec spec-debt typecheck loc major minor patch bootstrap native install release dist dist-wasm elm elm-test browser browser-path browser-check interop sync-renderer run run-native run-wasm wasm-spike check-glue mutate mutate-list mutate-clean

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

# A SUBSET OF THE HASKELL SUITE by tasty pattern, exe built as in `test'; no elm.
# `-p' is an awk-like expr: terms are `/slashed/', combined with `&&' `||' `!'.
#   make test-p    P='/font stack/'
#   make test-p    P='/Serve/ && /log/'
test-p:
	@test -n "$(P)" || { echo "test-p: pass P='/pattern/' (tasty -p expr)"; exit 2; }
	cabal build -v0 exe:glance
	GLANCE_BIN="$$(cabal list-bin -v0 exe:glance)" cabal test --test-options='-p "$(P)"'

# The matching names, run none -- to find a pattern before `test-p'.
test-list:
	@test -n "$(P)" || { echo "test-list: pass P='/pattern/'"; exit 2; }
	cabal build -v0 exe:glance
	GLANCE_BIN="$$(cabal list-bin -v0 exe:glance)" cabal test --test-options='-p "$(P)" --list-tests'

# The spec is AGENTS.hs; this prints the ledger.  The model itself is checked by
# `cabal test' (TestSpec), which reads its registries beside the real code's.
#
# THE DELTA IS GIT'S TO ANSWER and is counted here rather than in the model: both
# sides are a text count of the `Note' literals, HEAD's against the tree's, so the
# two measure one thing.  Outside git it degrades silently -- no repo, no commit,
# the file untracked -- and prints no line.
spec:
	@runghc AGENTS.hs
	@was=$$(git show HEAD:AGENTS.hs 2>/dev/null | grep -c 'Note "'); \
	if [ "$$was" -gt 0 ] 2>/dev/null; then \
	  now=$$(grep -c 'Note "' AGENTS.hs); d=$$((now - was)); \
	  case $$d in \
	    0)  echo "notes ±0 since HEAD" ;; \
	    -*) echo "notes $$d since HEAD" ;; \
	    *)  echo "notes +$$d since HEAD" ;; \
	  esac; \
	fi

# The same binary, tier three spelled out: every unguarded note under its section.
spec-debt:
	@runghc AGENTS.hs debt

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
# ENGINE BY OS: Linux builds the WebKitGTK window from `cabal.project.native'
# (the vendored gi-webkit2 4.1 bindings + the GIR path); macOS builds the
# Cocoa/WKWebView shim, which is just the `native-window' flag on the plain
# project -- no vendored GTK bindings, no GIR, so no project file and no GIR env.
ifeq ($(shell uname -s),Darwin)
NATIVE_BUILD = --builddir=dist-newstyle-native --flags=native-window
NATIVE_ENV =
else
NATIVE_BUILD = --project-file=cabal.project.native --builddir=dist-newstyle-native
NATIVE_ENV = HASKELL_GI_GIR_SEARCH_PATH=$(CURDIR)/vendored/gir
endif

# Cabal build knobs threaded through `native'/`install', EMPTY by default so a
# plain build stays -O1; `release' sets -O2 and split-sections.  `list-bin' takes
# the same OPT, or cabal reads a different config and reports the wrong binary.
# STRIP is the COPY's own flag: this install is a `cabal build' plus a hand copy,
# so cabal's own executable-stripping never runs -- `install -s' strips the copy
# instead, and only `release' asks for it, leaving a plain install's symbols.
OPT ?=
STRIP ?=

# ONE PREREQUISITE, GHCUP: bootstrap the pinned GHC + cabal from it and the
# WebKitGTK system libraries from pacman, so `make install' needs nothing else
# pre-installed.  Idempotent -- a satisfied machine installs nothing.
bootstrap:
	@tools/bootstrap

native: bootstrap
	$(NATIVE_ENV) \
	  cabal build $(NATIVE_BUILD) $(OPT) all

# The native build's own binary, copied over ~/.local/bin/glance atomically
# (a running daemon keeps the unlinked inode; the next launch takes the new one).
PREFIX ?= $(HOME)/.local
install: native
	@dest="$(PREFIX)/bin/glance"; \
	  bin="$$($(NATIVE_ENV) \
	          cabal list-bin -v0 $(NATIVE_BUILD) $(OPT) exe:glance)"; \
	  mkdir -p "$(PREFIX)/bin"; \
	  install -m 755 $(STRIP) "$$bin" "$$dest.new" && mv -f "$$dest.new" "$$dest"; \
	  echo "installed glance ($$(git -C $(CURDIR) rev-parse --short HEAD 2>/dev/null || echo local)) -> $$dest"

# The production build flags, ONE source for `release' and `dist': -O2 and
# split-sections.  Stripping is the copy's own (`STRIP=-s'), not a build flag.
RELEASE_OPT := --enable-optimization=2 --enable-split-sections

# THE PRODUCTION INSTALL: the UI rebuilt `--optimize' and re-embedded, the native
# binary at -O2 with its sections split, and the installed copy stripped -- the
# last two size-only, no runtime cost.  A plain `install' at -O1 embeds whatever
# `assets/' already holds; this forces all of it.  NOT a version cut (`major' etc).
release: elm
	@$(MAKE) --no-print-directory install OPT='$(RELEASE_OPT)' STRIP=-s

# THE PER-SYSTEM BUNDLE `dist/{triple}/', honest: only the systems THIS machine
# actually built.  The host triple's self-contained native binary always -- one
# file, the fresh `--optimize' UI and every asset embedded, -O2, split, stripped;
# `wasm32-wasi' when the ghc-wasm toolchain is there.  NO empty system dirs: cross
# and CI fill the rest, this box never fakes a folder it did not build.  `dist/'
# is throwaway (gitignored), wiped and rebuilt whole.
DIST := dist
# ghc's OWN target triple, not `uname''s, so the dir names the ABI the binary carries.
GHC_TRIPLE = $(shell ghc --info 2>/dev/null | sed -n 's/.*"target platform string","\([^"]*\)".*/\1/p')

dist: elm
	@triple="$(GHC_TRIPLE)"; test -n "$$triple" || { echo "dist: no ghc on PATH"; exit 1; }; \
	  rm -rf "$(DIST)"; d="$(DIST)/$$triple"; mkdir -p "$$d"; \
	  $(NATIVE_ENV) \
	    cabal build $(NATIVE_BUILD) $(RELEASE_OPT) all; \
	  bin="$$($(NATIVE_ENV) \
	          cabal list-bin -v0 $(NATIVE_BUILD) $(RELEASE_OPT) exe:glance)"; \
	  install -m 755 -s "$$bin" "$$d/glance"; \
	  printf '#!/bin/sh\n# Pick a FREE port; 7777 is the desktop daemon port.\nexec ./glance "$$@"\n' > "$$d/run.sh"; \
	  chmod +x "$$d/run.sh"; \
	  printf 'glance -- %s native build.  Self-contained: the UI and every asset\nare embedded in the binary, so this one file is the whole thing.\n\n  ./glance serve   --dir <org-tree> --port <port>   # headless server\n  ./glance desktop --dir <org-tree> --port <port>   # windowed (needs GTK/WebKit)\n  ./glance doctor                                    # check the tree\n  ./glance --help\n\nrun.sh execs ./glance with your args.  Pick a free port; 7777 is the daemon.\n' "$$triple" > "$$d/README.md"; \
	  echo "dist: $$d/glance ($$(du -h "$$d/glance" | cut -f1)), stripped -O2"
	@$(MAKE) --no-print-directory dist-wasm
	@printf 'glance -- distribution bundle.\n\nPer-system folders, each a self-contained build THIS machine produced:\n  %s/   native binary -- cd in, ./run.sh (or ./glance serve --dir <tree> --port <port>)\n  wasm32-wasi/    EXPERIMENTAL headless probe, present only when ghc-wasm was\n\nOnly systems built here appear; cross and CI fill the rest, none are faked.\n' "$(GHC_TRIPLE)" > "$(DIST)/README.md"
	@echo "dist/ ready:"; ls -R "$(DIST)"

# wasm32-wasi, the one cross target the tree can fill besides the host.  SKIPS
# LOUDLY without the ghc-wasm toolchain; the host build stays in dist/ regardless.
dist-wasm:
	@if [ ! -x "$$HOME/.ghc-wasm/wasm32-wasi-ghc/bin/wasm32-wasi-ghc" ]; then \
	  echo "dist-wasm: no toolchain at ~/.ghc-wasm -- skipped"; \
	else \
	  . "$$HOME/.ghc-wasm/env" && \
	  wasm32-wasi-cabal build --project-file=cabal.project.wasm glance-wasm-probe && \
	  d="$(DIST)/wasm32-wasi"; mkdir -p "$$d" && \
	  cp "$$(wasm32-wasi-cabal list-bin -v0 --project-file=cabal.project.wasm glance-wasm-probe)" \
	     "$$d/glance-wasm-probe.wasm" && \
	  printf 'glance -- wasm32-wasi probe (EXPERIMENTAL, headless).\n\n  wasmtime run --dir <org-tree>::/tree glance-wasm-probe.wasm /tree\n' \
	     > "$$d/README.md" && \
	  echo "dist-wasm: $$d/glance-wasm-probe.wasm"; \
	fi

wasm-spike:
	@if [ ! -x "$$HOME/.ghc-wasm/wasm32-wasi-ghc/bin/wasm32-wasi-ghc" ]; then \
	  echo "wasm-spike: no toolchain at ~/.ghc-wasm -- run ghc-wasm-meta's bootstrap first"; \
	else \
	  . "$$HOME/.ghc-wasm/env" && \
	  wasm32-wasi-cabal build --project-file=cabal.project.wasm glance-internal lib:glance; \
	fi

# The default browser's own tab; `run-native' is the app window.
run:
	cabal run glance -- desktop --browser xdg-open \
	  --dir $(GLANCE_DIR) --port $(GLANCE_PORT)

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
