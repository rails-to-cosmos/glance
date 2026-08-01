.PHONY: test native sync-renderer
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
