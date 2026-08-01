.PHONY: test native
test:
	cabal test

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
