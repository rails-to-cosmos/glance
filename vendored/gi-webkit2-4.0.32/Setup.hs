{-# LANGUAGE OverloadedStrings #-}

-- VENDORED, PATCHED: `version' names the TYPELIB haskell-gi reads at configure
-- time, and this machine carries WebKit2-4.1 alone.  `pkgVersion' stays the
-- Hackage release this was cut from.  @GI.Soup.Config@ now comes from
-- @gi-soup3@ -- same module path, Soup 3.0 instead of 2.4, which is the whole
-- of the 4.0/4.1 difference.  See gi-webkit2.cabal.

import Data.GI.CodeGen.CabalHooks (setupBinding, TaggedOverride(..))

import qualified GI.Atk.Config as Atk
import qualified GI.GLib.Config as GLib
import qualified GI.GObject.Config as GObject
import qualified GI.Gdk.Config as Gdk
import qualified GI.Gio.Config as Gio
import qualified GI.Gtk.Config as Gtk
import qualified GI.JavaScriptCore.Config as JavaScriptCore
import qualified GI.Soup.Config as Soup
import qualified GI.Cairo.Config as Cairo


main :: IO ()
main = setupBinding name version pkgName pkgVersion verbose overridesFile inheritedOverrides outputDir
  where name = "WebKit2"
        version = "4.1"          -- glance: 4.0 upstream
        pkgName = "gi-webkit2"
        pkgVersion = "4.0.32"
        overridesFile = Just "WebKit2.overrides"
        verbose = False
        outputDir = Nothing
        inheritedOverrides = [TaggedOverride "inherited:Atk" Atk.overrides, TaggedOverride "inherited:GLib" GLib.overrides, TaggedOverride "inherited:GObject" GObject.overrides, TaggedOverride "inherited:Gdk" Gdk.overrides, TaggedOverride "inherited:Gio" Gio.overrides, TaggedOverride "inherited:Gtk" Gtk.overrides, TaggedOverride "inherited:JavaScriptCore" JavaScriptCore.overrides, TaggedOverride "inherited:Soup" Soup.overrides, TaggedOverride "inherited:Cairo" Cairo.overrides]
