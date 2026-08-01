{-# LANGUAGE OverloadedStrings #-}

-- VENDORED, PATCHED: `version' names the TYPELIB haskell-gi reads at configure
-- time, and this machine carries JavaScriptCore-4.1 alone.  `pkgVersion' stays
-- the Hackage release this was cut from.  See gi-javascriptcore4.cabal.

import Data.GI.CodeGen.CabalHooks (setupBinding, TaggedOverride(..))

import qualified GI.GLib.Config as GLib
import qualified GI.GObject.Config as GObject


main :: IO ()
main = setupBinding name version pkgName pkgVersion verbose overridesFile inheritedOverrides outputDir
  where name = "JavaScriptCore"
        version = "4.1"          -- glance: 4.0 upstream
        pkgName = "gi-javascriptcore4"
        pkgVersion = "4.0.29"
        overridesFile = Nothing
        verbose = False
        outputDir = Nothing
        inheritedOverrides = [TaggedOverride "inherited:GLib" GLib.overrides, TaggedOverride "inherited:GObject" GObject.overrides]
