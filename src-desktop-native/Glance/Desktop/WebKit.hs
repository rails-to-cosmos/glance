{-# LANGUAGE CPP #-}

-- | The native window: a @WebKitWebView@ filling a @GtkWindow@, and nothing
-- else.
--
-- Stage 2 of the proposal's desktop shell (rev 4).  The architecture is the
-- one stage 1 runs — daemon, bridge, web UI — and this is one more client of
-- the same loopback server; what it replaces is the app-mode browser, which
-- costs a second process, a profile directory and a browser's claim on
-- @Ctrl+T@, @Ctrl+N@ and @Ctrl+W@.  A bare web view has no chrome to bind
-- those to, so the keymap the page declares is the whole of what the keyboard
-- does.
--
-- The engine and nothing above it.  Which window to open, when, and what
-- closing it means are 'Glance.Desktop.Native''s, which knows nothing of GTK;
-- this module knows nothing of the daemon.  They meet in the CLI.
--
-- Built only under the @native-window@ cabal flag.  Unflagged, 'nativeAvailable'
-- is 'False', nothing resolves @gi-gtk@ or @gi-webkit2@, and @glance desktop@
-- runs stage 1 exactly as before.
module Glance.Desktop.WebKit (nativeAvailable, nativeWindow) where

#ifdef NATIVE_WINDOW

import Control.Exception (throwIO)
import Control.Monad (unless, void)
import Data.Word (Word32)
import System.Posix.Signals (Handler (Catch), installHandler, sigINT)

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified GI.Gdk as Gdk
import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk
import qualified GI.WebKit2 as WK

#endif

-- | Whether this build has a window of its own — the one question the rest of
-- the program asks about the flag.
nativeAvailable :: Bool

-- | Open a window titled TITLE on URL and block until it closes.
--
-- Sized 1200x800 and black before the first paint — the page's own dark
-- background, set on the window and on the web view so neither flashes white
-- while WebKit starts.  It closes on the window manager's request, on the
-- page's, or on @Ctrl-C@.
--
-- @Ctrl-C@ needs the handler.  The main thread sits inside @gtk_main@, a
-- foreign call, where the RTS cannot deliver the interrupt until it returns;
-- the handler asks the GTK loop to quit instead, from an idle callback, which
-- is the one thread-safe way in.  The previous handler goes back before this
-- returns, so a caller still waiting after the window is gone is interruptible
-- again.
--
-- A display GTK cannot open throws, and the caller keeps serving without a
-- window.  @gtk_init_check@ is what makes that possible: @gtk_init@ would
-- print and exit the process, taking a daemon with it for want of an X server.
nativeWindow :: String -> String -> IO ()

#ifdef NATIVE_WINDOW

nativeAvailable = True

nativeWindow title url = do
  (started, _args) <- Gtk.initCheck Nothing
  unless started (throwIO (userError "GTK could not open a display"))
  paintBlack
  win <- Gtk.windowNew Gtk.WindowTypeToplevel
  Gtk.windowSetTitle win (T.pack title)
  Gtk.windowSetDefaultSize win 1200 800
  view <- WK.webViewNew
  rgba <- black
  WK.webViewSetBackgroundColor view rgba
  Gtk.containerAdd win view
  _ <- Gtk.onWidgetDestroy win Gtk.mainQuit
  Gtk.widgetShowAll win
  WK.webViewLoadUri view (T.pack url)
  previous <- installHandler sigINT (Catch quitLoop) Nothing
  Gtk.main
  void (installHandler sigINT previous Nothing)

-- | Ask the GTK loop to stop, from whichever thread calls this.  @g_idle_add@
-- is the thread-safe door into a running loop, and a signal handler runs on a
-- thread of the RTS's choosing.
quitLoop :: IO ()
quitLoop = void (GLib.idleAdd GLib.PRIORITY_DEFAULT (Gtk.mainQuit >> pure False))

-- | Paint this process's windows black before they are drawn.  The web view
-- covers the whole of the one window here, so this shows only between mapping
-- the window and WebKit's first frame — long enough to flash white without it.
paintBlack :: IO ()
paintBlack = do
  screen <- Gdk.screenGetDefault
  case screen of
    Nothing -> pure ()   -- No display, and 'Gtk.init' has already said so.
    Just s -> do
      css <- Gtk.cssProviderNew
      Gtk.cssProviderLoadFromData css (TE.encodeUtf8 (T.pack "window{background:#000000}"))
      Gtk.styleContextAddProviderForScreen s css appPriority
  where
    appPriority :: Word32
    appPriority = fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION

-- | Opaque black — the page's own background under the dark theme.
black :: IO Gdk.RGBA
black = do
  rgba <- Gdk.newZeroRGBA
  Gdk.setRGBARed rgba 0
  Gdk.setRGBAGreen rgba 0
  Gdk.setRGBABlue rgba 0
  Gdk.setRGBAAlpha rgba 1
  pure rgba

#else

nativeAvailable = False

-- Unflagged there is no window to open.  'nativeAvailable' is what keeps this
-- out of reach; saying so beats failing, the way every other window failure on
-- this path does.
nativeWindow _title url =
  putStrLn ("  window:  no native window in this build (cabal -f native-window); open "
              <> url <> " yourself")

#endif
