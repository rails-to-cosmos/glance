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

import Control.Exception (SomeException, throwIO, try)
import Control.Monad (unless, void)
import Data.GI.Base (castTo)
import Data.Text (Text)
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
  _ <- WK.onWebViewDecidePolicy view (elsewhere win)
  Gtk.widgetShowAll win
  WK.webViewLoadUri view (T.pack url)
  previous <- installHandler sigINT (Catch quitLoop) Nothing
  Gtk.main
  void (installHandler sigINT previous Nothing)

-- | A link the page asked for a NEW WINDOW for goes to the system browser, and
-- this window keeps showing the table.
--
-- The shell's @o@ opens what a row points at with @window.open(…, "_blank")@,
-- and a link with @target="_blank"@ is the same request.  Both arrive here as a
-- @WebKitPolicyDecision@ of type @NewWindowAction@, and a @WebKitWebView@ with
-- nothing connected answers one by doing nothing at all — so following a link
-- would work in a browser tab and silently fail in the window this build
-- carries.  The decision is refused and the URI handed to the desktop's own
-- default handler, which is what a reader means by opening a link: a glance
-- window is the table, and a second one of these would be a browser with no
-- address bar.
--
-- Every other decision type is left to WebKit ('False'), which is what keeps
-- ordinary navigation — the page loading, the socket upgrading — untouched.
--
-- @gtk_show_uri_on_window@ can fail (no handler registered, a scheme nothing
-- claims) and it fails by throwing.  A window failure has never taken this
-- daemon down and this one does not either: the link is dropped and the table
-- stays up.
elsewhere :: Gtk.Window -> WK.PolicyDecision -> WK.PolicyDecisionType -> IO Bool
elsewhere win decision kind
  | kind /= WK.PolicyDecisionTypeNewWindowAction = pure False
  | otherwise = do
      uri <- navigationUri decision
      WK.policyDecisionIgnore decision
      maybe (pure ()) (systemOpen win) uri
      pure True

-- | Where a navigation decision is headed.  'Nothing' when the decision is not
-- a navigation one after all, which the type above says it is — the cast is
-- checked rather than assumed, since a null here would be a crash where the
-- honest answer is to drop the link.
navigationUri :: WK.PolicyDecision -> IO (Maybe Text)
navigationUri decision = do
  navigation <- castTo WK.NavigationPolicyDecision decision
  case navigation of
    Nothing  -> pure Nothing
    Just nav -> do
      action <- WK.navigationPolicyDecisionGetNavigationAction nav
      request <- WK.navigationActionGetRequest action
      Just <$> WK.uRIRequestGetUri request

-- | Hand URI to whatever the desktop opens it with, and swallow the failure.
-- The timestamp is @GDK_CURRENT_TIME@, which is what a caller with no event to
-- date the request by passes.
systemOpen :: Gtk.Window -> Text -> IO ()
systemOpen win uri = do
  outcome <- try (Gtk.showUriOnWindow (Just win) uri (fromIntegral Gdk.CURRENT_TIME))
  case outcome of
    Right () -> pure ()
    Left err -> putStrLn ("  window:  could not open " <> T.unpack uri
                            <> ": " <> show (err :: SomeException))

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
