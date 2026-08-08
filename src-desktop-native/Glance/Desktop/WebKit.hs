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
import qualified GI.JavaScriptCore as JSC
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
  ucm <- WK.webViewGetUserContentManager view
  _ <- WK.onUserContentManagerScriptMessageReceived ucm (Just handlerName) (openMessage win)
  _ <- WK.userContentManagerRegisterScriptMessageHandler ucm handlerName
  -- `q' ON THE MAIN PAGE QUITS, which only a window can answer: the page posts
  -- here and this destroys the window, and closing the window stops the daemon
  -- because the window IS the app.  A browser tab reaching for the same key
  -- finds no handler and says so.
  _ <- WK.onUserContentManagerScriptMessageReceived ucm (Just quitName)
         (\_value -> Gtk.widgetDestroy win)
  _ <- WK.userContentManagerRegisterScriptMessageHandler ucm quitName
  override <- WK.userScriptNew openOverride
                WK.UserContentInjectedFramesTopFrame
                WK.UserScriptInjectionTimeStart Nothing Nothing
  WK.userContentManagerAddScript ucm override
  Gtk.widgetShowAll win
  WK.webViewLoadUri view (T.pack url)
  previous <- installHandler sigINT (Catch quitLoop) Nothing
  Gtk.main
  void (installHandler sigINT previous Nothing)

-- | A link the page asked for a NEW WINDOW for opens in a POPUP of this
-- window's own, and this window keeps showing the table.  The native window
-- has no tabs to switch to, so an @http(s)@ link reads HERE, in a transient
-- window sized the way the material sheet sits over the table; anything else
-- — @mailto:@, a scheme the desktop owns — goes to the system handler, a mail
-- client being the right reader either way.
--
-- TWO DOORS, because WebKit has two.  A real anchor with @target="_blank"@
-- arrives as a @NewWindowAction@ policy decision, and 'elsewhere' answers it.
-- The shell's @o@ opens links with @window.open(…)@, and a scripted open
-- NEVER reaches the policy decision: it fires the @create@ signal — whose
-- unconnected default returns null and drops the open on the floor, which is
-- how the first live press of @o@ found that the previous system-browser
-- handoff, wired to the policy door alone, had never fired at all.  And the
-- @create@ door cannot be taken: returning a view from it makes WebKitGTK
-- read the scripted open's window features, which @window.open(…, "noopener")@
-- leaves unset, and the web process aborts the whole daemon on the engaged
-- assertion (@std::optional\<WebCore::WindowFeatures\>::operator*@, observed
-- live under 2.50).  So the scripted half is intercepted BEFORE WebKit's
-- window machinery: 'openOverride' replaces @window.open@ at document start
-- with a post to the 'handlerName' script-message handler, and 'openMessage'
-- opens the popup itself — the same shape a WKWebView port must use, iOS
-- having no @create@ signal to connect.
--
-- Every other decision type is left to WebKit ('False'), which is what keeps
-- ordinary navigation — the page loading, the socket upgrading — untouched.
elsewhere :: Gtk.Window -> WK.PolicyDecision -> WK.PolicyDecisionType -> IO Bool
elsewhere win decision kind
  | kind /= WK.PolicyDecisionTypeNewWindowAction = pure False
  | otherwise = do
      uri <- navigationUri decision
      WK.policyDecisionIgnore decision
      case uri of
        Just u | webby u -> popupOpen win u
        Just u           -> systemOpen win u
        Nothing          -> pure ()
      pure True

-- | The name the page's patched @window.open@ posts to.
handlerName :: Text
handlerName = T.pack "popup"

-- | And the one @q@ posts to.  Its PRESENCE is the page's test for "is there a
-- window to quit", so the name is the contract.
quitName :: Text
quitName = T.pack "quit"

-- | The document-start patch: @window.open@ posts its URL to 'handlerName'
-- and answers null, which is what the un-patched engine answered anyway once
-- the default @create@ handler dropped the open.  The shell never reads the
-- return value.  Injected into the top frame alone; a popup's own view is
-- built without it, so a page read in a popup keeps a real @window.open@ —
-- inert there, its @create@ going unanswered, which is the drop rather than
-- the crash.
openOverride :: Text
openOverride = T.concat
  [ T.pack "window.open = function (u) {"
  , T.pack " window.webkit.messageHandlers.", handlerName
  , T.pack ".postMessage(String(u));"
  , T.pack " return null; };"
  ]

-- | The scripted-open door: the URL posted by 'openOverride', read back out
-- of the JavaScript world and opened the way 'elsewhere' opens the anchor
-- kind.
openMessage :: Gtk.Window -> WK.JavascriptResult -> IO ()
openMessage win result = do
  value <- WK.javascriptResultGetJsValue result
  uri <- JSC.valueToString value
  if webby uri then popupOpen win uri else systemOpen win uri

-- | What the in-window reader takes: the two schemes a page can render.  The
-- page's own @followable@ rule, spelled here because this layer cannot see it.
webby :: Text -> Bool
webby u = any ((`T.isPrefixOf` u) . T.pack) ["http://", "https://"]

-- | Open URI in the reading pane.
popupOpen :: Gtk.Window -> Text -> IO ()
popupOpen win uri = do
  view <- popupShell win uri
  WK.webViewLoadUri view uri

-- | The reading pane: 80% × 90% of the main window, centred over it and
-- transient so the manager stacks the pair; ESC or the manager's close ends
-- it, and closing it never touches the table.  Its own new-window requests
-- navigate IN PLACE — one popup is a reading pane, and a tree of them would
-- be a browser with no address bars.
popupShell :: Gtk.Window -> Text -> IO WK.WebView
popupShell win uri = do
  (w, h) <- Gtk.windowGetSize win
  pop <- Gtk.windowNew Gtk.WindowTypeToplevel
  Gtk.windowSetTitle pop uri
  Gtk.windowSetTransientFor pop (Just win)
  Gtk.windowSetPosition pop Gtk.WindowPositionCenterOnParent
  Gtk.windowSetDefaultSize pop (max 400 (w * 4 `div` 5)) (max 300 (h * 9 `div` 10))
  view <- WK.webViewNew
  rgba <- black
  WK.webViewSetBackgroundColor view rgba
  Gtk.containerAdd pop view
  _ <- WK.onWebViewDecidePolicy view (inPlace view)
  _ <- Gtk.onWidgetKeyPressEvent pop $ \ev -> do
         kv <- Gdk.getEventKeyKeyval ev
         if kv == Gdk.KEY_Escape
           then True <$ Gtk.widgetDestroy pop
           else pure False
  Gtk.widgetShowAll pop
  pure view

-- | The popup's own policy: a new-window request loads where the reader is.
inPlace :: WK.WebView -> WK.PolicyDecision -> WK.PolicyDecisionType -> IO Bool
inPlace view decision kind
  | kind /= WK.PolicyDecisionTypeNewWindowAction = pure False
  | otherwise = do
      uri <- navigationUri decision
      WK.policyDecisionIgnore decision
      maybe (pure ()) (WK.webViewLoadUri view) uri
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
-- date the request by passes.  @gtk_show_uri_on_window@ can fail (no handler
-- registered, a scheme nothing claims) and it fails by throwing; a window
-- failure has never taken this daemon down and this one does not either — the
-- link is dropped and the table stays up.
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
