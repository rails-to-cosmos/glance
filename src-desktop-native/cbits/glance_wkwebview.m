// The Darwin native window: a WKWebView filling an NSWindow, the Cocoa mirror
// of `Glance.Desktop.WebKit''s GTK window.  Built only under the `native-window'
// flag on macOS (glance.cabal `if os(osx)'), linked against Cocoa + WebKit.
//
// ONE C ENTRY, `glance_native_window', that BLOCKS until the window closes --
// the shape `nativeWindow :: (Int,Int) -> String -> String -> IO ()' imports.
// Every JS message is answered HERE, so the FFI boundary carries no callback:
// the page posts `popup'/`quit'/`zoom' to `window.webkit.messageHandlers.*'
// (WKWebView's own shape, identical to the GTK side, AGENTS.hs), and this file
// opens the link, closes the window, or zooms the view.
//
// UNBUILT ON LINUX -- there is no macOS SDK here.  Verify ON A MAC (or a
// `macos-14' runner): the zoom clamp against `zoomAsked', the black paint with
// no white flash on load, `_blank'/`window.open' reaching a popup vs the system
// browser, ESC closing a popup, C-c returning control, and `[NSApp run]'
// returning (not `terminate') so the Haskell side stops the daemon.

#import <Cocoa/Cocoa.h>
#import <WebKit/WebKit.h>
#include <math.h>
#include <signal.h>
#include <stdlib.h>

// The page's own `followable' rule, spelled again -- this layer cannot see it
// (the same duplication `webby' is on the GTK side).
static BOOL glance_webby(NSString *u) {
  return [u hasPrefix:@"http://"] || [u hasPrefix:@"https://"];
}

// `zoomAsked' in C: parse a level, DROP one that will not read (a non-number, a
// NaN/Inf), else clamp to the band's [low, high] as whole percentages / 100.
static BOOL glance_zoom(NSString *said, int zmin, int zmax, double *out) {
  const char *s = said.UTF8String;
  if (s == NULL) return NO;
  char *end = NULL;
  double v = strtod(s, &end);
  if (end == s || *end != '\0') return NO;   // the whole string must be the number
  if (isnan(v) || isinf(v)) return NO;
  double lo = zmin / 100.0, hi = zmax / 100.0;
  if (v < lo) v = lo;
  if (v > hi) v = hi;
  *out = v;
  return YES;
}

// `[NSApp run]' returns only after `stop:' AND one more event; post a dummy so
// it returns at once when the window (or C-c) asks to leave.
static void glance_stop(void) {
  [NSApp stop:nil];
  NSEvent *nudge = [NSEvent otherEventWithType:NSEventTypeApplicationDefined
                                      location:NSZeroPoint
                                 modifierFlags:0
                                     timestamp:0
                                  windowNumber:0
                                       context:nil
                                       subtype:0
                                          data1:0
                                          data2:0];
  [NSApp postEvent:nudge atStart:YES];
}

// A popup's own window: ESC closes it, the way the GTK popup's key handler does.
@interface GlancePopupWindow : NSWindow
@end
@implementation GlancePopupWindow
- (void)cancelOperation:(id)sender { [self close]; }   // ESC
@end

@interface GlanceShell : NSObject
    <WKScriptMessageHandler, WKUIDelegate, WKNavigationDelegate, NSWindowDelegate>
@property (assign) int zmin;
@property (assign) int zmax;
@property (weak) WKWebView *view;      // the main view, for zoom
@property (weak) NSWindow *window;     // the main window; closing it leaves
@property (strong) NSMutableArray *popups;   // holds live popups against ARC
@end

@implementation GlanceShell

// A webby link opens IN a popup this process owns; anything else is the system
// browser's, exactly as the GTK `elsewhere'/`openMessage' split it.
- (void)openLink:(NSString *)u {
  if (u == nil || u.length == 0) return;
  if (glance_webby(u)) {
    [self openPopup:u];
  } else {
    NSURL *url = [NSURL URLWithString:u];
    if (url) [[NSWorkspace sharedWorkspace] openURL:url];
  }
}

- (void)openPopup:(NSString *)u {
  NSRect main = self.window.frame;
  CGFloat w = fmax(400, main.size.width * 4.0 / 5.0);
  CGFloat h = fmax(300, main.size.height * 9.0 / 10.0);
  GlancePopupWindow *pop =
    [[GlancePopupWindow alloc] initWithContentRect:NSMakeRect(0, 0, w, h)
       styleMask:(NSWindowStyleMaskTitled | NSWindowStyleMaskClosable | NSWindowStyleMaskResizable)
         backing:NSBackingStoreBuffered
           defer:NO];
  pop.releasedWhenClosed = NO;
  pop.title = u;
  pop.backgroundColor = [NSColor blackColor];
  [pop center];

  WKWebView *view = [[WKWebView alloc] initWithFrame:NSMakeRect(0, 0, w, h)
                                       configuration:[[WKWebViewConfiguration alloc] init]];
  // In-place navigation for the popup's OWN new-window links (GTK `inPlace').
  view.UIDelegate = self;
  pop.contentView = view;
  pop.delegate = self;
  [self.popups addObject:pop];
  [view loadRequest:[NSURLRequest requestWithURL:[NSURL URLWithString:u]]];
  [pop makeKeyAndOrderFront:nil];
}

// WKUIDelegate: a `_blank' anchor or a `window.open' WebKit tried to open in a
// NEW view arrives here.  Returning nil cancels the new view; the URL goes to a
// popup (main window) or in-place (a popup's own view).
- (WKWebView *)webView:(WKWebView *)webView
    createWebViewWithConfiguration:(WKWebViewConfiguration *)configuration
               forNavigationAction:(WKNavigationAction *)navigationAction
                    windowFeatures:(WKWindowFeatures *)windowFeatures {
  NSURL *url = navigationAction.request.URL;
  if (url == nil) return nil;
  if (webView == self.view) {
    [self openLink:url.absoluteString];         // main window: popup / system
  } else if (glance_webby(url.absoluteString)) {
    [webView loadRequest:navigationAction.request];   // a popup: in place
  } else {
    [[NSWorkspace sharedWorkspace] openURL:url];
  }
  return nil;
}

// WKScriptMessageHandler: the three names the page posts to.
- (void)userContentController:(WKUserContentController *)ucc
      didReceiveScriptMessage:(WKScriptMessage *)message {
  NSString *name = message.name;
  NSString *body = [NSString stringWithFormat:@"%@", message.body];
  if ([name isEqualToString:@"popup"]) {
    [self openLink:body];
  } else if ([name isEqualToString:@"quit"]) {
    [self.window close];
  } else if ([name isEqualToString:@"zoom"]) {
    double level;
    if (glance_zoom(body, self.zmin, self.zmax, &level)) {
      if (@available(macOS 11.0, *)) self.view.pageZoom = level;
    }
  }
}

// The MAIN window closing leaves the loop; a popup closing only drops itself.
- (void)windowWillClose:(NSNotification *)notification {
  if (notification.object == self.window) {
    glance_stop();
  } else {
    [self.popups removeObject:notification.object];
  }
}

@end

// C-c while the window stands: leave the loop rather than take the process, so
// the Haskell side (`runNative') stops the daemon.  The RTS is off the main
// thread; the stop must be posted TO it.
static void glance_on_sigint(int sig) {
  (void)sig;
  dispatch_async(dispatch_get_main_queue(), ^{ glance_stop(); });
}

void glance_native_window(int zmin, int zmax, const char *title, const char *url) {
  @autoreleasepool {
    NSApplication *app = [NSApplication sharedApplication];
    [app setActivationPolicy:NSApplicationActivationPolicyRegular];

    NSWindow *win =
      [[NSWindow alloc] initWithContentRect:NSMakeRect(0, 0, 1200, 800)
         styleMask:(NSWindowStyleMaskTitled | NSWindowStyleMaskClosable
                    | NSWindowStyleMaskMiniaturizable | NSWindowStyleMaskResizable)
           backing:NSBackingStoreBuffered
             defer:NO];
    win.releasedWhenClosed = NO;
    win.title = [NSString stringWithUTF8String:title];
    win.backgroundColor = [NSColor blackColor];
    [win center];

    WKWebViewConfiguration *cfg = [[WKWebViewConfiguration alloc] init];
    WKUserContentController *ucc = cfg.userContentController;

    GlanceShell *shell = [[GlanceShell alloc] init];
    shell.zmin = zmin;
    shell.zmax = zmax;
    shell.window = win;
    shell.popups = [NSMutableArray array];

    // The same override the GTK side injects at document-start, main frame only:
    // a scripted `window.open' becomes a `popup' message rather than a new view.
    NSString *override =
      @"window.open = function (u) {"
      @" window.webkit.messageHandlers.popup.postMessage(String(u));"
      @" return null; };";
    WKUserScript *script =
      [[WKUserScript alloc] initWithSource:override
                             injectionTime:WKUserScriptInjectionTimeAtDocumentStart
                          forMainFrameOnly:YES];
    [ucc addUserScript:script];
    [ucc addScriptMessageHandler:shell name:@"popup"];
    [ucc addScriptMessageHandler:shell name:@"quit"];
    [ucc addScriptMessageHandler:shell name:@"zoom"];

    WKWebView *view = [[WKWebView alloc] initWithFrame:NSMakeRect(0, 0, 1200, 800)
                                         configuration:cfg];
    view.navigationDelegate = shell;
    view.UIDelegate = shell;
    shell.view = view;
    // Black under the page too, so the load shows no white flash.
    if (@available(macOS 12.0, *)) view.underPageBackgroundColor = [NSColor blackColor];
    win.contentView = view;
    win.delegate = shell;

    NSURL *nsurl = [NSURL URLWithString:[NSString stringWithUTF8String:url]];
    [view loadRequest:[NSURLRequest requestWithURL:nsurl]];

    signal(SIGINT, glance_on_sigint);

    [win makeKeyAndOrderFront:nil];
    [app activateIgnoringOtherApps:YES];
    [app run];
    // Reached when the main window (or C-c) asked to leave; `shell', `view',
    // `win', `cfg' stay retained until here, then ARC releases them.
  }
}
