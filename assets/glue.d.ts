// The two globals the shell reads and does not define: the renderer's mount
// (vendored beside this file) and the page's own config blob element.  Typed
// loosely on purpose — the renderer's real surface is its own repo's jsconfig,
// and CFG's shape is Glance.Web.Page.Glue's.
declare const TableView: any;

// The WebKit message bridge the native window injects, and only there: the
// page asks whether it is running inside one before it posts anything.
interface Window {
  webkit?: { messageHandlers?: Record<string, { postMessage(v: any): void }> };
}
