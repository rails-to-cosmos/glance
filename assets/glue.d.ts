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

/**
 * ONE PANEL ROW, the shape both sides of the port agree on: a property, or one
 * of the three fixed planning entries whose key is org's and whose delete
 * CLEARS rather than drops.  `assets/elm/src/Panel.elm' decodes exactly this.
 */
interface PanelRow {
  id: string;
  key: string;
  val: string;
  fixed: boolean;
}

/** The whole model, pushed back after every change for the shell to mirror. */
interface PanelState {
  rows: PanelRow[];
  at: number;
  id: string;
  flags: string[];
}

/** A stop in the document pane: a headline line, a body element, or a child. */
interface DocRow {
  id: string;
  kind: string;
  grain: string;
  name: string | null;
  owner: string | null;
  from: number;
  to: number;
  text: string;
  index: number;
  level: number;
  cells: { key: string; val: string }[];
  span: [number, number] | null;
}

/** The pane's whole model, pushed back after every change. */
interface DocState {
  rows: DocRow[];
  at: number;
  id: string;
  col: number | null;
  grain: string;
  flags: string[];
  lines: number;
  body: string;
}

interface DocPorts {
  docIn: { send(m: { kind: string } & Record<string, any>): void };
  docState: { subscribe(f: (s: DocState) => void): void };
  docSaid: { subscribe(f: (said: string) => void): void };
  docBody: { subscribe(f: (body: string) => void): void };
  docTook: {
    subscribe(f: (a: { taken: string[]; named: number; body: string }) => void): void;
  };
}

interface PanelPorts {
  panelIn: { send(m: { kind: string } & Record<string, any>): void };
  panelState: { subscribe(f: (s: PanelState) => void): void };
  panelOpen: { subscribe(f: (r: PanelRow) => void): void };
  panelTook: { subscribe(f: (cleared: string[]) => void): void };
}

// The Elm programs, compiled from `assets/elm' and served beside the shell:
// the property panel and the sheet's document pane.
declare const Elm: {
  Panel: { init(opts: { node: any; flags: string }): { ports: PanelPorts } };
  Doc: { init(opts: { node: any }): { ports: DocPorts } };
};
