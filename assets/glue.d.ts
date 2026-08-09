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

/** ONE ROW of a small list: an id, its cells by column key, and an optional ink. */
interface ListRow {
  id: string;
  cells: Record<string, string | number>;
  colour?: string;
}

/** Where point is and what is flagged, pushed back after every change. */
interface ListState {
  at: number;
  id: string;
  ids: string[];
  flags: string[];
}

interface ListPorts {
  listIn: { send(m: { kind: string } & Record<string, any>): void };
  listState: { subscribe(f: (s: ListState) => void): void };
  listClicked: { subscribe(f: (id: string) => void): void };
}

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

// The Elm programs, compiled from `assets/elm' and served beside the shell: the
// small-list widget (four instances) and the sheet's document pane.
declare const Elm: {
  Listing: {
    init(opts: {
      node: any;
      flags: { cols: { key: string; header: string }[]; hint: string };
    }): { ports: ListPorts };
  };
  Doc: { init(opts: { node: any }): { ports: DocPorts } };
};
