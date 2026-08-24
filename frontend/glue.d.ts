// Typed loosely on purpose: the renderer's surface is its own repo's jsconfig.
declare const TableView: any;

// Injected by the native window, and only there.
interface Window {
  webkit?: { messageHandlers?: Record<string, { postMessage(v: any): void }> };
}

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
  fold: boolean;
  entries: boolean;
  name: string | null;
  owner: string | null;
  from: number;
  to: number;
  text: string;
  index: number;
  level: number;
  cells: { key: string; val: string }[];
  span: [number, number] | null;
  reach: [number, number] | null;
}

/** The pane's whole model, pushed back after every change. */
interface DocState {
  rows: DocRow[];
  at: number;
  id: string;
  grain: string;
  flags: string[];
  lines: number;
  body: string;
  properties: string[][];
  planning: string[][];
}

interface DocCargo {
  body: string;
  properties: string[][];
  planning: string[][];
}

interface DocPorts {
  docIn: { send(m: { kind: string } & Record<string, any>): void };
  docState: { subscribe(f: (s: DocState) => void): void };
  docSaid: { subscribe(f: (said: string) => void): void };
  docBody: { subscribe(f: (cargo: DocCargo) => void): void };
  docTook: {
    subscribe(
      f: (a: DocCargo & { taken: string[]; refused: number; meta: number }) => void
    ): void;
  };
}

// Compiled from `assets/elm', served beside the shell.
declare const Elm: {
  Listing: {
    init(opts: {
      node: any;
      flags: { cols: { key: string; header: string }[]; hint: string };
    }): { ports: ListPorts };
  };
  Doc: { init(opts: { node: any }): { ports: DocPorts } };
};
