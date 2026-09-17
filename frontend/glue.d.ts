// Glance's vendored renderer extensions. The shared surface lives beside the
// renderer in `table-view.d.ts`; these disappear as the fork is upstreamed.
interface TableViewRow {
  producer?: boolean;
  under?: string | null;
  refused?: string;
}

interface TableViewOpenCell {
  id: string | null;
  col: number;
  key: string;
  value: string;
  raw: string;
  token: number;
}

interface TableViewMountOptions {
  onCellKey?: (event: KeyboardEvent, cell: TableViewOpenCell) => boolean;
}

interface TableViewHandle {
  fitColumns(): void;
  closeEditor(): void;
  cellRect(id: string, column: number): DOMRect | null;
  getEditing(): TableViewOpenCell | null;
}

interface CompletionItem {
  word: string;
  hint?: string;
}

interface CompletionMenuOptions {
  element(): HTMLElement;
  anchor?: () => DOMRect | null;
  renderItem?: (
    element: HTMLElement,
    item: CompletionItem,
    selected: boolean
  ) => void;
  apply(
    item: CompletionItem,
    field: HTMLInputElement
  ): [string, number] | null;
  changed(): void;
}

interface CompletionMenu {
  setItems(items: CompletionItem[], initialPoint?: number): void;
  move(step: number): void;
  take(field: HTMLInputElement): boolean;
  place(): void;
  close(): void;
  count(): number;
  point(): number;
}

interface GlueBand {
  key: string;
  def: number;
  min: number;
  max: number;
  step?: number;
}

interface GlueConfig {
  views: { id: string; query: string }[];
  dcells: string[];
  planning: string[];
  settable: string[];
  archiveTag: string;
  followable: string[];
  material: string[];
  codes: { code: string; means: string }[];
  lcols: TableViewColumn[];
  tcols: TableViewColumn[];
  log: GlueBand;
  zoom: GlueBand & { step: number };
}

interface WireSpan {
  start: number;
  end: number;
}

interface HeadlineLink {
  target: string;
  desc: string;
  type: string;
  span: [number, number];
}

interface HeadlineChild {
  index: number;
  level: number;
  line: number;
  span: WireSpan;
  [cell: string]: string | number | WireSpan | null;
}

interface HeadlineResponse {
  id: string;
  file: string;
  child: number | null;
  parent: number | null;
  path: string[];
  level: number;
  cells: Record<string, string | null>;
  children: HeadlineChild[];
  org: string;
  body: string;
  ownLines: number;
  properties: string[][];
  planning: string[][];
  logbook: string;
  digest: string;
  span: WireSpan;
  links: HeadlineLink[];
  titleAt: number | null;
}

interface ConfigKeywords {
  active: string[];
  inactive: string[];
}

interface ConfigLayer {
  path: string;
  tag: string | null;
  lines: string[];
  keywords: ConfigKeywords;
  template: string;
  digest: string;
}

interface ConfigResponse {
  layers: ConfigLayer[];
  tagsDir: string;
  keywords: ConfigKeywords;
  views: { id: string; query: string }[];
  themes: string[];
  colors: { theme: string; keyword: string; hue: string }[];
}

interface GitOutsideRepo {
  repo: false;
  dir: string;
}

interface GitRepoStatus {
  repo: true;
  dir: string;
  branch: string;
  upstream: string | null;
  detached: boolean;
  ahead: number;
  behind: number;
  staged: number;
  unstaged: number;
  untracked: number;
  locked: string | null;
  action: string | null;
  glyph: string;
  cls: string;
  label: string;
  autosync: boolean;
  armed: boolean;
}

type GitStatus = GitOutsideRepo | GitRepoStatus;

type StoreFrame =
  | { op: "upsert-row"; row: TableViewRow }
  | { op: "delete-row"; id: string };

type SaveState = "synced" | "syncing" | "conflict" | "error";

type SettingState =
  | "saved"
  | "changed"
  | "syncing"
  | "conflict"
  | "error"
  | "read-only";

interface SettingDescriptor {
  id: string;
  label: string;
  area: string;
  appliesTo: string;
  source: string;
  read(): string;
  state(): SettingState;
  commit?: (raw: string) => void | Promise<void>;
}

interface SaveSession {
  noteId: string;
  scope: string;
  state: SaveState;
  closed: string;
  dirty(): boolean;
  flush(): Promise<boolean>;
  refresh(): Promise<boolean>;
  shut(): void;
}

interface PageRoute {
  name: string;
  address: string;
  session: SaveSession;
  enter(): void;
  leave(): void;
  editing(): boolean;
  cancelEdit(): void;
  narrowed(): boolean;
  widen(): void;
}

interface PageAddress {
  label: string;
  back: boolean;
}

interface PageCoordinator {
  current(): PageRoute | null;
  named(name: string | null): PageRoute | null;
  address(): PageAddress;
  session(): SaveSession | null;
  open(route: PageRoute): void;
  mount(
    route: PageRoute,
    view: TableViewView,
    options?: TableViewMountOptions
  ): TableViewHandle;
  finish(route: PageRoute): void;
  fail(route: PageRoute): void;
}

// Injected by the native window, and only there.
interface Window {
  webkit?: { messageHandlers?: Record<string, { postMessage(v: any): void }> };
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
  keys: string[];
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
  Doc: { init(opts: { node: any }): { ports: DocPorts } };
};
