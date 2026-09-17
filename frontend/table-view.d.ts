type TableViewCell = string | number | null;

interface TableViewBadge {
  value: string;
  color: string;
}

interface TableViewColumn {
  key: string;
  header?: string;
  type?: "text" | "number" | "badge";
  align?: "left" | "right";
  sortable?: boolean;
  badges?: TableViewBadge[];
  values?: string[];
  multi?: boolean;
  editable?: boolean;
  compare?: string;
}

interface TableViewAction {
  key?: string;
  command: string;
  label?: string;
}

interface TableViewSort {
  column: string;
  ascending?: boolean;
  direction?: string;
  nullsFirst?: boolean;
}

interface TableViewSortKey {
  column: string;
  ascending: boolean;
  nullsFirst: boolean;
}

interface TableViewRow {
  id: string;
  cells?: Record<string, TableViewCell>;
  linked?: boolean;
}

interface TableViewSavedView {
  name: string;
  query?: string;
}

interface TableViewView {
  title?: string;
  columns: TableViewColumn[];
  actions?: TableViewAction[];
  sort?: TableViewSort | TableViewSort[];
  views?: TableViewSavedView[];
  rows?: TableViewRow[];
}

type TableViewOp =
  | { op: "insert"; index: number; row: TableViewRow }
  | { op: "delete"; index: number }
  | { op: "reset"; rows: TableViewRow[] };

interface TableViewCrumb {
  label: string;
  query: string;
}

interface TableViewMountOptions {
  onAction?: (command: string, id: string, row: TableViewRow) => void;
  onLink?: (target: string, row: TableViewRow | null) => void;
  onFilter?: (query: string) => void;
  onEdit?: (
    id: string | null,
    column: number,
    value: string,
    kind: "cell" | "header"
  ) => void;
  omnibox?: boolean;
  palette?: boolean;
  marks?: boolean;
  flags?: boolean;
  actionHints?: boolean;
  flagHelp?: string;
  pageSize?: number;
  initialQuery?: string;
  chipLabel?: (token: string) => string | null;
  composer?: boolean;
  inline?: boolean;
  filterDock?: "overlay" | "strip";
  onPin?: () => void;
  onRefused?: (token: string) => void;
  pinned?: boolean;
}

interface TableViewSelection {
  id: string | null;
  col: number | null;
}

interface TableViewPageInfo {
  page: number;
  pages: number;
  from: number;
  to: number;
  total: number;
}

interface TableViewToken {
  negated: boolean;
  added: boolean;
  key: string | null;
  value: string;
  quoted: boolean;
  start: number;
  end: number;
  sep: number;
}

interface TableViewHandle {
  el: HTMLElement;
  setView(view: TableViewView): void;
  setRows(rows: TableViewRow[]): void;
  upsertRow(row: TableViewRow): void;
  deleteRow(id: string): void;
  applyDelta(ops: TableViewOp[]): void;
  getRows(): TableViewRow[];
  getVisible(): TableViewRow[];
  select(id: string, column?: number): boolean;
  getSelection(): TableViewSelection;
  editCell(id: string, column: number): boolean;
  editHeader(column: number): boolean;
  getQuery(): string;
  setCrumbs(crumbs: TableViewCrumb[]): void;
  getCrumbs(): TableViewCrumb[];
  setPinned(on: boolean): void;
  setQuery(query: string): void;
  pushCrumb(crumb: TableViewCrumb): number;
  popCrumb(): TableViewCrumb | null;
  stripLastToken(): boolean;
  filtering(): boolean;
  destroy(): void;
  openFilter(options?: { narrow?: boolean }): void;
  closeFilter(): void;
  selectStep(step: number): boolean;
  nextPage(): boolean;
  previousPage(): boolean;
  pageInfo(): TableViewPageInfo;
  sortBy(column: string, ascending?: boolean): boolean;
  sortPromote(column: string): boolean;
  getSort(): TableViewSortKey[];
  setSort(sort?: TableViewSort | TableViewSort[] | TableViewSortKey[] | null): void;
  toggleMark(id: string): boolean;
  markAll(): number;
  flagRow(id: string): boolean;
  unflagRow(id: string): void;
  getFlagged(): string[];
  clearFlags(): void;
  flaggedCount(): number;
  getMarked(): string[];
  clearMarks(): void;
  markedCount(): number;
}

interface TableViewApi {
  mount(
    container: Element,
    view: TableViewView,
    options?: TableViewMountOptions
  ): TableViewHandle;
  displayText(value: TableViewCell | undefined): string;
  comparator(
    column: TableViewColumn
  ): (left: TableViewCell | undefined, right: TableViewCell | undefined) => number;
  parseQuery(query: string, keys?: string[]): TableViewToken[];
}

declare const TableView: TableViewApi;
