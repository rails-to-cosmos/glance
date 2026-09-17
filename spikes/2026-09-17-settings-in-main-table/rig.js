(() => {
  "use strict";

  const variants = {
    flat: { note: "One searchable key/value catalogue. The shortest route, with weak grouping as the list grows." },
    category: { note: "Stable categories first; keyword layers reuse the same table-view drill and breadcrumb." },
    source: { note: "Storage ownership first. Precedence is explicit, but finding a key requires knowing its source." },
  };
  const settings = {
    interface: [
      ["Theme", "system", "dark"], ["Reading line", "browser", "on"],
      ["Zoom", "browser", "100%"], ["TODO hue", "system", "amber"],
      ["DONE hue", "system", "green"],
    ],
    views: [
      ["Default view", "tree", "main"], ["Rows per page", "tree", "50"],
      ["Log panel", "browser", "collapsed"],
    ],
    system: [
      ["TODO cycle", "system.org", "TODO · NEXT · DONE"],
      ["New item template", "system.org", "TODO %?"],
    ],
    book: [
      ["TODO cycle", "tag:book.org", "READ · READING · DONE"],
      ["New item template", "tag:book.org", "READ %?"],
    ],
    film: [
      ["TODO cycle", "tag:film.org", "WATCH · WATCHING · SEEN"],
      ["New item template", "tag:film.org", "WATCH %?"],
    ],
  };
  const main = [
    ["Design settings as a table", "project", "NEXT"],
    ["Review frozen columns spike", "project", "TODO"],
    ["Read Designing Data-Intensive Applications", "book", "READING"],
    ["Publish weekly notes", "routine", "DONE"],
  ];
  const overrides = new Map();
  const params = new URLSearchParams(location.search);
  const state = {
    variant: variants[params.get("variant")] ? params.get("variant") : "flat",
    page: params.get("page") === "main" ? "main" : "settings",
    path: [],
  };
  let table = null;
  let models = new Map();

  const $ = (selector) => document.querySelector(selector);
  const title = (text) => text.charAt(0).toUpperCase() + text.slice(1).replaceAll("-", " ");
  const say = (message) => { $("#echo").textContent = message; };
  const keyOf = (item) => `${item[1]}:${item[0]}`;
  const branch = (name, contents, target) => ({ kind: "branch", name, contents, target });
  const leaf = (item) => ({ kind: "leaf", item });
  const allSettings = () => Object.values(settings).flat();

  function routeModels() {
    if (state.page === "main") return main.map(leaf);
    const here = state.path.at(-1);
    if (state.variant === "flat") return allSettings().map(leaf);
    if (state.variant === "category") {
      if (!here) return [
        branch("Interface", "5 keys · theme, reading line, hues", "interface"),
        branch("Views", "3 keys · route and table density", "views"),
        branch("Keywords", "3 config layers", "keywords"),
      ];
      if (here === "keywords") return [
        branch("system.org", "2 keys · global fallback", "system"),
        branch("tag:book.org", "2 keys · overrides system", "book"),
        branch("tag:film.org", "2 keys · overrides system", "film"),
      ];
      return settings[here].map(leaf);
    }
    if (!here) return [
      branch("Browser", "4 local keys", "browser"),
      branch("Tree", "2 workspace keys", "tree"),
      branch("system.org", "4 global keys", "system-source"),
      branch("tag:book.org", "2 layer keys", "book"),
      branch("tag:film.org", "2 layer keys", "film"),
    ];
    if (here === "browser") return [...settings.interface.slice(0, 3), settings.views[2]].map(leaf);
    if (here === "tree") return settings.views.slice(0, 2).map(leaf);
    if (here === "system-source") return [...settings.interface.slice(3), ...settings.system].map(leaf);
    return settings[here].map(leaf);
  }

  function labels() {
    if (state.page === "main") return ["@ views:main"];
    return ["@ views:main", "settings", ...state.path.map(title)];
  }

  function asView() {
    const entries = routeModels();
    models = new Map();
    const branches = entries.some((entry) => entry.kind === "branch");
    const rows = entries.map((entry, index) => {
      if (entry.kind === "branch") {
        const id = `route:${entry.target}`;
        models.set(id, entry);
        return { id, linked: true, cells: { title: entry.name, contents: entry.contents } };
      }
      const [name, source, initial] = entry.item;
      const id = state.page === "main" ? `main:${index}` : `setting:${keyOf(entry.item)}`;
      models.set(id, entry);
      return { id, cells: { title: name, source, value: overrides.get(keyOf(entry.item)) ?? initial } };
    });
    return {
      title: labels().at(-1),
      columns: branches
        ? [
            { key: "title", header: "Route", sortable: true },
            { key: "contents", header: "Contents" },
          ]
        : [
            { key: "title", header: state.page === "main" ? "Item" : "Key", sortable: true },
            { key: "source", header: "Source", sortable: true },
            { key: "value", header: "Value", sortable: true, editable: state.page === "settings" },
          ],
      rows,
    };
  }

  function renderCrumbs() {
    const names = labels();
    $("#crumbs").innerHTML = names.map((name, index) =>
      `${index ? '<span class="sep">›</span>' : ""}<button class="crumb ${index === names.length - 1 ? "current" : ""}" data-depth="${index}">${name}</button>`
    ).join("");
    $("#depth").textContent = String(names.length);
    document.querySelectorAll(".crumb").forEach((button) => button.addEventListener("click", () => {
      const depth = Number(button.dataset.depth);
      if (depth === 0) { state.page = "main"; state.path = []; }
      else { state.page = "settings"; state.path = state.path.slice(0, depth - 1); }
      render();
    }));
  }

  function renderTable() {
    const mount = $("#mount");
    if (table) table.destroy();
    const view = asView();
    table = TableView.mount(mount, view, {
      actionHints: false,
      filterDock: "strip",
      pageSize: 50,
      onEdit: (id, column, value, kind) => {
        if (kind !== "cell" || !id || column !== 2) return;
        const model = models.get(id);
        if (!model || model.kind !== "leaf") return;
        overrides.set(keyOf(model.item), value);
        const row = table.getRows().find((candidate) => candidate.id === id);
        if (row) { row.cells.value = value; table.upsertRow(row); }
        say(`saved ${model.item[0]}`);
      },
    });
    const first = view.rows[0];
    if (first) table.select(first.id);
    mount.ondblclick = (event) => {
      const row = event.target.closest("tr[data-id]");
      if (row && models.get(row.dataset.id)?.kind === "branch") activate();
    };
  }

  function render() {
    document.querySelectorAll("[data-variant]").forEach((button) =>
      button.classList.toggle("active", button.dataset.variant === state.variant));
    $("#variant-note").textContent = variants[state.variant].note;
    renderCrumbs();
    renderTable();
  }

  function activate() {
    const selected = table && table.getSelection().id;
    const model = selected && models.get(selected);
    if (!model) return;
    if (state.page === "main") { say(`opened: ${model.item[0]}`); return; }
    if (model.kind === "branch") {
      state.path.push(model.target);
      render();
      return;
    }
    table.editCell(selected, 2);
  }

  function toggleSettings() {
    state.page = state.page === "settings" ? "main" : "settings";
    state.path = [];
    say(state.page === "settings" ? "settings replaced main table" : "returned to @ views:main");
    render();
  }

  document.querySelectorAll("[data-variant]").forEach((button) => button.addEventListener("click", () => {
    state.variant = button.dataset.variant;
    state.page = "settings";
    state.path = [];
    render();
  }));

  document.addEventListener("keydown", (event) => {
    if (event.target.matches("input, textarea, select")) return;
    if (event.key === ",") toggleSettings();
    else if (event.key === "n" || event.key === "ArrowDown") table.selectStep(1);
    else if (event.key === "p" || event.key === "ArrowUp") table.selectStep(-1);
    else if (event.key === "Enter") activate();
    else if (event.key === "Delete" || event.key === "Backspace") {
      if (state.path.length) state.path.pop();
      else if (state.page === "settings") state.page = "main";
      render();
    } else if (event.key === "/") {
      event.preventDefault();
      table.openFilter();
    }
  });

  render();
})();
