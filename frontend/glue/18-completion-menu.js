// Synchronous completion lists: their model, paint, walk, take and viewport
// placement. Features supply vocabulary and the spelling a chosen item writes.
const CompletionMenus = ((deps) => {
    const { atIn, part } = deps;
    const edge = 8;
    const viewport = () =>
      ({ top: 0, left: 0, right: window.innerWidth, bottom: window.innerHeight });
    const offerItem = (box, item, selected) => {
      const row = part(box, "div", selected ? "dof dat" : "dof");
      part(row, "span", "dow", item.word);
      if (item.hint) part(row, "span", "dot", item.hint);
    };

    /** @param {CompletionMenuOptions} options @returns {CompletionMenu} */
    function create(options) {
      let items = [], point = -1;
      const paint = () => {
        const box = options.element();
        if (point >= items.length) point = items.length - 1;
        box.textContent = "";
        box.className = items.length ? "on" : "";
        items.forEach((item, i) =>
          (options.renderItem || offerItem)(box, item, i === point));
      };
      const place = () => {
        const box = options.element();
        const rect = options.anchor && options.anchor();
        if (!rect || typeof box.getBoundingClientRect !== "function") return;
        const style = box.style;
        style.minWidth = `${rect.width}px`;
        const bounds = viewport(), drawn = box.getBoundingClientRect();
        const over = items.length > 0
          && rect.bottom + drawn.height + edge > bounds.bottom;
        box.classList.toggle("flipped", over);
        style.top = `${over ? Math.max(bounds.top + edge, rect.top - drawn.height)
                             : rect.bottom}px`;
        style.left = `${Math.max(bounds.left + edge,
                        Math.min(rect.left, bounds.right - drawn.width - edge))}px`;
      };
      return {
        setItems(next, initialPoint) {
          items = next;
          point = initialPoint === undefined ? -1 : initialPoint;
          paint();
          place();
        },
        move(step) {
          if (!items.length) return;
          point = atIn(items, point + step);
          paint();
        },
        take(field) {
          const took = point < 0 ? null : options.apply(items[point], field);
          if (!took) return false;
          const [value, at] = took;
          field.value = value;
          field.setSelectionRange(at, at);
          options.changed();
          return true;
        },
        place,
        close() { items = []; point = -1; paint(); },
        count: () => items.length,
        point: () => point,
      };
    }
    return { create, edge, viewport };
  })({ atIn, part });
