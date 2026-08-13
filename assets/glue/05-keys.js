// KEY NAMING AND THE ECHO PILL, the shell's first widget behind an argument
// list (docs/proposals/2026-08-08-widget-files.partial.md, step C).  It takes
// `el' and nothing else, so what it may reach is stated rather than inherited
// from the script scope every other part still shares.
const Keys = ((el) => {
    const NAMED = { Enter: "RET", Tab: "TAB", " ": "SPC", Escape: "ESC",
      Backspace: "DEL", Delete: "<delete>", ArrowUp: "<up>", ArrowDown: "<down>",
      ArrowLeft: "<left>", ArrowRight: "<right>", Home: "<home>", End: "<end>",
      PageUp: "<prior>", PageDown: "<next>" };
    // A letter names a PHYSICAL key: `e.code' for A-Z, `e.key' for the rest.
    const LETTER = /^Key([A-Z])$/;
    const downKeys = new Set();
    const keyToken = (e) => e.code || e.key;
    function repeating(e) {
      if (e.glanceRepeat === undefined) {
        const t = keyToken(e);
        e.glanceRepeat = !!e.repeat || downKeys.has(t);
        downKeys.add(t);
      }
      return e.glanceRepeat;
    }
    document.addEventListener("keyup", (e) => downKeys.delete(keyToken(e)));
    window.addEventListener("blur", () => downKeys.clear());
    function keyName(e) {
      let base = NAMED[e.key], special = base !== undefined;
      if (!special && /^F\d{1,2}$/.test(e.key))
        { base = `<${e.key.toLowerCase()}>`; special = true; }
      if (!special) {
        const sits = LETTER.exec(e.code || "");
        base = sits ? (e.shiftKey ? sits[1] : sits[1].toLowerCase()) : e.key;
        if (base.length !== 1) return null;
      }
      let mods = "";
      if (e.ctrlKey) mods += "C-";
      if (e.altKey || e.metaKey) mods += "M-";
      if (special && e.shiftKey) mods += "S-";
      return mods + base;
    }
    let echoAt = null, pending = [], pendingAt = null;
    function echo(text, hold) {
      const pill = el("echo");
      pill.textContent = text;
      pill.style.opacity = "1";
      clearTimeout(echoAt);
      if (!hold) echoAt = setTimeout(() => (pill.style.opacity = "0"), 1500);
    }
    function prefix(keys) {
      pending = keys;
      clearTimeout(pendingAt);
      if (!keys.length) return;
      const shown = keys.join(" ");
      echo(`${shown} -`, true);
      pendingAt = setTimeout(() => { pending = []; echo(`${shown} - timed out`); }, 2000);
    }

    // `pending' is the widget's own and leaves as an ANSWER rather than as the
    // array: the dispatch reads what is held, `prefix' is the only way to move it.
    const pendingKeys = () => pending.slice();
    return { echo, keyName, prefix, pendingKeys, repeating };
})(el);
// Consumers keep their spelling: the boundary is what the widget may SEE, and
// renaming forty call sites would buy nothing the argument list has not.
const { echo, keyName, prefix, pendingKeys, repeating } = Keys;
