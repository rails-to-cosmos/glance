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
    /** @typedef {object} Surface
     * @property {string} name         what `momentary()' answers with.
     * @property {boolean} [momentary] raised over the sheet rather than beside it.
     * @property {() => boolean} up    is it on screen.
     * @property {() => void} [off]    close it; absent means ESC falls through.
     * @property {() => boolean} [edit] is an edit open INSIDE it.
     * @property {() => void} [shut]   close that edit and leave the surface up.
     */
    /** @type {Surface[]} */
