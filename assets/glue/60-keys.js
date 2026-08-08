    const NAMED = { Enter: "RET", Tab: "TAB", " ": "SPC", Escape: "ESC",
      Backspace: "DEL", Delete: "<delete>", ArrowUp: "<up>", ArrowDown: "<down>",
      ArrowLeft: "<left>", ArrowRight: "<right>", Home: "<home>", End: "<end>",
      PageUp: "<prior>", PageDown: "<next>" };
    // ONE NAME PER PRESS, and the SPLIT inside it is the whole layout rule.
    // Every listener on this page names a key through here, so the dispatch, the
    // sheet, the palette and the popups inherit one spelling of it.
    //
    // A LETTER IS A PHYSICAL KEY.  `e.code' says where a key SITS — `KeyN' is
    // the one a Latin layout writes `n' on — so a reader on Cyrillic navigates
    // with `т з о л' and archives with `в', the letters landing where the
    // fingers already are.  SHIFT IS THE UPPERCASE BINDING rather than an `S-'
    // modifier, which keeps `d' and `D' the two rows they are; a chord comes
    // through the same door, so the `C-t' completing `C-c' is the physical key
    // too.  Shift ALONE decides the case, so a held CapsLock lands on the
    // lowercase row — the safe half of that pair, `d' flagging where `D' writes.
    //
    // EVERYTHING ELSE IS THE CHARACTER, `e.key' as it always was: the named
    // keys, the function keys, and the PUNCTUATION.  `^ : + < > [ ] / , ! @'
    // sit at different positions on every layout, so there is no position to
    // bind and the character is the honest answer; a press carrying no `code' at
    // all falls back to it whole.
    //
    // THE CODE WINS ON EVERY `KeyA'–`KeyZ' PRESS, whatever the layout writes on
    // it: the map is QWERTY'S POSITIONS.  Two consequences, both named rather
    // than worked around.  A Latin layout that MOVES its letters reads its own
    // labels as this map's — an AZERTY `a' sits on `KeyQ', so it is `q' here,
    // and a Dvorak hand likewise.  And a layout spelling NO `<' or `[' (the
    // Russian one does not) cannot reach the punctuation half; the letters carry
    // movement, marks, states and the archive, and the rest wants a layout that
    // has the character.
    const LETTER = /^Key([A-Z])$/;
    // REPEAT IS DERIVED, never just read: WebKitGTK's auto-repeat can arrive
    // with `repeat' unset, which would disarm every ONCE guard at once — a
    // held `d' flagging and archiving in one press, a held DEL stripping the
    // whole query.  A keydown whose physical key is already down is a repeat
    // whatever the event says; keyup releases it; a focus loss clears the
    // set, its keyups never coming.  The answer is STAMPED on the event so
    // the four listeners that ask all read one verdict — the first caller
    // decides, and deciding twice would read its own bookkeeping as a hold.
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
    // THE MODAL SURFACES, as ONE list.  Each holds the keys with NOTHING
    // FOCUSED — the two sheets, the value palette in letter mode, and the two
    // popups, which browse on the table's own movement keys — so each would
    // otherwise leave the table's keys live underneath it, `d' included.
    //
    // TWO LAYERS RATHER THAN A STACK.  A SHEET is a WORKSPACE: it stands, holds
    // a cursor, and there are two, never both.  A MOMENTARY is raised OVER one
    // to answer a question and is answered and gone.  At most one momentary is
    // up, opening one closing whichever stood, at the DOOR rather than in the
    // listeners (`sole'); the open one holds the keys unconditionally; closing
    // it gives them back to the sheet with its cursor where it was.  A rank
    // could express none of that, the sheet being also what raises a momentary
    // over itself.
    //
    // ORDER DECIDES ONE THING, through `momentary()': `+' over the tags popup
    // leaves BOTH up, and the tie goes to the earlier entry.  Swapping them
    // hands the add field's letters to the tags listener.
    //
    // FOUR READERS: `typing()' asks whether ANY is up (killing every `table'
    // row), `live''s `modal' arm whether a WORKSPACE is, `momentary()' which
    // raised one is, and `cancel' walks the list for the surface ESC belongs to.
    // Each entry names its `up', its `off', and the OPEN EDIT that is a rung
    // under it.  NEITHER SHEET names an `off': ESC falls through to the sheet
    // ladder, where closing a workspace belongs.
    /**
     * A MODAL SURFACE, as this list declares one.  Four readers ask four
     * different questions of it, so every field but `off'/`edit'/`shut' is
     * owed by every entry — a shape stated here rather than inferred from the
     * literal, where an entry omitting one silently made the field optional
     * for every reader of the list.
     * @typedef {object} Surface
     * @property {string} name         what `momentary()' answers with.
     * @property {boolean} [momentary] raised over the sheet rather than beside it.
     * @property {() => boolean} up    is it on screen.
     * @property {() => void} [off]    close it; absent means ESC falls through.
     * @property {() => boolean} [edit] is an edit open INSIDE it.
     * @property {() => void} [shut]   close that edit and leave the surface up.
     */
    /** @type {Surface[]} */
