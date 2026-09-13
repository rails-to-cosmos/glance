    // THE TAG VOCABULARY AND ITS OFFERS.  A TAG COMPLETES THE WAY A DATE DOES:
    // the very menu the date box carries -- the list under the field, the hint
    // column, the arrows walking it and `TAB' taking what point stands on
    // (`wmenu', 20-sheet.js).  TWO SURFACES STAND IN NO BOX OF THEIR OWN -- the
    // draft's tag CELL, which is the renderer's in-cell input, and the tags
    // popup's rename field -- so ONE menu element hangs at the page's root and is
    // placed under whichever rect the surface names.

    /** THE STORE'S OWN TAGS, asked ONCE and kept until the store settles again.
     * `GET /tags?vocabulary=true' NAMES NO ROW, so it answers for the whole tree
     * rather than for the rows this view draws -- which is the point: a capture
     * files a row under a tag the view need not show.  The renderer's own
     * `tagVocab()' stays the FILTER's (assets/table-view.js) and is a different
     * question: a filter completes over what the view can narrow TO, and a tag no
     * row here wears narrows to nothing. */
    let tpool = null, tpoolFresh = false, tpoolAsking = false;
    /** The store moved, so the next OPEN asks again.  The held answer keeps
     * drawing meanwhile: a menu is never re-asked out from under the reader. */
    function staleTagVocab() { tpoolFresh = false; }
    function askTagVocab() {
      if (tpoolFresh || tpoolAsking) return;
      tpoolAsking = true;
      getJSON("/tags?vocabulary=true")
        .then((a) => {
          tpool = { list: a.vocabulary || [], counts: a.counts || {} };
          tpoolFresh = true;
          tpoolAsking = false;
          drawTagOffers();
        })
        .catch(() => { tpoolAsking = false; });
    }
    const tagPool = () => tpool || { list: [], counts: {} };

    /** THE WORD THE CARET SITS IN inside TEXT's tag run, and its bounds.  A RUN
     * IS COLON-DELIMITED -- how `cellTags' reads one and how the draft's seed
     * writes one (`draftTags') -- so the word is what stands between the colon
     * behind the caret and the one ahead of it. */
    function tagWordAt(text, at) {
      const s = String(text == null ? "" : text);
      const i = Math.max(0, Math.min(at === undefined ? s.length : at, s.length));
      const from = i === 0 ? 0 : s.lastIndexOf(":", i - 1) + 1;
      const ahead = s.indexOf(":", i);
      const to = ahead === -1 ? s.length : ahead;
      return { from, to, word: s.slice(from, to) };
    }
    /** TEXT with the caret's word swapped for WORD, THE RUN'S OWN SPELLING KEPT:
     * the run stays colon-delimited, so the take opens the word with a `:' where
     * none stood behind it and always closes it with one -- `:a:b' and `book'
     * make `:a:book:' -- and the caret rests after that colon, where the next tag
     * is typed.  A DECLARATION, like `tagWordAt' and `tagOffers': the suite drives
     * the three over vectors through a direct `eval' of this glue, which a
     * `const' does not leak.  @returns {[string, number]} the value, and the caret. */
    function tagRunTake(text, at, word) {
      const s = String(text == null ? "" : text);
      const { from, to } = tagWordAt(s, at);
      const head = s.slice(0, from), tail = s.slice(to);
      const lead = head + (head.slice(-1) === ":" ? "" : ":") + word + ":";
      return [lead + (tail.slice(0, 1) === ":" ? tail.slice(1) : tail), lead.length];
    }

    /** WHAT WORD OFFERS out of VOCAB, each hinted with the rows wearing it.  The
     * reader's own line LEADS where the tree spells no such tag, hinted `new',
     * and a word the tree DOES spell coincides with its own entry -- the pair
     * box's rule and the tag palette's, asked of the WHOLE vocabulary before the
     * cap so a real tag never draws itself twice.  The narrow is the SUBSTRING
     * the palette narrows by, ranked by the rows behind each. */
    function tagOffers(word, vocab) {
      const typed = String(word == null ? "" : word).trim();
      const want = typed.toLowerCase();
      const list = vocab.list || [], counts = vocab.counts || {};
      const fits = list.filter((w) => w.toLowerCase().includes(want))
        .sort((a, c) => (counts[c] || 0) - (counts[a] || 0)
                     || (a < c ? -1 : a > c ? 1 : 0));
      const minted = leadTyped(typed, list);
      const folds = minted || !want ? null
        : fits.find((w) => w.toLowerCase() === want);
      const dress = (w) =>
        ({ word: w, hint: counts[w] === undefined ? "" : String(counts[w]) });
      const shown = fits.filter((w) => w !== folds).slice(0, OFFERS);
      return (minted ? [{ word: typed, hint: NEW_HINT }]
              : folds ? [dress(folds)] : []).concat(shown.map(dress));
    }

    const tmenu = { box: "toffer", list: [], at: -1 };   // `-1' is point on NO offer
    /** WHICH SURFACE THE OFFERS STAND UNDER, or null.  FIELD and RECT are ASKED
     * rather than held: the draft's in-cell input is rebuilt by every repaint
     * (`resumeEditor', assets/table-view.js) and a held reference would dangle
     * across a store settle.  RUN says the field holds a tag run rather than one
     * tag, which is the whole difference between the two surfaces; LISTEN wires
     * THIS open's field for `input', a field the page owns being wired once at
     * boot instead.
     * @type {{field: () => any, rect: () => any, run: boolean,
     *         listen?: boolean}|null} */
    let toffering = null;
    /** THE WORD THE DRAWN LIST ANSWERS FOR.  A cell's redraw is a keystroke
     * behind the key that moved the text (`draftKey', 35-draft.js), and a take
     * off a list the reader never saw would write a word nothing offered. */
    let tword = null;
    const tagOffering = () => !!toffering && !!toffering.field();
    /** THE CARET IS `selectionEnd': the open lays a WHOLE selection down, so the
     * word the reader means there is the one the run ENDS on rather than the one
     * it starts with. */
    const caretIn = (f) =>
      (typeof f.selectionEnd === "number" ? f.selectionEnd : String(f.value).length);
    /** The word this surface is completing: a run's own, else the whole field. */
    const tagWordIn = (f) =>
      (toffering.run ? tagWordAt(f.value, caretIn(f)).word : String(f.value).trim());
    function openTagOffers(o) {
      toffering = o;
      // THE FIELD'S OWN `input' REDRAWS SYNCHRONOUSLY, so the list a `TAB' reads
      // is the one the text asks for.  A field this page owns is wired once at
      // boot; a cell's input is built fresh per open and wired here.
      if (o.listen) { const f = o.field(); if (f) f.addEventListener("input", drawTagOffers); }
      askTagVocab();
      drawTagOffers();
    }
    function shutTagOffers() {
      toffering = null;
      tword = null;
      tmenu.list = [];
      tmenu.at = -1;
      paintOffers(tmenu.box, [], -1);
    }
    /** The offers over the word the caret sits in, painted and then placed.
     * POINT STANDS ON THE READER'S OWN LINE where anything is typed and on NO
     * OFFER where nothing is, which is `dateMoved''s own rule. */
    function drawTagOffers() {
      if (!tagOffering()) return;
      const word = tagWordIn(toffering.field());
      tword = word;
      tmenu.list = tagOffers(word, tagPool());
      tmenu.at = word ? 0 : -1;
      menuPaint(tmenu);
      placeMenu(tmenu, toffering.rect());
    }
    /** THE OFFER THAT STANDS, taken the way the surface spells one: a RUN keeps
     * its colons, a one-tag field simply becomes the word.  THE READER'S OWN
     * LINE IS NOTHING TO TAKE -- it IS the word standing there -- so `TAB' over
     * it falls through to the ring and `RET' to the commit. */
    const takeTagOffer = () => {
      if (!tagOffering()) return false;
      const f = toffering.field();
      // THE READER WALKED THE LIST THE WORD ASKED FOR: a word that has outrun
      // it redraws instead, and the press stays the surface's own.
      if (tagWordIn(f) !== tword) { drawTagOffers(); return false; }
      // FOLDED, because a tag's PRESENCE is (`foldTag'): an offer differing from
      // what stands there only in case is the word already standing there.
      const stood = foldTag(tagWordIn(f));
      return menuTook(tmenu, f, (w) =>
        (foldTag(w) === stood ? null
         : toffering.run ? tagRunTake(f.value, caretIn(f), w) : [w, w.length]),
        drawTagOffers);
    };
    /** THE KEYS THE MENU CLAIMS wherever it stands, and whether it spent one: the
     * walk, and the take.  `ESC' IS NEVER ONE OF THEM -- the date box's own rule
     * -- so the surface goes down whole and the menu with it. */
    function tagOfferKey(k) {
      if (!tagOffering()) return false;
      const step = walkStep(k);
      if (step) { menuWalk(tmenu, step); return true; }
      return (k === "TAB" || k === "RET") && takeTagOffer();
    }
    // A FIELD THIS PAGE OWNS REDRAWS ON ITS OWN `input'; the draft's cell is the
    // renderer's, and redraws a keystroke behind (`draftKey', 35-draft.js).
    el("tname").addEventListener("input", drawTagOffers);
    // AND THE MENU FOLLOWS ITS ANCHOR, the way the date box does: every scroller
    // in the capture phase, since `scroll' does not bubble.
    const anchorMoved = () =>
      { if (tagOffering()) placeMenu(tmenu, toffering.rect()); };
    window.addEventListener("resize", anchorMoved);
    document.addEventListener("scroll", anchorMoved, true);
