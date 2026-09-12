// THE DATE, ONE GRAMMAR: org's own stamp, the English phrase, the shift and the
// ghost's ink, read for INK alone -- the server resolves what travels.  A BARE
// FRAGMENT of the one script scope, so the doc pane and the table's own cells
// read one phrase and the harness's pinned clock reassigns one `dateNow'.

    // Month and day RANGE-CHECKED; the lookahead stops `32' reading as day `3'.
    const DATE = "\\d+-(?:0?[1-9]|1[0-2])-(?:0?[1-9]|[12]\\d|3[01])(?!\\d)";
    // ONE ORG STAMP, or two joined by `--' wearing the SAME bracket; kept no looser
    // than the server's wall (`settledPlanning') and no wider until
    // docs/proposals/proposed/2026-08-22-a-date-is-read-where-a-date-is-owed.md.
    const ACTIVE = `<${DATE}[^<>\\n]*>`;
    const INACTIVE = `\\[${DATE}[^\\[\\]\\n]*\\]`;
    const STAMP = new RegExp(
      `^(?:${ACTIVE}(?:--${ACTIVE})?|${INACTIVE}(?:--${INACTIVE})?)$`);
    // Everything past the head is the stamp's TAIL; declared below `DATE' (TDZ).
    const STAMP_HEAD = new RegExp(`^[<[]${DATE}(?:[ \\t]+[A-Za-z]+)?`);

    // ================================================== THE DATE, READ FOR INK
    // Wall's fourth spelling, DRIFT-PINNED over `test/fixtures/english-dates.json'.
    const DAY_MS = 86400000;
    // UTC THROUGHOUT: a local-midnight `Date' shifts a day across a DST boundary.
    // The year is set explicitly since `Date.UTC' reads 0..99 as 1900+y.
    const dnum = (c) => {
      const t = new Date(0);
      t.setUTCFullYear(c.y, c.m - 1, c.d);
      t.setUTCHours(0, 0, 0, 0);
      return Math.round(t.getTime() / DAY_MS);
    };
    const civil = (n) => {
      const t = new Date(n * DAY_MS);
      return { y: t.getUTCFullYear(), m: t.getUTCMonth() + 1, d: t.getUTCDate() };
    };
    const leapYear = (y) => (y % 4 === 0 && y % 100 !== 0) || y % 400 === 0;
    const daysInMonth = (y, m) =>
      [31, leapYear(y) ? 29 : 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31][m - 1];
    // The wall `Time.fromGregorianValid' stands for: `31 feb' never reaches disk.
    const dayReal = (c) => !!c && c.m >= 1 && c.m <= 12 && c.d >= 1 && c.d <= daysInMonth(c.y, c.m);
    const addDays = (c, n) => civil(dnum(c) + n);
    /** Is C finite and real on the calendar?  A shift off `Date''s range is `NaN'. */
     const showable = (c) => !!c
      && Number.isFinite(c.y)
      && Number.isFinite(c.m)
      && Number.isFinite(c.d)
      && dayReal(c);
    const addMonths = (c, n) => {
      const k = c.m - 1 + n;
      const y = c.y + Math.floor(k / 12);
      const m = ((k % 12) + 12) % 12 + 1;
      return { y, m, d: Math.min(c.d, daysInMonth(y, m)) };
    };
    const DOW = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"];
    const dowOf = (c) => DOW[new Date(dnum(c) * DAY_MS).getUTCDay()];
    const pad2 = (n) => (n < 10 ? "0" : "") + n;
    const isoDay = (c) => `${c.y}-${pad2(c.m)}-${pad2(c.d)}`;
    const stampOf = (c, time, inactive, tail) =>
      `${inactive ? "[" : "<"}${isoDay(c)} ${dowOf(c)}`
      + `${time ? " " + time : ""}${tail || ""}${inactive ? "]" : ">"}`;
    const MONTH_WORDS = {
      jan: 1, january: 1, feb: 2, february: 2, mar: 3, march: 3,
      apr: 4, april: 4, may: 5, jun: 6, june: 6, jul: 7, july: 7,
      aug: 8, august: 8, sep: 9, september: 9, oct: 10, october: 10,
      nov: 11, november: 11, dec: 12, december: 12,
    };
    const MONTH_LIST = Object.keys(MONTH_WORDS);
    // Offers spell a month in full: a three-letter form abbreviates its neighbour.
    const MONTH_FULL = MONTH_LIST.filter((w) => w.length > 3 || w === "may");
    const NOT_A_DATE = "not a date";
    const INVERTED = "ends before it starts";
    const NO_DATE_WHY = "not a date — try 2026-08-18, today, +3d, 18 aug,"
      + " from 18 to 19 aug, or org's own <2026-08-05 Wed>";
    const INVERTED_WHY = "ends before it starts — spell a year at each end,"
      + " as in from 30 dec 2026 to 2 jan 2027";
    // The server REPARSES CLOSED's value (`Glance.Web.Base.unreadable').
    const NOT_A_STAMP = "not a timestamp";
    const notReadBack = (key) => `${key} is not a timestamp org would read back`;
    /** The reader's ONE refusal; HOW adds `hard' (dead) or `unfinished' (typing). */
    const noDate = (how) => ({ ok: false, ...how, short: NOT_A_DATE, why: NO_DATE_WHY });
    // Two-digit month and day, org's canonical spelling; `STAMP' stays liberal.
    const dayOf = (s) => {
      const m = /^(\d+)-(\d{2})-(\d{2})$/.exec(s);
      if (!m) return null;
      const c = { y: +m[1], m: +m[2], d: +m[3] };
      return dayReal(c) ? c : false;          // `false' is spelled, no such day
    };
    /** Org's own spelling, KEPT VERBATIM once it reparses — the one form whose
     * weekday is NOT recomputed, wrong weekday and all (pinned at
     * test/TestQuery.hs:1791). */
    function verbatimDate(s) {
      if (!/^[<[]/.test(s)) return null;
      if (STAMP.test(s)) {
        // The RAW stamp rides along, so the step splices into THAT (`dateStepped').
        const m = /^[<[](\d+)-(\d{1,2})-(\d{1,2})/.exec(s);
        const c = m ? { y: +m[1], m: +m[2], d: +m[3] } : null;
        return { ok: true, bracketed: true, stamp: s,
                 start: c && dayReal(c) ? c : undefined };
      }
      // STILL BEING TYPED.  The text must END on the closer: an interval holds two.
      if (!/[>\]]$/.test(s)) return noDate({ unfinished: true });
      return { ok: false, hard: true, short: NOT_A_DATE,
               why: "that bracket is no stamp org would read back" };
    }
    /** A shift's BASE: `null' where there is none, `false' where none is real. */
    function shiftBase(t, today) {
      // ONE ROSTER with the filter's (`Glance.Query.dayWords'); `*today*' is old.
      if (t === "" || t === "today" || t === "*today*") return today;
      if (t === "tomorrow") return addDays(today, 1);
      // THE SAME DOOR AS THE BARE FORM: a second ISO regex here is drift.
      const iso = dayOf(t);
      if (iso !== null) return iso;
      return null;
    }
    function shippedDate(s, today) {
      const t = s.toLowerCase();
      const tm = /^(\d{4}-\d{1,2}-\d{1,2})[ \t]+(\d{1,2}):([0-5]\d)$/.exec(t);
      if (tm) {
        const c = dayOf(tm[1]);
        if (!c || +tm[2] > 23) return noDate({ hard: true });
        return { ok: true, start: c, time: `${pad2(+tm[2])}:${tm[3]}` };
      }
      // ONE SHIFT GRAMMAR, THE FILTER'S OWN (`shiftIn', Glance.Query), read off the
      // END, so `2026-09-15-7d' is the week before.  NO TRIM: the wall trims none.
      const sh = /^(.*)([+-])(\d+)([dwmy])$/.exec(t);
      if (sh) {
        const base = shiftBase(sh[1], today);
        if (base === null) return null;
        if (base === false) return noDate({ hard: true });
        const n = (sh[2] === "-" ? -1 : 1) * +sh[3], u = sh[4];
        return { ok: true,
                 start: u === "d" ? addDays(base, n)
                      : u === "w" ? addDays(base, 7 * n)
                      : u === "m" ? addMonths(base, n)
                      : addMonths(base, 12 * n) };
      }
      const half = /^(.*?)[+-]\d*$/.exec(t);
      if (half) {
        const under = shiftBase(half[1], today);
        if (under !== null && under !== false) return noDate({ unfinished: true });
      }
      const b = shiftBase(t, today);
      if (b === null) return null;
      if (b === false) return noDate({ hard: true });
      return { ok: true, start: b };
    }
    /** `day month [year]' or `month day [year]'; an elided year is the clock's. */
    function englishDay(w, today) {
      if (w.length < 2 || w.length > 3) return null;
      let y = null;
      if (w.length === 3) {
        if (!/^\d{4}$/.test(w[2])) return null;
        y = +w[2];
      }
      let d = null, mo = null;
      if (/^\d{1,2}$/.test(w[0]) && MONTH_WORDS[w[1]])
        { d = +w[0]; mo = MONTH_WORDS[w[1]]; }
      else if (MONTH_WORDS[w[0]] && /^\d{1,2}$/.test(w[1]))
        { mo = MONTH_WORDS[w[0]]; d = +w[1]; }
      else return null;
      const c = { y: y === null ? today.y : y, m: mo, d };
      return dayReal(c) ? { c } : { bad: true };
    }
    /** The interval's left end, a day alone or a whole date.  EACH ELIDED FIELD
     * TAKES THE RIGHT END'S, or `from 18 to 19 august 2027' spans twelve months. */
    function englishLeft(w, right) {
      if (w.length === 1) {
        if (!/^\d{1,2}$/.test(w[0])) return null;
        const c = { y: right.y, m: right.m, d: +w[0] };
        return dayReal(c) ? { c } : { bad: true };
      }
      return englishDay(w, right);
    }
    function englishDate(s, today) {
      let w = s.toLowerCase().split(/[ \t]+/).filter(Boolean);
      if (!w.length) return null;
      if (w[0] === "from") w = w.slice(1);
      const i = w.indexOf("to");
      if (i > 0 && i < w.length - 1) {
        const right = englishDay(w.slice(i + 1), today);
        if (!right) return null;
        if (right.bad) return noDate({ hard: true });
        const left = englishLeft(w.slice(0, i), right.c);
        if (!left) return null;
        if (left.bad) return noDate({ hard: true });
        const a = dnum(left.c), b = dnum(right.c);
        // WITH NO TIMES `<D>--<D>' AND `<D>' ARE ONE INTERVAL: one spelling.
        if (a === b) return { ok: true, start: left.c };
        // Refused, which keeps the current-year default statable: spell both years.
        if (a > b)
          return { ok: false, hard: true, short: INVERTED, why: INVERTED_WHY };
        return { ok: true, start: left.c, end: right.c };
      }
      const one = englishDay(w, today);
      if (!one) return null;
      if (one.bad) return noDate({ hard: true });
      return { ok: true, start: one.c };
    }
    /** Resolve the phrase inside org's brackets, wearing THE ACTIVITY THE BRACKET
     * NAMES.  THE INVERSION TRAVELS; every other refusal stays the bracket's. */
    function wrappedDate(s, today) {
      const inactive = s[0] === "[";
      if (!s.endsWith(inactive ? "]" : ">")) return null;
      const body = s.slice(1, -1).trim();
      if (!body) return null;
      const r = resolvedDate(body, today, inactive);
      if (!r) return null;
      if (!r.ok) return r.short === INVERTED ? r : null;
      return { ...r, bracketed: true };
    }
    /** Resolve PHRASE by the grammar both readings share, stamped per INACTIVE. */
    const resolvedDate = (phrase, today, inactive) => {
      const g = englishDate(phrase, today) || shippedDate(phrase, today);
      if (!g) return null;
      if (!g.ok) return g;
      if (!showable(g.start) || (g.end && !showable(g.end)))
        return noDate({ hard: true });
      const one = (c, time) => stampOf(c, time, inactive);
      return { ok: true, start: g.start, end: g.end,
               stamp: g.end ? `${one(g.start)}--${one(g.end)}`
                            : one(g.start, g.time) };
    };
    /** TEXT read as a planning date against TODAY.  A declaration, so a direct
     * `eval' of this glue reaches it: the drift pin drives it over the corpus
     * `test/fixtures/english-dates.json' the server's own reader is driven over. */
    function readsDate(text, today) {
      const s = String(text == null ? "" : text).trim();
      if (!s) return noDate();
      // ORG'S OWN SPELLING OUTRANKS THE WRAPPED READING, as `planningTimestamp' does.
      const v = verbatimDate(s);
      if (v) return v.ok || v.unfinished ? v : (wrappedDate(s, today) || v);
      return resolvedDate(s, today, false) || noDate();
    }
    /** TEXT read the way the plain stamp wall reads it, for KEY's own refusal.
     * A SECOND READER AND NEVER A SECOND GRAMMAR, and NO CLOCK: nothing resolves. */
    function readsStamp(text, key) {
      const s = String(text == null ? "" : text).trim();
      const v = s ? verbatimDate(s) : null;
      if (v && v.ok) return v;
      return { ok: false,
               ...(s && !(v && v.unfinished) ? { hard: true } : { unfinished: true }),
               short: NOT_A_STAMP, why: notReadBack(key) };
    }
    /** The reader's own day, civil, read for INK alone — the server's clock
     * decides.  A summon pins it once at open (one clock read, docs/invariants.md). */
    function dateNow() {
      const n = new Date();
      return { y: n.getFullYear(), m: n.getMonth() + 1, d: n.getDate() };
    }
    const extendsAny = (list, w) => list.some((x) => x.indexOf(w) === 0);
    // `*today*' rides along unoffered, and last: `shiftBase' reads its prefixes.
    const DATE_VOCAB = ["today", "tomorrow", "+1d", "+1w", "+2w", "+1m", "+3m",
                        "+1y", "*today*"];
    const partWriting = (p, hi) =>
      p === undefined || p === ""
      || (p.length === 1 ? +p <= Math.floor(hi / 10)
                         : p.length === 2 && +p >= 1 && +p <= hi);
    const dayAndMonthTyped = (a, b) =>
      (/^\d{1,2}$/.test(a) && !!MONTH_WORDS[b])
      || (!!MONTH_WORDS[a] && /^\d{1,2}$/.test(b));
    const yearTyped = (y) => /^\d{1,3}$/.test(y);
    /** Is TEXT still being WRITTEN?  R is the reader's own answer for TEXT. */
    function dateWriting(text, r) {
      if (r.hard) return false;
      if (r.unfinished) return true;
      const t = String(text).trim().toLowerCase();
      if (!t) return false;
      // `2026-08-1' is on the way to the 18th; `2026-8' is a refusal already.
      const iso = /^(\d*)(?:-(\d*)(?:-(\d*))?)?$/.exec(t);
      if (iso) {
        const yy = iso[1], mm = iso[2], dd = iso[3];
        const whole = !!yy && mm?.length === 2 && dd?.length === 2;
        if (!whole && partWriting(mm, 12) && partWriting(dd, 31)) return true;
      }
      const tw = /^(\d+-\d{2}-\d{2})[ \t]+(\d{0,2})(:(\d{0,2}))?$/.exec(t);
      if (tw && dayOf(tw[1])) {
        const hh = tw[2], mm = tw[4] || "";
        if (hh.length === 2 && +hh > 23) return false;
        if (tw[3] === undefined) return true;      // no colon typed yet
        return mm.length < 2 ? mm.length === 0 || +mm <= 5 : +mm <= 59;
      }
      if (extendsAny(DATE_VOCAB, t)) return true;
      if ("from".indexOf(t) === 0) return true;
      const w = t.replace(/^from[ \t]+/, "").split(/[ \t]+/);
      const to = w.indexOf("to");
      if (to > 0) {
        const right = w.slice(to + 1);
        if (!right.length) return true;
        if (right.length === 1
            && (/^\d{1,2}$/.test(right[0]) || extendsAny(MONTH_LIST, right[0])))
          return true;
        if (right.length === 2)
          return (/^\d{1,2}$/.test(right[0]) && extendsAny(MONTH_LIST, right[1]))
            || (!!MONTH_WORDS[right[0]] && /^\d{1,2}$/.test(right[1]));
        return right.length === 3 && dayAndMonthTyped(right[0], right[1])
          && yearTyped(right[2]);
      }
      const last = w[w.length - 1];
      if (w.length > 1 && last !== "" && "to".indexOf(last) === 0) {
        const left = w.slice(0, -1);
        if ((left.length === 1 && /^\d{1,2}$/.test(left[0]))
            || (left.length === 2 && dayAndMonthTyped(left[0], left[1]))
            || (left.length === 3 && dayAndMonthTyped(left[0], left[1])
                && /^\d{4}$/.test(left[2])))
          return true;
      }
      if (w.length === 1)
        return /^\d{1,2}$/.test(w[0]) || extendsAny(MONTH_LIST, w[0]);
      if (w.length === 2 && /^\d{1,2}$/.test(w[0]))
        return extendsAny(MONTH_LIST, w[1]);
      // FOUR DIGITS ARE NO LONGER WRITING: `18 aug 1899' must show its answer.
      if (w.length === 3 && dayAndMonthTyped(w[0], w[1])) return yearTyped(w[2]);
      return false;
    }
    /** WHAT THE GHOST SAYS, or `""' for nothing.  READ is the caller's answer. */
    function dateGhost(text, today, read) {
      const t = String(text == null ? "" : text).trim();
      if (!t) return { text: "", bad: false };
      const r = read || readsDate(t, today);
      if (!r.ok)
        return dateWriting(t, r) ? { text: "", bad: false }
                                 : { text: ` ✗ ${r.short}`, bad: true };
      if (!r.stamp || r.stamp === t) return { text: "", bad: false };
      return { text: ` → ${r.stamp}`, bad: false };
    }

    // ============================================== THE STEP, OVER THE STAMP
    const dateStep = (k) =>
      k === "S-<right>" ? 1 : k === "S-<left>" ? -1
      : k === "S-<down>" ? 7 : k === "S-<up>" ? -7 : 0;
    const dateStepped = (r, to) => {
      if (!r.bracketed) return isoDay(to);
      const stood = r.stamp;
      const head = stood.indexOf("--") === -1 && STAMP_HEAD.exec(stood);
      // The tail excludes the closing bracket: the one org-stamp writer adds it.
      return stampOf(to, null, stood.charAt(0) === "[",
                    head ? stood.slice(head[0].length, -1) : "");
    };
