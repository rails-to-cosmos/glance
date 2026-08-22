# Bug — an open completion swallows the typed value

**Status:** fixed · **Reported:** 2026-08-22 (a tag typed into the tags popup
landed as another tag) · **Surface:** the value palette in its typing mode
(add-a-tag, the kind picker) and the sheet's inline pair box · **Fixed in:**
`frontend/glue/00-core.js`, `frontend/glue/20-sheet.js`,
`frontend/glue/30-capture.js`, `frontend/glue/40-popups.js`,
`frontend/glue/50-settings.js`, `frontend/glue/60-refer.js`,
`frontend/glue/70-shell.js`

## Symptom

Four fields complete over a vocabulary the reader may add to, and each of
them could commit a word the reader never typed. The field narrows to what
the typed text is a substring of, point lands on the first match, and `RET`
takes the entry under point — so a typed value that is a *prefix* of a
vocabulary entry is replaced by that entry on accept:

- **the tags popup's `+`** — a tree holding `bookshelf`, `shelf` typed, `RET`
  pressed: `add-tag` goes out carrying `bookshelf`. The tag the reader named
  is never written, and the popup shows a tag they did not ask for;
- **the pair box's key half** — `OW` typed against a tree spelling `OWNER`:
  `:` completes the key to `OWNER` and the pair is written under it;
- **the pair box's value half** — `ad` typed under `OWNER` with `ada` in the
  tree: `RET` fills the field with `ada` rather than applying, and there is no
  way back to `ad`, since the walk clamps at index 0 and index 0 was the
  tree's word;
- **the kind picker** — the same widget as the tags popup, so the same rule.

A word with **no** match had a second, invisible door: `freely()` read the
input box and made an entry out of it, so a zero-match field committed the
typed line and a one-match field committed the match. Which of the two a
press meant was decided by how many rows happened to be drawn.

The empty field carried the same door's other edge: `RET` over an empty
`wider` field took `shown[0]`, a tag the reader had never named.

## Steps to reproduce

Serve a tree with a headline tagged `:bookshelf:` and another with neither
that tag nor `:shelf:`.

1. Put point on the untagged row and press `:` for the tags popup, then `+`.
2. Type `shelf`. One row draws, `bookshelf`, with point on it.
3. Press `RET`. `POST /command` carries `{"name":"add-tag","args":{"tag":
   "bookshelf"}}` and the row is tagged `bookshelf`.

For the pair box: open the sheet over a headline whose tree spells `:OWNER:`,
walk to the drawer, press `+`, type `OW` and press `:`. The key field reads
`OWNER`.

## Evidence

- `frontend/glue/30-capture.js:325` (`narrowTo`) — the shown list was the
  filtered vocabulary alone: `prompting.shown = prompting.choices.filter(...)`,
  with `prompting.at = 0`. Nothing in the list stood for what was typed.
- `frontend/glue/70-shell.js:267` — `takeChoice(promptNow().shown[promptNow()
  .at] || freely())`. The fallback ran only when the list was empty, so the
  typed line reached a commit exactly when it had no match.
- `frontend/glue/30-capture.js:346` (`freely`) — the second door, gated on
  `prompting.wider` alone, which is set by `askFrom` and by nothing else. The
  open/closed split was therefore implied by "was this palette given a list",
  which the closed template-code caller also satisfies.
- `frontend/glue/20-sheet.js:469` (`offersFor`) — the offers were the
  vocabulary's words and org's three planning words; the typed text appeared
  in none of them. `takeOffer` (`:531`) guards on
  `want === box.value.trim()`, so a partial word always differed from the
  offer and was always overwritten.
- `frontend/glue/20-sheet.js:525` (`walkOffer`) — `Math.max(0, ...)` clamps at
  the head of the offers, which is why the value half could not return to what
  was typed once point had left it.

## Fix

One law, spelled once and drawn on every open surface — AGENTS.hs, "THE TYPED
VALUE IS ALWAYS AN OFFER where a field's VOCABULARY IS OPEN".

The typed value is drawn as its own **leading** entry, hinted `new` in the
slot the pair box's `planning` hint already rides, so `RET` commits the word
the reader spelled and a match is one `C-n` away. An empty field offers no
literal, and a typed value that case-folds to an entry coincides with it: one
entry drawn, never two.

A commit therefore has one source — the entry point rests on — and
`freely()` goes with the `|| freely()` fallback that called it. The empty
field's footgun goes with them: with nothing typed there is no literal and
`shown[at]` is what it always was.

Openness is now spelled at the call rather than read off the list.
`askFrom` takes a `vocabulary` argument, `"open"` or `"closed"`, typed as a
string union so `make check-glue` refuses a third word:

- `40-popups.js` add-a-tag — `"open"`, a tag the tree has never held is a tag;
- `60-refer.js` kind — `"open"`, which is what the foot already promised
  ("a kind not listed is just typed");
- `50-settings.js` capture-template code — `"closed"`, since the codes are the
  server's list and the completion cannot offer one the expansion does not
  know;
- the state palette raises through `ask` rather than `askFrom`, so it stays
  closed and `*empty*` keeps `DEL`.

The pair box gets the same leading literal in `offersFor`, above the `OFFERS`
cap, with the planning words still riding last. The value half's clamp is
fixed by the same prepend: index 0 is the typed line, so `C-p` returns to it.

The word `new` is spelled once, as `NEW_HINT` in `frontend/glue/00-core.js`,
so the palette and the pair box cannot drift apart on what an open field calls
the reader's own line.
