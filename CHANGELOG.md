# Changelog for `glance`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

Versions 0.1 through 0.4 were cut retroactively over one dense build: each
section groups a feature arc, and its date is that arc's last commit.

## Unreleased

### Added

- **`set-planning` writes `CLOSED`, verbatim or not at all.** The command
  refused org's third planning word outright; it now takes all three, and the
  keyword picks which wall the value meets. `SCHEDULED` and `DEADLINE` are
  composed for — the whole date grammar in, the bytes org itself would write
  out — while `CLOSED` is org's own bookkeeping and is never resolved for: a
  timestamp that reparses lands byte for byte, and every other spelling is a
  400 saying `CLOSED is not a timestamp org would read back`. `POST /headline`
  already split this way and both doors now read one function
  (`Glance.Web.Base.plannedValue`), so a value one takes is a value the other
  takes. A null date clears the entry as it does for the other two, and a
  keyword naming no planning entry is refused ahead of the date — an unknown
  key outranks every value. What this buys the reader: the date widget over a
  `CLOSED` entry commits through the same door as every other entry, and the
  refusal it draws in the field is the server's own sentence.

- **`C-c C-s` reads English, and shows you what it read before it writes it.**
  In the material document `C-c C-s` and `C-c C-d` raised a blind text box with
  nothing in it; they now open a FIELD in the value's own slot — the planning
  line's SCHEDULED or DEADLINE value, the line drawn in where the row has none
  — wearing the pane's own editing dress, and the resolution rides after what
  was typed as ghost on that same line: `10 jan → <2026-01-10 Sat>` in the mute
  ink, the weekday computed. A refusal no further character can rescue shows
  its short word in the marked ink instead, and an empty field, a term still
  being written and a value that already spells its own answer show nothing at
  all — a refusal flashed at every keystroke is a refusal nobody reads. The
  standing value opens WHOLLY SELECTED, so one keystroke replaces it and `RET`
  with none recommits it byte for byte, the default `org-read-date` takes;
  `RET` over a standing offer takes the offer and writes nothing, and over the
  finished value it applies; `ESC` leaves the sheet the bytes it was, the
  ghosted-in planning line included; an empty value clears the entry, as it
  always did; `S-←`/`S-→` adjust a day and `S-↑`/`S-↓` a week in place, the
  ghost following. A drawer pair whose key case-folds to `scheduled` or
  `deadline` already routes to the planning line, and its value half wears the
  same field and the same ghost — one widget, both doors. **And the grammar
  under it reads English dates.** `18 aug`, `aug 18` and `18 August 2029` are
  timestamps wherever a date is owed, and `from 18 to 19 august` — `from`
  optional, `to` not — is org's own `--` pair with the weekday computed at both
  ends, the left end inheriting the month and year it elides. The year is the
  clock's, flat, so `18 aug` typed in December is that August and a typist
  meaning the next one writes the year; the month word folds totally
  (`August ≡ august ≡ AUGUST`) over an exact table of twelve short forms and
  twelve full ones; a year is four digits or absent; a bare day, a bare month,
  an ordinal and every separator but whitespace are refused rather than guessed
  at, and so is a weekday word — `Thu 18 aug` even when the weekday is right,
  the weekday being computed here and never read; an interval whose end falls
  before its start is refused in those words, spelling both years the remedy;
  and `from 18 to 18 aug` collapses to the single stamp `18 aug` writes. Org's
  own bracket is still kept verbatim, wrong weekday and all. `set-planning`
  takes every one of these over `POST /command`, since the field and the door
  read ONE grammar: what the reader types travels as typed, the server
  resolves it once against its own clock, and the pane redraws off that
  answer — the page spells no org, and the field's own resolver is a preview
  that writes nothing. `docs/commands.md` carries the law.

- **Dates compare, and `*today*` is one of them.** `scheduled:`, `deadline:`
  and `planned:` now take a comparison in the value position: `>=`, `<=`, `>`
  and `<` at the head of the value, longest spelling read first, with the range
  `A..B` beside them. The date is the literal the bare form always took — any
  prefix of an ISO stamp, so `2026-08` is a month and `2026-08-0` a month's
  first nine days — and the operator says which end of that interval it cuts
  at: `<` and `>=` cut at the first instant, `<=` and `>` at the last. No date
  arithmetic happens anywhere, and the bare form is the two inclusives at once,
  `deadline:2026-09` ≡ `deadline:>=2026-09 deadline:<=2026-09`. `*today*` joins
  the starred family as a DATE rather than a whole value, legal wherever a
  literal stands — `scheduled:*today*` is the day itself, `deadline:<*today*`
  the overdue — and it resolves once per request against the server's local
  day. Two guards hold as laws: the empty cell sits outside every comparison,
  so an undated row passes none of the four and `+deadline:*empty*` is how
  those rows come back; and negation is no mirror, `-scheduled:<*today*`
  carrying the undated rows that `scheduled:>=*today*` leaves out. On
  `planned:` the range says what two tokens cannot, ONE date cell inside the
  interval, where `planned:>=A planned:<=B` lets one cell answer each token.
  The operator is read on the three timestamp keys alone, `title:>x` staying
  the substring it always was, and a query with no comparison in it means
  exactly what it meant before. `docs/query.md` carries the law.

- **A date can be shifted, so the agenda gets its lookahead.** A date value now
  takes a shift — `BASE+N UNIT` or `BASE-N UNIT`, written without spaces, the
  base a spelled date, `*today*` or nothing at all, `N` a run of digits and the
  unit one of org's own `d`, `w`, `m`, `y`. `scheduled:<=*today*+30d` is the next thirty
  days, `deadline:>=*today*-7d` the last week, and `planned:*today*..*today*+30d`
  the thirty-day agenda in one token — the line a tree pins into
  `#+GLANCE_AGENDA_FILTER` and `A` reads as the day it is pressed. The quoted
  value form is that same token with room to breathe:
  `scheduled:"<= *today* + 30 days"` takes spaces beside the operator, the range
  mark, the sign and the unit, and spells the unit as a word, case-folded like
  every value; a pre-pass folds it onto the compact spelling, so there is one
  parser and one law. **The shift resolves at compile to a plain day literal** —
  once per request and never per row, the request's own day for `*today*`, the
  spelled date otherwise, a week seven days, months and years calendar-clipped
  so Jan 31 `+1m` is Feb's last day rather than March 3 — and after that every
  law already written applies untouched: the granularity cuts, the empty cell
  outside every comparison, negation no mirror, the alternatives, the signs.
  Both ends of a range take a shift and either may be plain
  (`scheduled:*today*-7d..*today*+7d` is the fortnight around today); the base
  may be dropped and a bare `+30d` reads today-relative, the way `set-planning`
  already reads one, which leaves the token's own sign where it always was — in
  `+scheduled:+30d` the first `+` widens the axis and the value's own `+` is the
  shift's. `*today*` shifted with no
  clock behind it still names no day, and a half-typed `*today*+` or `*today*+30`
  narrows nothing unsigned and empties the table negated, as every half-typed
  token does. A shifted value is one more spelling of a day literal and never a
  new kind of value, so every query that composed before answers as it did.

- **`+` in the drawer asks inline, as the pair it will become.** Adding a
  property raised two prompts over the sheet, one for the key and one for the
  value. `+` now draws a fresh row at the end of the properties drawer —
  opening the drawer to show it — and lays a two-field box over that row,
  wearing the drawer's own colons, so what is typed reads as the `:KEY: value`
  line it is about to be. `:`, `TAB` or `RET` hands the key over to its value;
  `TAB` or `RET` from the value writes the pair at once. Both halves complete
  from the tree's own property vocabulary — every key the tree spells and every
  value under the key beside it, commonest first — with the arrows and
  `C-n`/`C-p` walking the offers. `ESC` cancels the input whole: the box goes,
  the drawn row with it, and the drawer is the bytes it was. A key the drawer's
  frame owns (`PROPERTIES`, `END`) or one the store owns (`ORG_GLANCE_ID`,
  `ORG_GLANCE_CREATION_TIME`) is refused at the field, and the box stays up with
  what was typed still in it.

- **A planning word typed as a property key lands on the planning line.** A
  drawer key that case-folds to `scheduled`, `deadline` or `closed` is a
  planning entry wearing a property's clothes, and the sheet writes it as one.
  Typed into the inline pair box it is set or replaced on the planning line —
  upcased, where the planning composer puts it — and the drawer is left the
  bytes it was. `RET` on a pair another writer already minted into a drawer
  MIGRATES it: the drawer entry off and the planning entry set on the same
  single write, since one commit carries both lists. `CLOSED` is one of the
  three, and the bracket kind is the typist's — `<2026-09-01 Tue>` and
  `[2026-09-02 Wed 18:30]` are both written as spelled. The value meets the
  server's own planning wall before anything is sent — one org stamp, or two
  joined by `--` and wearing the SAME bracket, each reading its month and day
  as org's parser does, so `<2026-8-1 Sat>` is a date and `<2026-13-45 Foo>` is
  not — and a value org would not read back is refused at the field in the
  wall's own words (`SCHEDULED is not a timestamp org would read back`) with the
  box still up, what was typed still in it, and the drawer and the planning line
  byte-identical. `RET` over a stray `:SCHEDULED:` line meets that same wall at
  that door, since committing it routes the pair the same way. The key half's completion offers the three
  beside the tree's own vocabulary, upcased and hinted `planning`, since `GET
  /properties` reads DRAWERS and the parser lifts planning off the headline
  before one is read; accepting an offer flows the usual `:`/`TAB` advance. An
  emptied value clears the entry, org's own way.

- **`/` filters, `.` composes the whole query.** `/` (`filter-rows`) edits the
  FILTER half alone: its completion offers the narrowing keys — the six
  columns, `planned`, `ref`, `substring` — and a shaping token typed into it is
  refused on commit, never chipped, never part of what is served, left standing
  in the box with an echo naming the other door. The `+` sign, the `*word*`
  metas and every value stage work there as they do everywhere, and a standing
  `sort:`/`columns:` rides along untouched, so narrowing never loses the order.
  `.` — `compose-query`, on a key that was unbound — opens the same box on the
  whole expression, filters and `sort:`/`columns:`/`view:` together, which is
  what `/` opened before. One `?q=` underneath both: `/` is a restricted VIEW
  over the one query, never a second query.

- **`+key:value` widens its own axis.** A query token opening with `+` joins
  its key's axis as an alternative: inside one axis the plain and negated
  tokens AND as before and the `+` tokens OR against that conjunction, while
  the axes still AND with each other — so an addition widens one filter and
  leaves every other one standing. `priority:[#A] tag:book +priority:[#B]` is
  the book rows at A or B, and means that however the three are interleaved:
  grouping is BY KEY, never by adjacency. Every narrowing key takes the sign,
  bare free text included (`milk +bread` carries either word); alternatives and
  metas ride along (`+state:DONE|CANCELLED`, `state:TODO +state:*inactive*`);
  a `+` alone on its axis is the plain token, `+state:DONE` ≡ `state:DONE`. A
  query with no `+` in it means exactly what it meant before. A half-typed
  token adds nothing and establishes no axis wherever it stands — `+state:` on
  a widened axis, and the plain `state:` beside it, so `state: +state:DONE` is
  the DONE rows rather than every row — and the table never empties on one; a
  lone `+` is that same non-event where a lone `-` empties the table. In a URL
  the sign travels percent-encoded, a bare `+` in a query string decoding to a
  space: `?q=state%3ATODO%20%2Bstate%3ADONE`. The shaping keys refuse the sign
  the way they refuse a negation — `+sort:…`, `+columns:…` and `+view:…` are
  400s naming the token — order and shape never narrowing, they have nothing to
  widen. `docs/query.md` carries the law.

- **Completion behind a `+` skips what the axis already carries.** With
  `priority:[#A]` standing, `+priority:` offers B and C and never A: re-adding
  a carried value is `A ∨ A`, a dead offer. Every alternative of every other
  token on that key counts, chips and half-typed box alike, whatever its sign,
  and the comparison is the column's own reading — `[#A]`, `A` and `a` are one
  value. The plain and negated value stages narrow and are untouched: they
  still offer the whole domain.

- **A headline walks headlines, and `p` is headline-sized past a body's edge.**
  `n`/`p` from a child headline step through every visible headline in document
  order, at every depth — org's own next-visible-heading — and a folded subtree
  is skipped whole. Contents stay behind `f`/`b`. The root is the reader's
  exception: `n` from the entry's own line steps into its contents, and `p`
  from the first content row climbs back. Inside a body `p` is the element step
  it always was; where the body has nothing above it — its first element, the
  tail past every subtree — it lands on the nearest visible headline above
  point, org's own previous-visible-heading, where the press used to leave
  point standing. One press from the tail is the document's last headline, and
  a folded subtree is its line alone. A run's items keep their list's edge, and
  `n` is untouched.

- **"open link" opens the material document.** A `glance:` or `org-glance-*:`
  link names a headline, and following it lands in that headline's material
  doc instead of a browser tab. In the links popup `RET` and `o` open the
  link; `e` edits it.

- **The boot speaks its configuration.** The log panel opens on
  `loading … view: X`, and a boot with no query in the address bar applies
  the tree's default view to the table and logs the config it read — layers,
  saved views, keyword hues.

- **The pane ends on an empty line.** One empty row past everything, hidden
  until the walk reaches it; `RET` there writes a fresh paragraph at the
  document's end —
  the door `+` cannot offer when the body ends in something it owns, a
  properties drawer above all. Only ever the one line; it has no span, so
  flag and delete refuse it, and the headline walk ends on it.

- **`o` opens a headline's reach.** On a headline, `o` gathers the links of
  its whole subtree — the root's reach is the entry — where before it read
  the headline's own line alone and a child's `o` opened nothing.

- **User docs for the query language, capture, commands and configuration** —
  `docs/query.md`, `docs/capture.md`, `docs/commands.md`, `docs/config.md`;
  the README keeps a crib of each and points there.

- **The `:PROPERTIES:` drawer and the planning line are in the document.** The
  sheet's separate Key/Value panel is gone; the pane draws the planning line and
  the drawer under the headline, org's own spelling, keys and frame in the
  reserved-token ink. The drawer opens folded to `:PROPERTIES:…` and `TAB`
  toggles it, as in org; `f` into a folded drawer opens it, and one the reader
  opened stays open. `RET` edits a pair as its `:KEY: value` line, `+` adds one
  (see above), `d d` drops a pair. Hidden properties stay hidden and survive
  every write.

- **Child headlines are drawn whole, inside the document.** A child's own
  paragraphs, lists and drawers appear under its headline instead of behind
  `RET`; a grandchild under it, all the way down. Navigation is the list's own:
  `n`/`p` step siblings over whole subtrees, `f` enters a child's contents,
  `b` climbs back out. A shelf indents under its own headline's first letter —
  the cleaned stars' own width, so the columns never cross at any depth.
  Editing a child's paragraph
  writes through the same splice as the entry's own; `RET` on the child still
  materializes into it.

- **The doc pane draws the tree instead of org's own bullets.** A list item wore
  two marks at once — the connector the pane draws and the `-` the file spells —
  a cell apart at the same height, reading as one dashed run. An unordered
  bullet (`-`, `+`, `*`) now steps aside wherever a connector is drawn, keeping
  its column so no text moves and the line still copies as org wrote it. An
  ordinal (`1.`, `1)`) and a checkbox are content and always paint. The look is
  remembered under `glance-bullets`: store `shown` to get the bullets back.

- **A reference reads from both ends, and the edge's kind is a predicate.**
  `ref:ID` has always served the rows pointing AT a row; **`from:ID` now serves
  the rows that row points at.** Its own links resolve through the same two
  namespaces `ref:` already reads — every row's id and title for a
  `[[glance:…]]` or `[[Title]]` link, the `:ID:` properties for an `[[id:…]]`
  one — so a link naming no row, a `https:` bookmark or a `file:` attachment,
  is no reference and brings nothing back. Both keys take **`?kind=SLUG`**, and
  they spell it the way the FILE does: org-glance writes
  `[[glance:ID?kind=SLUG][…]]`, so `ref:ID?kind=blocked-by` narrows to the edges
  carrying that kind and the peer's own slug — downcased, whitespace runs folded
  to one `-` — is applied on the write and on the read, making `?kind=Blocked
  By` and `?kind=blocked-by` one kind. The value cuts at the first `?` the way a
  link target does, one reading and one parser — and the cut is taken only where
  a `kind=` comes out of it, so an id carrying a `?` that declares no kind stays
  whole and resolves to the row it always did, and a title's own `?` is text for
  the same reason. **The bare forms stay kind-blind**, which is what keeps
  every `ref:` query written before kinds meaning what it meant. **`*any*` joins the starred
  family** as its seventh member, standing where a row id stands and reading as
  the union over that slot: `ref:*any*` serves exactly the rows some `ref:ID`
  serves — those carrying a reference — `from:*any*` the rows something points
  at, and `-from:*any*` the orphans, which the census puts at 91.2% of the
  corpus. The two laws follow rather than being added: a row is neither its own
  reference nor its own from-target, so a self-link alone answers neither, and
  an unresolvable anchor serves nothing, so a link naming no row counts towards
  neither. A reference value is the one value that is not case-folded, stars
  included — `ref:*ANY*` names no row and matches none, the way `ref:ALPHA`
  does. `ref` and `from` are different relations and so are **two axes**: two
  tokens on one key AND, a `+` widens the axis it names and no other, and the
  two AND with each other, token order carrying nothing as everywhere else. The
  edge map behind `*any*` is built at most once per request and bound lazily, so
  a query naming no `*any*` forces it not at all. Every query that served rows
  still serves them byte for byte — `?kind=` inside a `ref:` value named no row
  and served nothing — and `from:` is the one cost, paid once, a spelling coming
  out of free text: a token that used to search for the literal `from:x` now
  names a relation, and quoting it, `"from:x"`, searches it again.
  `docs/query.md` carries the law.

### Changed

- **The query box docks on the chip strip.** `/` and `.` raise it onto the
  strip's own row, beside the chips it is about to add to; before, they raised a
  centred palette over a veiled table. The veil is gone from the main table —
  the rows stay lit and in place while a query is typed, the table keeps its
  full height and its sort arrows, and the hint line and pager stay where they
  were. What the box does is untouched: two ESCs (the typed text first, the box
  second), a DEL over an empty box that reaches nothing behind it, and a filter
  applied on commit alone. `/` still opens the filter half and `.` the whole
  expression. The relations picker keeps its own inline box and its one-step
  ESC.

- **The project is under the Apache License 2.0.** It was BSD-3-Clause. The new
  terms add an explicit patent grant and ask that changed files be marked. The
  vendored `assets/table-view.js` stays MIT under the same author.

- **The cursor in the doc pane wears the table's own highlight.** Point marked
  its line with a connector alone; the stop under it now takes `--g-sel`, the
  ground a selected table row wears, so one cursor reads the same on both
  surfaces. A row drawn inside point keeps the page's ground, or the cursor
  would run the whole subtree. In the light theme point's ink moved from gold
  to a deep blue: gold is that theme's ground, and gold text on it read as
  nothing — the ordinals went missing.

- **`TAB` folds a child headline, and reserved tokens read by their letter.**
  TAB on a child hides its whole subtree behind org's ellipsis and brings it
  back; `f` on the headline enters the body.
  The drawer's colons dim, the leading one hanging into the gutter so
  `:PROPERTIES:` and every `:KEY:` align on their letters, and the left rail
  runs unbroken through the drawer block.
- **An enclosing run's bar rides the ramp.** Inside a nested list, point's own
  run bars in the page's ink and each enclosing run steps down the accent by
  its distance out — the block spines' own grammar, one storey down.
- **A list run wears a spine, and org's bullet always paints.** The tree's
  elbows are gone: every item bars its whole extent at its run's rail,
  siblings stack into one unbroken bar, a nested run adds its column inside
  the parent item's — the blocks' own grammar one storey down. The marker is
  content and is never hidden; the `glance-bullets` look and its storage key
  are retired. A headline at point lights its block's spine and stops there;
  a flag's red stretch rides one level over the spine.
- **The dim follows the reader into a child's shelf.** Focus mode engages
  inside any block — a list, a drawer, a child's contents — and lights the
  path alone: chain headlines read, other branches dim, their stars and
  their reserved tokens (`DEADLINE`, `:PROPERTIES:`, pair keys) with them.
  A selected or offered headline's own block reads whole. A fold mark
  breathes: the ellipsis rides the folded line as " …".
- **The pane is a narrowing.** What is written stays inside the materialized
  subtree: a typed headline at the root's level or above is demoted to the
  first child level, org's own narrowed-buffer rule.
- **The bars are block spines on F's ramp.** Every top-level row's bar is a
  segment of its block's spine — margins bridged, the list beside its own
  tree — and the ink is the spike's winning ramp: the block point is in wears
  the page's ink, each enclosing block a step dimmer in the accent, other
  branches resting. A flag still outranks everything. A block is one element,
  so its spine runs unbroken past margins and nested headlines alike; a
  selected headline lights the block it carries, trees and drawer bars
  included, and a drawer's pair bars wear the spine's own ink.
- **A hidden bullet leaves no blank.** With bullets stepped aside, the elbow's
  horizontal grows toward the text, `tree`'s own look; an ordinal or a bare
  checkbox is content and keeps the short turn.
- **A child headline is a headline, at every depth.** It wears the headline's
  own face, the path strip names it by its title (it read "paragraph" before),
  and `f` on it enters its body — a child holding only a deeper child refused
  before.

### Fixed

- **A field that completes over an open vocabulary commits the word you
  typed.** Typing `shelf` into the tags popup's `+` with `bookshelf` in the
  tree narrowed to that one match, put point on it, and `RET` wrote
  `bookshelf` — the reader's own word was never drawn, so it could not be
  taken. The same swallow ran the kind picker and both halves of the sheet's
  inline pair box, where `OW` completed to `OWNER` on `:` and a value walked
  away from could not be walked back to. **The typed value is always an offer**
  now, wherever the vocabulary is open: it is drawn as its own leading entry,
  hinted `new`, so `RET` commits what was typed and a match is one `C-n` away.
  An empty field offers no literal, and a typed value that folds to an entry
  coincides with it — one entry drawn, never two, and the entry it folds to
  leads, since the coincidence is asked of the whole vocabulary rather than of
  what an offer cap left standing. A commit has one source, the
  entry point rests on, which retires the free-text side door that used to fire
  only when the list happened to be empty. Openness is spelled at each call
  now, so the closed lists stay closed: the state palette, and the capture
  template's `%`-codes, which are the server's own.

- **The renderer reads the added sign.** The page's own grammar had no `+`, so
  the sign was body text wherever the page read a query: the strip drew
  `+priority:[#B]` as `substring:+priority:[#B]`, a widening spelled as a
  narrowing; completion over `+sta` offered the text-search literal alone and
  committed the dead `substring:+sta`; and the local matcher hunted the sign as
  characters in a row. The scanner takes both signs now. Chips spell an added
  token as written, free text as `+substring:bread`; completion works behind the
  sign and hands it back on accept, as it always did for `-`; the shaping keys
  drop with a sign on them, which is the 400 the server answers; and the local
  matcher answers the axis law — per key the plain and negated tokens AND, the
  added ones OR against that conjunction, the axes AND — so the page and the
  daemon serve one query. An opposite-signed twin committed onto a chip already
  standing in the strip annihilates the pair, the strip's own affordance over
  `-state:DONE +state:DONE`, which the grammar answers as every row.

- **A headline at point lights one shelf.** The block-light rule reached
  lists through child headlines' blocks — a depth paragraphs and drawers
  never claimed — so a selected headline lit list spines all the way down
  the subtree. The child combinator now holds the light to the block's own
  shelf, and the browser suite pins the child's list resting.
- **The focus dim covers every ink.** Reserved tokens, punctuation, links and
  state boxes ride inherited variables, so the dim and the lit path move as
  one — the fold ellipsis and a headline's tags included. Lit bars take a
  mark ink a step short of the page's, and a sibling headline's list spines
  stay unlit, so the path leads the eye without competing with the words.

- **A bare `:PROPERTIES:` drawer dims whole** — the word with its colons,
  inside a lit block too: a frame holding no pairs is furniture.

- **An `[[id:…]]` link resolves in org-id's namespace.** `id:` names the
  `:ID:` property, so `ref:` filtering now matches it there — and never
  against `ORG_GLANCE_ID`, whose links are the `glance:`/`org-glance-*:`
  family's.

- **A planning keyword's colon sits after its last letter.** The hanging-colon
  rule matched any first `.dpunc` element, so `DEADLINE:`'s trailing colon was
  pulled a character left onto the `E`.

- **A continuation typed into a wrapped list item is visible.** `M-RET` inside
  an item whose own line wraps added the line and left the box a row short, so
  what was typed sat under the pane's own text. The open edit now makes room by
  the rows its text occupies rather than by org's newlines alone.

- **A reference carrying a kind resolves again.** Emacs's org-glance writes an
  edge as `org-glance-material:ID?kind=SLUG`; the daemon kept the whole tail as
  the target, so every such link pointed at nothing — `ref:` queries missed the
  rows, and the subtree that named them showed no reference. The target is now
  the id, cut at the first `?`. A title keeps its own question mark, being text.

## 0.7.0.1 - 2026-08-14

### Added

- **`glance --help`, and `--help` after any command.** It printed
  `cannot read --help as UTF-8 org` and exited 1, the flags being documented in
  the README instead of in the program. Every command now answers `--help`,
  `-h` and `help` with its own block — each flag on a line of its own, plus the
  facts a reader needs before pointing a browser at it: `serve` binds
  `127.0.0.1` alone, and binds before the walk, so store routes answer 503
  until the tree lands. Asking outranks running: `serve --help` prints without
  `--dir`.

- **`make major`, `make minor` and `make patch` cut a release.** A cut has to
  move three files that spell the version and rename the changelog's
  `## Unreleased` to the version and today's date; done by hand, one of them
  drifts. PVP decides which digit moves: `major` for a breaking change (`0.7` →
  `0.8`), `minor` for an addition, `patch` for a fix. Cutting an empty
  `Unreleased` is refused. Nothing is committed or tagged.

### Changed

- **A bare `glance` prints the usage, and the REPL is `glance repl`.** Running
  the command with no argument opened an org prompt, and a first argument that
  was not a command name was read as a document to load — so a mistyped command
  dropped the reader into a REPL rather than saying what it did not recognise.
  Every command is now asked for by name, `repl` among them (`glance repl FILE`
  for the old one-argument form), an unknown one is a complaint plus the usage,
  and no argument at all is the question the usage answers.

- **Front-end source moved out of `assets/` and into `frontend/`.** The glue
  needs no build step, so its source bytes are the embedded bytes and
  `assets/glue/` read as honest; Elm arrived later and its `src/` was parked
  beside its output, leaving 5000 lines of Elm source and tests plus a build
  cache in a directory named for shipped bytes. `assets/` now holds `elm.js` and
  `table-view.js` alone.

## 0.7.0.0 - 2026-08-14

### Added

- **Every region of a subtree says what a new line inside it looks like.**
  `S-RET` in the materialize sheet asks which region the caret stands in and
  writes that region's own continuation: a bullet item its own bullet, a
  checkbox item an empty box, a numbered item the next number, a TABLE an empty
  row aligned to the table's own column widths, a `#+begin_X` BLOCK or a DRAWER
  (`:LOGBOOK:` among them) an empty line at its indent, a paragraph an empty
  line with a blank above. The line lands immediately under the caret's own,
  inside the region — so a run splits where the reader stands, a block still
  closes and a table still parses — and a caret on a closing line (`#+end_X`,
  `:END:`) lands past the region instead. `+` with no box open reads no caret
  and rides past the whole structure as it always did.
- **A region nested inside a list item answers for itself.** A `#+begin_src`
  run, a table or a drawer riding under `- alpha` is asked about in its own
  right, so a caret in the source adds an empty source line and a caret in a
  `:LOGBOOK:` adds a clock line. A line no nested region claims is still the
  item's, and so is a caret on the nested block's closer — the new item lands
  right under the block rather than past the whole list.
- **The box a `+` opens puts point where the reader types.** At the end of a
  lead (`- `, `1. `, `- [ ] `) as before, and one space INSIDE the first cell of
  a seeded table row: point at the end of `|   |   |` made the first character
  typed a third column, which org's own align then kept.

- **A browser delete now tells Emacs, so the record leaves with the bytes.**
  Deleting a row moves its blob to the trash and appends one line to
  `<store>/.org-glance/meta/EXTERNAL.jsonl`: the two fields every write already
  spells, plus a third, `"tombstone":true`. `M-x
  org-glance-graph:refresh-external` folds that line into the tombstone
  `graph:delete` writes, so the index drops the record instead of keeping it
  live over bytes that have moved. A delete splices no spans and so never
  reached the door every other write leaves by; the note rides the move itself
  instead. The field is on a delete alone and carries JSON `true` alone —
  absence is the plain line, so each fact has one spelling — and the word is
  org-glance's own WAL spelling, so neither side learned a second vocabulary.
  The version skew is safe both ways: a new glance against an older org-glance
  degrades to exactly the old behaviour, that reader taking the `id` and
  ignoring keys it does not know, and an older glance against a new org-glance
  writes no tombstones at all. `make interop` reads the whole leg back — the
  bytes glance wrote, the kind Emacs's own reader took them as, the record Emacs
  dropped, and glance's index fold seeing the tombstone Emacs wrote — and its
  instrument line is now `unmatched 1 unindexed blobs, 0 records without blobs`.
  The one left is the tagged capture, which is still pinned as a hole.
- **`make test` runs the Elm scanner's suite too, and `make typecheck` asks all
  three languages at once.** `make test` is now every suite that runs off this
  tree alone: `cabal test` first, so the first red line is a Haskell one where
  there is one, then `make elm-test`'s cases, which sat behind their own
  target for the network reason and were run by nobody else. `make typecheck`
  is new — `cabal build all`, then `make check-glue`'s tsc over the shell, then
  the Elm compiler at `--output=/dev/null`, since Elm's compiler is its
  typechecker and the committed `assets/elm.js` is a build input only `make
  elm` may rewrite. Both skip loudly where npx is missing. `browser-check`,
  `interop` and `mutate` stay out: a chromium, an Emacs plus the peer checkout,
  and a runtime measured in minutes.
- **`make interop` runs glance and Emacs against ONE org-glance store, in both
  directions.** Emacs seeds a store, the daemon serves it, and its cases
  ask the one question neither project's own suite can: that the bytes one
  program writes are the bytes the other reads. Browser to Emacs — a `set-state` over a
  blob leaves a notification line org-glance's own reader parses, out of the
  file its own accessor names, and `refresh-external` folds it into the WAL and
  moves its cursor past the line, leaving the bytes where they were; a keyword
  only a tag's
  `#+TODO:` cycle declares comes back as a state rather than as title text; a
  glance write leaves every other file in `meta/` byte-identical. Emacs to
  browser — org-glance writing a blob of its own reaches an open socket as an
  `upsert-row` frame in 142 ms, with no notification file involved at all. And
  at rest — `glance scan` over a store org-glance actually wrote reports zero
  rows disagreeing, zero unmatched blobs or records, and zero span violations,
  which is the whole index-reading side proven against the real writer instead
  of against hand-written fixtures. One case PINS a known contract hole rather
  than blessing it: a browser capture mints an id Emacs's fold skips. Deletion
  was the second such hole and is closed above, read back end to end by the last
  case. And one case moves the notification file's BYTES under a live cursor
  between two folds, which is the only thing in either repo that would notice
  the cursor's prefix digest going missing — every other case here only ever
  grows the file, and an offset alone survives that. It is OUT of `cabal test` for `make browser-check`'s reason — it
  needs Emacs, a sibling org-glance checkout and a daemon — and SKIPS LOUDLY,
  naming which is missing. Host Emacs is the default; `EMACS_RUN=podman` runs
  the same cases on org-glance's own pinned image. `BREAK=name` takes one
  harness step out to watch the case for it go red. The Emacs half reads either
  shape the peer's `--read-external` answers — a bare id, or an `(ID . KIND)`
  cons — so an org-glance predating the third field makes the target skip
  loudly instead of dying on a type error.
  The daemon and the temp store are torn down on success,
  on failure and on a signal alike: teardown is one function the normal exit and
  `SIGINT`/`SIGTERM`/`EPIPE` all reach, so a run piped into `head` no longer
  leaves a daemon serving a temp store.

- **A commit closes what the typing opened.** Type `#+begin_src` in the
  materialize document and commit, and the block arrives with its `#+end_src`;
  a drawer opener arrives with its `:END:`. Nesting closes innermost first, a
  verbatim block suspends the rule inside it so a `#+begin_quote` in a src block
  stays text, the closer takes the opener's own case and indent and never its
  arguments, and text that already closes itself is written unchanged. An EMPTY
  block arrives with a blank line between its ends, so there is a line to type
  on; a block that already holds something does not.

### Changed

- **`M-RET` outside an open box is `+`.** Org's own `org-insert-item`, so all
  three of `+`, `S-RET` and `M-RET` add a sibling of the stop at point. Inside
  an open paragraph `M-RET` stays the newline and `S-RET` stays the commit that
  asks for another one.

- **A capture always lands in the tree's own `inbox.org`.**
  `#+GLANCE_CAPTURE_TARGET:` is no longer read, and a tree still carrying the
  line captures into the inbox anyway. One entry point per tree, so there is no
  path a config can aim a write at, and the three refusals it used to earn — an
  absolute path, one climbing out through `..`, and a name the walk would
  decline — are gone with the setting.
- **The settings sheet is two panels, theme and keywords.** GENERAL held the
  capture target and the log height and holds neither now, so it went with them;
  `TAB` walks the two and the sheet opens on the theme panel.
- **The log's height is a developer preference.** No field reaches it; the boot
  still applies whatever `glance-log` holds in `localStorage`, within the same
  band, so a page nobody has touched still opens at seven lines.

### Fixed

- **The `+` echo names what the caret actually lands on.** The word for where a
  sibling joins is the model's now, said with the draw, where the page used to
  work it out a second time off the row's fields: a caret on a table row inside
  a list item said `an item at this level`, and a caret on a `#+end_src` said
  `a line here` for a paragraph landing past the block. It reads `a row in this
  table`, `a line in this block` and `a line in this drawer` where those are what
  joins.
- **A large subtree opens without the pause.** The document pane's scanner asked
  for the Nth line of a list, which costs N steps, so its work grew with the
  square of the body: the largest subtree in a 6,331-file corpus took 28 ms to
  scan and now takes 4. Typing in an open box no longer re-measures the row it
  covers on every character, either — a row's padding cannot move while the box
  is over it.
- **A bullet inside a block or drawer nested in a list item is no longer a stop
  of its own.** The document pane's structure scanner hunted an item's raw lines
  for bullets knowing nothing of `#+begin_X` or `:LOGBOOK:`, so a `- ` inside a
  nested block became a row the pane drew, `f` descended onto, and `d` then `D`
  took — carrying the block's `#+end_src` out with it and leaving the block
  unclosed. The scanner and the region walk now answer one question between
  them, so a line a nested region holds is that region's wherever it is asked
  about; a bullet in a `:LOGBOOK:` under an item behaved the same way and is
  fixed by the same join.
- **A caret inside a block, a table or a drawer nested in a list item no longer
  writes a bullet into it.** The region walk settled on the item the moment the
  list run held the caret's line and never looked inside it, so `S-RET` in a
  `#+begin_src` under `- alpha` spliced `- NEW` between two source lines and
  left the block unclosed; a table under an item gained a second table and a
  drawer under one took a list INSIDE `:LOGBOOK:`.
- **A caret in a table inside a `#+begin_pin` no longer splits the table.** How
  deep the document pane's walk looks is org's own greater/lesser split now: it
  re-enters a GREATER region — an item, a drawer, and every block org parses the
  contents of, `quote` and a tree's own special blocks among them — and treats a
  LESSER one as opaque, those being tables and the five VERBATIM blocks
  `org-element-greater-elements` leaves out (`comment`, `example`, `export`,
  `src`, `verse`). It re-entered the ITEM alone, so a table riding in any other
  block was answered with the block's own empty line and org read the result as
  two tables. Six files in this corpus carry the shape. Two answers move with
  the rule and both are org's: a block inside a nested drawer is the BLOCK's
  lines, and a bullet inside a `:LOGBOOK:` is an ITEM — which is how that
  drawer's own state lines read.
- **A `#+begin_comment` block holds no items and no tables.** The opaque list
  was org's LIST rule, `org-list-forbidden-blocks`, which names four and spares
  `comment`; the split this walk asks is the ELEMENT one, and
  `org-element-greater-elements` leaves out five. A caret inside a comment block
  wrote a bullet or a table row into it where org parses neither. Nothing in
  this corpus spells the shape; it was reachable by typing it.
- **A block or a drawer straddling a list item no longer cuts the item in two.**
  The item run hunted bullets THROUGH a block it knew nothing about, so a `- b`
  written between two source lines ended the item above it and minted a stop
  whose `d`/`D` carried the `#+begin_src` off without its `#+end_src`. A region
  is one syntactic unit and the run steps over it whole, which is org's own
  `org-list-struct`. Nothing in this corpus spells the shape; it was reachable
  by typing it.
- **A caret on a non-item line no longer cuts the structure it stands in.**
  Inside a `#+begin_src` run, `S-RET` used to splice a bulleted item into the
  middle of the block and leave it unclosed; blocks and tables now answer with
  their own continuation instead of the list's.
- **`S-RET` over a list in the materialize sheet writes an item, whatever the
  stop over it.** One `n` off the headline lands on the whole list as ONE stop,
  so a box opened there covers every line of it — and the sibling it committed
  came out with no bullet at all, which reads as the region not being a list.
  A line inside a list belongs to an item however wide the stop laid over it:
  the new item wears the caret line's own indent and bullet and joins that
  line's own run. A caret on a continuation line takes the prefix of the item
  holding it rather than the list's first, so a column-1 bullet is never
  written into a nested run. `+` with no box open reads no caret and still
  lands a paragraph past the whole list, which is the only way to one.
- **An open paragraph edit no longer clips its own text or highlights past the
  row.** The block reserved 19.5px a line while the field rendered 20.8px, so a
  fourth typed line scrolled out of sight; the floor is counted in the pane's
  own metrics now, which is the pair the field inherits. And the box is the
  block it covers on all four edges: over a list item — whose row carries no
  horizontal padding, the nesting being the file's own spaces — the highlight
  used to start 22px left of the row, at the beginning of the line.
- **`S-RET` in the materialize sheet now puts the new item under the line the
  caret stands on.** Editing a list item that carries a nested run, the sibling
  it committed wore the caret line's own indent and bullet and then landed past
  the whole structure — a checkbox typed halfway down a run arrived at the
  bottom of it. The caret's line is the anchor now as well as the prefix: the
  item lands immediately under that line, everything below it stays below, and
  the row drawn before the write stands exactly where the write puts it. `+`
  pressed with no box open reads no caret and rides past the whole structure as
  it always did, which is right where nothing named a line to split.
- **A delete this daemon reports is no longer lost while Emacs is folding
  it.** Nothing here changed: the peer stopped rewriting
  `<store>/.org-glance/meta/EXTERNAL.jsonl` and now reads from a byte cursor
  beside it, so a line appended between its read and its write — which used to
  be destroyed, taking a tombstone with it and leaving a live record over bytes
  in the trash — is simply past the offset that fold recorded. The daemon's
  half of the argument is `appendLine`: it opens the path per line and holds no
  descriptor, which is what makes the peer's rotation safe, a rename landing
  mid-write putting that line in the rotated file rather than into an unlinked
  inode. `make interop`'s sixth case reads the new contract — the cursor
  advanced, the file kept every byte, and a second write landed past it.
  The cursor's own price is paid on the peer's side too: an offset says how far
  without saying which bytes, and this store is git-synced with the cursor
  tracked beside the file, so a union merge could put another machine's lines
  ahead of it and resume a fold mid-line. The peer's cursor now carries the sha1
  of the prefix it measured and re-folds whenever that no longer holds, and its
  git conflict resolver — which rewrote every `meta/*.jsonl` at graph open, this
  file included — now names the WAL's own files positively, so this daemon's two
  files there are out of its reach by construction rather than by exclusion.
  That sha1 is taken over the bytes the fold READ, out of the ONE read it slices
  its lines from, so a checkout landing while a fold runs costs a re-fold too;
  and the peer deletes an old notification generation only once that
  generation's own cursor says it is drained, where deleting the oldest by
  position took the deletes an unread generation still owed. `make interop`'s
  `bytes-move-under-a-live-cursor` re-lays the file under a live cursor between
  two folds and asks whether the keyword still arrives — the only thing on THIS
  side of the wire that would notice the digests going missing, the peer's own
  suite reddening three cases without it.
- **An outside write is no longer missed when that file is replaced at its own
  length.** Nothing here changed again: the peer asked whether a fold was owed
  by comparing its cursor with the file's SIZE, and `Data.Org.External.noteLine`
  spells fixed-width lines — one length for every write, another for every
  delete — so a synced store replacing a 58-byte tombstone with another 58-byte
  tombstone left the second id live through any number of reads. Its poll
  verifies the last 4 KiB before the cursor and that the cursor ends a line, at
  a cost flat in the file's size, and every fold that consumes anything verifies
  the whole prefix out of the buffer it had already read. The same predicate
  guards the peer's rotation, which used to unlink a re-laid generation and
  every delete note in it. `bytes-move-under-a-live-cursor` asks the peer for
  that answer now beside the cursor it always read: its re-laying leaves the
  length alone, so a peer that verifies what a fold consumes and polls by size
  passes every assertion the case already had.

- **An open edit sits on the same line grid as the text under it.** Every stop
  carried a pixel of vertical padding, and a list's items each spent it again
  inside the list, so the drawn lines walked 2px per item away from the field
  laid over them — measured 0, 2, 4 and 6 across four items, and over a
  sixteen-line list the foot stood more than a full line out. The outer box
  always matched, which is why nothing caught it.

## 0.6.0.0 - 2026-08-11

### Added

- **`make browser-check` measures the page in a real engine.** A headless
  browser opens the served page over a temp copy of a committed org fixture and
  reads geometry and computed colour back — where a box ENDS UP, which is what
  no test here could ask before. Eight cases, each named after a display bug
  that shipped green: an open edit pushes the line under it down instead of
  covering it, a paragraph drawn before it is written still owns a line, a flag
  paints one red on the table and in the document pane, no surface scrolls
  sideways or down at 360, 800 or 1400 pixels wide, a popup clamps inside the
  viewport, one keyword paints one hue in the table and the sheet, a paragraph
  sits under the title text, and the cursor is a ground drawn only where the
  keys are. It is OUT of `cabal test` for `make elm-test`'s reason — it needs a
  browser, a daemon and the machine's fonts — and SKIPS LOUDLY, saying which is
  missing, so a machine with neither changes nothing. `make browser` installs
  the browser; nothing is downloaded by the check itself. `BREAK=name` takes one
  rule out of the page to watch the case for it go red, and `ONLY=text` runs a
  subset. The daemon and the browser are torn down on success and on failure
  alike; a red run leaves its screenshots and the tree it served, named in the
  report.
- **`/` narrows every small list.** The link popup, the tags popup, the
  materialize sheet's property panel and the settings sheet's states table each
  take a filter field at the head of their own list — one program, so the key
  reaches all four. Matching is substring, case-folded, over the cells the list
  draws, with no grammar: a bar, a colon and a leading `-` are the characters
  they spell. The cursor keeps its row where the narrow spares it and lands on
  the first match where it does not. While the field holds the keys the
  surface's own bindings are the reader's typing — `RET` leaves the field with
  the narrow standing, `C-n`/`C-p` and the arrows step rows, `DEL` erases a
  character. `ESC` clears the narrow before it closes the surface, and `DEL`
  over a popup does the same. Flags survive a narrow, as they do under the
  table's own filter, so a row the field is hiding is still in the set `D`
  takes. A surface that closes takes its narrow with it.
- **The span layer's laws are tested as universals.** The suite grows a document
  generator and 24 property groups over it: a `DocSpec` is rendered to org text
  while the offsets it lands at are recorded, so the parser's spans are compared
  against an answer counted by code the parser never ran. What they cover is
  every sub-span's position, `hsFull` as a fold, subtree extents tiling, the
  nesting and reparse invariants, `stripSpans` leaving no offset behind,
  `applyEdits`' acceptance boundary and length algebra, the subtree lens'
  byte-identity, and a timestamp surviving render then parse under any weekday
  spelling. The seed is fixed so a red run replays from the commit alone;
  `GLANCE_QC_SEED=N cabal test` unfixes it. The generator is asserted before
  anything is read through it — a census over 400 documents that fails when the
  image goes narrow.
- **`make mutate` grades the suite.** One rewrite per mutant over one file —
  ten rules across Haskell, JavaScript and Elm — run in a git worktree with its
  own build dir at `-O0`, and a mutant the suite leaves green names an
  assertion nobody wrote. `make mutate TARGET=path` is a sitting: the cold build
  is paid once, `SAMPLE=N` draws N sites seeded by the target's own blob digest
  so an unchanged file repeats its mutants and an edited one draws a different
  set, and `make mutate-list TARGET=path` prices a target without building
  anything. It reads the committed revision and never writes the working tree.
  The report names each survivor with its rule, its before and its after, and
  tallies killed / invalid / equivalent / survived with a mutation score. Out of
  `cabal test` for `make elm-test`'s reason one size up: a check whose unit is
  minutes lives behind its own target.
- **`x` is dired's `dired-do-flagged-delete`,** on the table and on all four of
  the materialize sheet's flag surfaces. It takes the FLAGGED rows alone —
  never the row at point, which is what `D` does — and asks first, naming the
  count. Nothing flagged writes nothing and says so. One question, weighted to
  what it will do: a set that is wholly archived asks for the typed `delete` it
  always did, everything else asks for `yes`, and neither asks twice.

### Changed

- **`+` in the materialize sheet adds a sibling of the stop, so an item joins
  the run the cursor stands in.** Standing on a list item, `+` now adds an item
  at the bottom of that item's own run, wearing its indent and bullet, an empty
  checkbox where the stop has one, and the run's next number where it is
  numbered — so `f` into a nested run and `+` writes at the nested indent. The
  drawn row shows that prefix before a character is typed, and an empty `+`
  still writes nothing. Standing on the list itself — one `b` away — `+` adds a
  paragraph past the whole structure as before, and a table line and a
  `#+begin_X` run keep that landing, neither having a prefix to spell.
- **A write spells no trailing space.** Every text glance composes for a write —
  a subtree the materialize sheet hands back, a captured entry, the document a
  tagged capture stores — lands with each line ending at its last non-blank
  character, however it was typed. Horizontal space INSIDE a line is content and
  survives: a table's alignment and a source block's indentation are the bytes
  they were. Line endings survive too, so a CRLF file stays a CRLF file. Bytes
  outside the region a write touches are untouched as ever, so a file the daemon
  has not written keeps whatever it holds.

### Fixed

- **A heading carrying a bracketed date is no longer reported as a span
  violation.** `* Decided [2026-08-11]` tripped the scan's own title check,
  which compared the slice against a re-render — and a render recomputes a
  timestamp's weekday, so a source stamp without one never matched itself. The
  parse and the span were always right; the oracle was too strict.
- **`delete` no longer logs "state cleared" over every row it removes.** The
  log strip is the page's audit surface, and the one destructive command was
  reporting a state change on each file it moved out of the tree.
- **A list token you edit is the token that gets written.** Changing the
  drawn `- [ ] ` to `- DONE` wrote both, because the composer prepended the
  token the box was already showing. What the box holds is now what is
  written, so a plain `- ` run continues as one too.
- **`+` adds a list item directly below the one you are on,** where it landed
  at the bottom of the whole run. Org's own `M-RET`: you walked to an item, so
  the new one belongs under that item rather than somewhere you would walk
  back up from.
- **Adding a list item shows the bullet while you type it.** The box sits over
  the drawn row exactly and opaquely, so the `- ` or `- [ ] ` the row wears was
  hidden until `RET` and the field looked empty. It carries the token now, and
  what goes over the wire is still only what you added.
- **A tag's config file can no longer set a tree-wide setting.** The default
  view, the agenda and archive views, the capture target and the tree's state
  hues belong to `system.org`; a settings write aimed at a tag layer that named
  one used to be stopped by a list kept beside the rule, and a setting missing
  from that list would have been written into the tag file silently. Which file
  a setting belongs to is now declared once, with the setting, and the write
  reads that declaration. What a tag layer does own — its keyword cycle and its
  capture template — is written exactly as before.
- **A highlighted line in the materialize sheet sits where an unhighlighted
  one does.** The cursor row carried the edit box's floor even with nothing
  open, and the box's line height is a shade tighter than the pane's, so the
  highlighted line stood three quarters of a pixel taller than its
  neighbours and read as offset upward.
- **A headline whose keyword or priority ends its line no longer eats the line
  under it.** `* TODO` followed by a second headline read as ONE entry titled
  with the second's own line; followed by a property drawer it swallowed the
  drawer and the `ORG_GLANCE_ID` inside it, which is how an entry loses the id
  everything else keys on. Trailing spaces on a title line did the same to the
  planning line and to the drawer.

## 0.5.0.0 - 2026-08-11

### Added

- **`M` toggles.** With every row already marked it takes the marks off
  instead of saying the same number twice.
- **The agenda is `A` rather than `a`.** `a` is free.
- **`+` adds a paragraph in the materialize sheet's document pane.** The empty
  paragraph is DRAWN where it is going and point moves to it, so the textarea
  is over a line of your own rather than over the one you were standing on —
  the same textarea `RET` edits one with, so `RET` writes it and `S-RET` is the
  newline. Point stays on the new paragraph afterwards. `ESC` leaves behind
  what it found, point included, and writes nothing. It lands under the whole
  structure the cursor stands in, never between a list's items, through a table
  or inside a `#+begin_` block; on the headline line it leads the body, and it
  gives a body to an entry that had none. A blank one adds nothing, and a child
  refuses and names `RET`, which opens it.
- **`D` over archived rows deletes them, after asking.** A row org has archived
  is one step from gone, so the same key takes the next step: the blob's whole
  directory — its document and the history org-glance keeps beside it — is
  gzipped into `<store>/.org-glance/trash/` and leaves the live tree. It asks
  for the word `delete` rather than a keystroke, a mixed set is archived rather
  than deleted, and a row in a file other rows share is refused — only a blob is
  a single row's document.
- **A third saved view, `archive` (`tag:*archive*`)** — the one query that lifts
  `/headlines`' own exclusion, now reachable as `view:archive`, from `P`, and
  from the settings sheet like the other two.
- **`view:default` and `view:agenda` in the filter box** apply those views, the
  same as pressing `g` and `a`, and the box **offers them**: the view JSON
  declares every saved view with the query it holds, and `view:` completes from
  that with the query shown beside each name. Picking one applies it and closes
  the box — a view is the whole answer, not another narrowing. The token expands
  to the view's own query rather than surviving in it, so a view the server
  grows works with no page or renderer change.
- **Every popup has a URL you can send.** A surface on screen writes
  `?page=NAME` beside the query — `&row=ID` where it stands on a row, and the
  panel as the fragment, so the settings sheet on its theme tab is
  `/?q=state%3A*active*&page=config#theme`. Opening the link lands on the row
  and raises the surface once the rows are in hand; closing it takes the
  parameter and the fragment off.
- **Repeating entries repeat.** Completing a row whose `SCHEDULED:` or
  `DEADLINE:` carries an org repeater cookie now shifts the stamp forward and
  resets the keyword to its chain's first active state, in ONE drift-locked
  write. All three cookies: `+1w` adds one interval to the stamp (so an overdue
  entry stays overdue, org's own behaviour), `++1w` catches up past today, and
  `.+1w` counts from today. The time of day, the warning cookie, a range end and
  the brackets ride through as written; only the dates move, and each weekday is
  recomputed. A row that repeats says so on the wire as a sparse `repeats` field
  carrying its cookie.
- **A completion ledger, beside the entry rather than inside it.** Each repeat
  appends one line to `<root>/.org-glance/meta/COMPLETIONS.jsonl` —
  `{"id","at","state","shifted"}`, append-only, keyed by `ORG_GLANCE_ID`. THE
  LEDGER IS DERIVED, NEVER TRUTH: the org file already carries the shifted stamp
  and the reset keyword, so deleting the ledger loses only the history and
  changes no entry. A tree with no `.org-glance` store repeats org-natively and
  records nothing — no daemon makes a store directory it was not given. It is
  incomplete by construction: Emacs's own `org-todo` writes org's LOGBOOK and no
  ledger line, so this records THIS daemon's completions.
- **`substring:VALUE` is free text under a key.** The filter grammar reads
  `key:value` everywhere now, and a bare word is that spelling with the key
  elided — ONE matcher answers both, so they can never come to mean two
  things. Negation, alternation and the half-typed `substring:` all fall out
  of the machinery a predicate already has. What the key buys is a value that
  may spell a separator's neighbour — a leading `-`, a colon, a bar — under
  quotes without being read as something else. Both sides of the wire
  (SCHEMA.md, Filter query).
- **The settings sheet is tabbed, and a tree colours its own states.** General,
  theme and keywords are one panel at a time. `TAB` walks the panels and wraps,
  `S-TAB` walks back, and the new panel's first control takes the focus. The
  theme tab grows a **states table** — `tag | state | group | colour`, one row
  per keyword the tree knows, by layer and then in each layer's own cycle
  order. `RET` edits a row, `+` adds a state to its layer, `d`/`dd`/`D` remove
  one, `u` unflags: dired's gesture, the same one the property panel has. Every
  config layer is editable, system and per-tag alike, so a state belongs to a
  file and rides that file's write; a colour is tree-wide and rides
  `system.org`'s, and one row can move two files in the one flush. Colours are
  under **the theme on screen** — there is one theme control now — and stay
  stored per theme because readability is. A keyword only a plain org file
  declares is listed under the tag `file`, colour-editable and refusing removal.
- **The agenda is a view the tree configures.** `a` used to apply a query this
  page spelled; it now reads `system.org`'s `#+GLANCE_AGENDA_FILTER:` line, the
  default view's own rule one entry over — filter, order and columns in one
  string, since the query is the whole carrier of a view. The settings sheet's
  general panel picks WHICH saved view its composer edits (`default`, `agenda`),
  and a write names the views that MOVED, so editing one leaves the other's line
  where it was. A tree naming no line gets the built-in it always had.
- **`+` opens on the tag the view is filtered to.** Capturing from a table
  narrowed to one tag almost always means another entry of that kind, so the
  form's tag field arrives carrying it and that tag's template is already
  expanded — its asks on screen before the reader types. A suggestion rather
  than a rule: the field is focused and ordinary, so backspacing to the inbox
  is one key. Only a tag a capture could WEAR seeds it — a negation says which
  kind this is not, an alternation names no one kind, and a starred word is a
  meta.
- **`sort:` completes to the order in force.** Typing the key and nothing else
  now offers the chain the table is ALREADY in, in canonical arrow form, as the
  first row — so RET fills it in rather than leaving the reader to spell an
  order the view could have told them. It does not commit: the caret lands at
  the end of the chain with the list still open, which is where a segment comes
  off with backspace or another goes on with `->`.
- **`q` closes a browsing popup, and quits the native window from the table.**
  Over the link and tags popups it is DEL's rung under another key — dired's
  own — through the same door ESC reaches; the state palette keeps its letters,
  `q` there being a keyword's initial like any other. On the table with nothing
  raised it asks the native window to close, which stops the daemon because the
  window IS the app; a browser tab has no such handler and the key says so.
- **`SPC` (and `C-c C-c`) toggles an org checkbox in the materialize sheet.**
  On a list item wearing a box — `- [ ] step`, numbered items included — `SPC`
  at the stop flips it org's own way: `[ ]` checks, `[X]` clears, the partial
  `[-]` a parent inherits checks. The write splices that item's lines and
  nothing else, drift-locked like every element commit, and the echo names
  `org-toggle-checkbox` with the state it landed. `C-c C-c` with no element
  open is the same toggle — org's own second meaning of the key — and still
  says `nothing open here` away from a box; `SPC` off a box refuses with
  `no checkbox here` and writes nothing.
- **The filter palette completes `columns:` the way it completes `sort:`.**
  After `columns:` the suggestion list offers the view's own columns, a comma
  re-opens the domain the way `->` re-opens the sort's — the set is completed
  one column at a time and stays one token — and a name already in the set is
  not offered twice. A name the view does not carry stays writable: it is the
  producer's custom property column, so the list is vocabulary, never a wall.
- **`columns:` shapes the table from the filter box.** A third view token
  beside the filter and the sort: `columns:State,Title,Tags` shows those
  columns in that order and narrows nothing — typed through `/` like any
  other token, one chip in the strip (same shape, the link hue instead of
  frost, so it reads apart from both the filter and the sort), `DEL` takes
  it whole and the default six come back. Names match case-insensitively
  against the view's keys and headers alike (`Tags`, `tag`, `#` all land);
  a name the view does not carry is a CUSTOM column reading that headline's
  own property drawer — `columns:state,ORG_GLANCE_ID` puts the id on
  screen — and `Closed` is the planning line's own timestamp. The minimal
  set is Title: a set naming it keeps it where it was put, one without it
  gets it first, and an empty list (`columns:`) falls back to the default
  view. Repeats compose in written order, a name named twice keeps its
  first place; a negation or an alternation is the whole request's 400
  naming the token. Server-shaped end to end: `/headlines` declares and
  fills the picked set, a picked `state` column keeps its badges, and the
  shell remounts whenever an answer's columns differ from the mounted ones.
  The query is the one carrier of a view, so `P` pins filter, sort AND
  columns in the config's one `#+GLANCE_DEFAULT_FILTER:` line and `g`
  applies all three back — no new mechanism, the tokens simply ride.
- **RET on the headline line in the materialize sheet opens its title, in
  place.** The whole line's edit is its title — state and tags have their
  popups, the priority ring is pressed — so the element-grain RET and RET on
  the title cell are one door, with no `f` spent picking the cell first. The
  editor is ONE field laid over the title text alone: the stars, the state
  badge and the tags stay on screen around it, the field wears the document's
  own font with no padding jump, and only the ground says an edit is open.
  A headline with no title yet opens it empty and `set-title` writes one in.
- **The native window opens `o`'s links in a reading pane of its own.** The
  window has no tabs to switch to, so an `http(s)` link opens in a popup — 80%
  wide and 90% tall of the main window, centred over it, transient so the
  window manager stacks the pair — with ESC or the manager's close ending it
  and the table untouched underneath; any other scheme still goes to the
  desktop's own handler. Before this, a scripted `window.open` in the native
  window went NOWHERE: it fires WebKit's `create` signal, which nothing
  answered, so the old system-browser handoff (wired to the policy door alone)
  had never fired for `o` at all. The scripted half is intercepted at document
  start — `window.open` is patched to post its URL to a script-message handler,
  the same shape an iOS/WKWebView port must use — because answering `create`
  with a view aborts the whole daemon on current WebKitGTK when the open was
  made with `"noopener"` (a disengaged `WindowFeatures` optional inside the
  engine). A real `target="_blank"` anchor keeps the policy door and lands in
  the same popup.
- **`+` CAPTURES INTO THE STORE, under a tag and through that tag's own
  template.** The key is a chain of prompts now — which tag, whatever that tag's
  capture template asks (`%^{PROMPT}`, one field per prompt in template order),
  then the line — and ESC at any of them ends the whole thing with nothing sent.
  A TAGGED capture writes a real org-glance blob: a minted `ORG_GLANCE_ID`
  (`org-id-uuid`'s own version-4 form), org-glance's sharded
  `data/<2>/<rest>/data.org` path, the tag on the headline, the creation stamp in
  the drawer, and the `meta/EXTERNAL.jsonl` line that makes `M-x
  org-glance-graph:refresh-external` adopt it — so a capture from a phone lands
  as a first-class org-glance headline and Emacs sees it on its next refresh.
  Leaving the tag EMPTY is the inbox capture exactly as it was, bare `* text`
  plus a creation drawer, byte for byte.
- **A tag's capture template is the first heading of its config layer**, which is
  the file that already carries its `#+TODO:` cycle — org-glance's own
  convention, read the way `org-glance-tag-config--entry` reads it (from the
  first `*` line to the end of the file, right-trimmed), so `book.org`'s `* Book`
  over `*** Notes` is ONE template. `system.org`'s is the tree's default, and a
  tag no layer configures takes the bare `* %?`. The expansion subset is `%?`
  (where the typed line lands, and a template without it is refused naming it),
  `%U`/`%T` (the moment of capture, inactive and active, one clock read per
  request) and `%^{PROMPT}`; **everything else copies through verbatim**, so a
  template using a code this server has never heard of captures it literally
  rather than being silently emptied.
- **`GET /capture[?tag=NAME]`** — what a capture will ask for before it asks it:
  `{template, prompts, tags, codes}`. `prompts` are the template's own asks in
  template order (one spelled twice is asked once), `tags` is the tree's whole
  vocabulary for the tag prompt to complete over, and `codes` is the expansion
  subset with a line of meaning each. The subset is spelled ONCE, server-side:
  what this route serves is what expands and what the settings box completes.
- **The settings sheet's selected layer gains its capture template**, verbatim,
  beside its cycle — the server slices the heading's extent and splices what
  comes back, in the SAME `/config` write, so a layer is still one file, one
  digest, one splice. `%` in the box raises the code list the server served.
- The answer to a capture names **the row it made** — the minted id for a blob,
  the target file's `FILE#K` ordinal for an inbox line — and the cursor lands on
  it when the watch delivers it. A row the view has not got (a filter that hides
  it, a watch step that has not arrived) leaves point exactly where it stands.
- LINKS ARE WRITEABLE, which is the write boundary the popup was waiting on.
  `GET /links` now carries a per-link `span` — the half-open CHAR range the link
  occupies in the FILE — and the file's `digest`, and `POST /command` implements
  an eighth name, `edit-link {span, target, desc}`, which splices exactly that
  range. The scanner grew the offsets rather than gaining a second pass: one
  `linkParts` answers all three questions asked of a bracket link (what it SHOWS,
  where it POINTS, where it SITS), `linkAt` reports the WIDTH it consumed so a
  scan costs the links it finds rather than the tail behind each of them, and
  `subtreeLinks` shifts the subtree scan's spans into document offsets — the
  currency `Data.Org.Edit` splices in. A target spelled twice is still ONE entry
  and the entry is now the first occurrence's description AND span, so an edit
  through a deduplicated link edits the first spelling and the others stand.
  THE FORM IS PRESERVED, which is what makes it a link edit rather than a rewrite
  of the text around one: `[[T][D]]` keeps its description under a target-only
  edit, `[[T]]` stays desc-less, a plain URL swaps its target and stays plain,
  and a description ARRIVING is the one thing that changes a shape — a plain URL
  has nowhere to write one, so it brackets. ABSENT IS NOT NULL, the `args`
  discipline (`.:!`) reaching its first non-keyword field: a request saying
  nothing about the description leaves the author's, `null` takes it off, and a
  description that SHOWS nothing is the null spelled another way, since
  `[[T][]]` shows its target — the emptiness test strips and the value is
  written verbatim, content being nobody's to trim, which is the target's own
  rule (a whitespace target is refused, a spaced one is written as given). TWO
  WALLS, both 400 naming what they turned down:
  the span must sit inside the ROW's own subtree — a span outside it would let
  one row's write reach bytes no reader of that row was shown, under that row's
  digest — and cover exactly one link edge to edge; and the REPLACEMENT must read
  back as THE LINK IT CLAIMS TO BE, which reparses and COMPARES rather than
  checking the shape (a target spelling `a][b` renders `[[a][b]]`, one link
  pointing at `a` described `b`, neither of them asked for). A newline in either
  half is refused ahead of both, being the one thing reparsing cannot catch: the
  scanner has no line rule, so the link reads back as itself and lands a column-1
  star that the ORG parser reads as a new headline. `Data.Org.Edit` is
  content-agnostic by law, so this is the layer that owes all three. `edit-link`
  is also the only command that names ONE ROW: its args name a row's own
  CHARACTERS, so a span means nothing to a second row and over two files would
  name a different range in each. That rule is its own `csArgs`, which is handed
  the ids beside the `args` (see Changed).
- `RET` over the link popup EDITS the link at point, and the stub that named the
  missing write is gone. The title and url cells become fields over themselves
  (`LROW`, the shared edit overlay's THIRD shape), `TAB` hops, `RET` commits
  `edit-link` over the span `/links` handed out under the digest that answer
  carried, and `ESC` restores — the property panel's edit model exactly, so a
  panel row, a tag and a link are edited alike and the derived type cell never
  opens. The overlay's `cell` flag became a `cells: [FROM, TO]` RANGE over the
  row's non-gutter cells, which is the one generalization the third surface
  needed (`[0, 0]` for the tag rename, `[1, 2]` for a link, absent for the whole
  row). `fire` gained a trailing `pin`, so a command measured against a text can
  say which one; the commands naming a PROPERTY of a row send none. The page
  holds no bracket grammar and no offsets of its own: it sends the range it was
  given and the two strings a reader typed, and the untouched FIELD is what makes
  absent-not-null reachable — the description field opens on what the link SHOWS,
  which for a link carrying none of its own is its target, so a field left alone
  sends no `desc` at all. THE POPUP CLOSES ON THE PRESS, both outcomes alike,
  which is `o`'s own rule and is forced rather than chosen: the spans it holds
  describe a file the write has just moved, the store does not know yet
  (`/command` never writes it — the watch does, a debounce later), and a re-read
  HERE would answer with what the file said BEFORE the write, which is the tags
  popup's own documented reason for folding answers instead. `o` again is one
  keystroke and comes back with fresh spans, descriptions and types. KNOWN
  CONSEQUENCE, stated rather than worked around: the popup is also the only
  editor, so a row holding exactly ONE link is followed and never listed, and
  that link has no editor — a key that LISTS whatever the count is would settle
  it.
- The log strip's height is a preference, and it is STATIC. It grew to what had
  arrived and stopped at seven of its own line boxes; it is now exactly that
  many, always, whatever it is holding — a fixed frame the messages scroll
  inside. A strip that grew was the table resizing under a reader's cursor every
  time a write logged a line, which is the one thing a keyboard surface must not
  do, and a quiet page now reads the same as a busy one. The figure is a
  `localStorage` preference edited from the settings sheet's GENERAL panel
  (`#clog`, the second row, under `capture target`). The
  stylesheet keeps the arithmetic and declares the default — `#log{ …
  --g-logn:7; height:calc(var(--g-logn) * 1.5em + 2 * 6px + 2 * 1px);
  flex:none … }` — and the knob
  writes a NUMBER onto the element (`style.setProperty("--g-logn", …)`), so
  there is one formula in one place and a page whose glue has not run — or a
  reader who never touched the field — gets the same figure the sheet
  would put back. Stored under `glance-log` beside `glance-theme`, applied on
  boot and on every accepted keystroke, on `input` rather than `change` so the
  field is a knob rather than a form. `LOG = {key:"glance-log", def:7, min:1,
  max:50}` in the glue is mirrored in Haskell as
  `logLinesDefault`/`logLinesMin`/`logLinesMax` and `logLinesBand` (the
  placeholder's `1–50`) — the same constants the stylesheet's declared value is
  spelled from, so the two cannot drift. Blank is the default, which is how a
  reader asks for it back, and it REMOVES the key rather than storing `""` — a
  preference spelling the empty string is still a preference. A whole number
  inside the band is that number;
  everything else is DECLINED rather than clamped, so the height a reader had
  stands, nothing is stored, and reopening the sheet draws the preference back
  over the refused value — half a number on the way to a whole one is the
  ordinary case of that. A stored value the band no longer takes falls back to
  the default, the boot reading it through the same check. The panel says where a preference is READ rather than
  what writes it: `cmoved` never sees `#clog`, so the knob costs no request and
  cannot make a pristine sheet dirty. The table takes the whole of the rest
  (`#app` is `flex:1 1 auto`, the strip `flex:none`). `LOGCAP` = 500 is the
  strip's RING (how many lines it keeps) and is a different limit, unchanged.
- **The order is part of the query.** `sort:COL` orders the answer by that
  column and `sort:COL:desc` reverses it; written order is precedence, so
  several tokens compose a chain (`sort:state sort:deadline` is state with
  deadline settling its ties). The token NARROWS NOTHING — it states an order
  and leaves the set to the predicates beside it — which makes it the one key in
  the grammar that is no predicate, and `?q=sort:deadline&limit=100` is
  therefore the first hundred rows OF THAT ORDER rather than a hundred arbitrary
  rows a browser then re-sorts. What the view declares is the effective chain,
  so what a client is told and what it is served stay one fact.
  A query naming any sort key replaces the default chain; naming none leaves it
  standing, which keeps the default invisible until a reader diverges from it.
  Refusals are per token and name it: one column, one direction, so a negation
  (`-sort:x`), an alternation (`sort:a|b`), a column that is not there and a
  direction other than `asc`/`desc` are each a 400, as is a column named twice.
  DOCUMENT ORDER is a token too: `sort:*none*` is the EMPTY CHAIN — walk order
  whatever the limit, and no `sort` field on the wire for a renderer to
  re-apply — and it wears the stars because it is a reserved meta rather than a
  column, the family `*empty*`/`*archive*`/`*active*`/`*inactive*` already being
  in. It ADMITS NO COMPANIONS: another sort key beside it, or a direction on it,
  is a 400 naming the meta, two orders in one query being a reader who meant one
  of them. The half-typed `sort:` is no companion, naming nothing either way.
  `^` is that grammar's key: it composes the chain the way it always did — the
  column at point to the head, or a flip where it already leads — and now WRITES
  IT INTO THE QUERY, so the press is an ordinary commit. The URL carries the
  order, `DEL` walks the keys back off one at a time, a `?q=` link opens in the
  order it names, and the daemon is asked for the order the reader just stated.
  The shell keeps no record of a sort and asks the renderer for none.
  New module `Glance.Web.Sort` beside `Glance.Web.Filter`: one query, split by
  what a token does — narrow, or order — over one scanner.
- **The default order opens on state, in the tree's own cycle.**
  `defaultSortChain` is state, title, deadline, scheduled, all ascending, with
  state read by BADGE PALETTE position — which is the order your `#+TODO:` line
  spells — so the table opens with the work in org's order rather than
  alphabetically, and the title settles rows sharing a state. Priority left the
  chain: a fifth key behind four that have already separated nearly every pair
  of rows, and `sort:priority` is how to ask for it.
- **`a` carries its own order.** The agenda query is
  `state:*active* -planned:*empty* sort:scheduled`, so the whole canned view is
  one string: the server answers page one in that order, `DEL` walks out of it
  like any other token, and nothing has to be asked of the renderer once the
  rows are up.
- The tags list on `:` is a MUTABLE LIST (`#ttable`), where the property panel
  and the link popup are read one way and written another. Three
  columns, declared server-side in `Glance.Query.tagColumns`: the tag, its
  COVERAGE over the rows the command would run over (`all`, or `2/3`), and how
  many rows the whole tree has under it. `GET /tags` grew the third as `counts`,
  one pass over the store's rows per request — `stTags` counts FILES, which is a
  different question. A tag is its row's id, so a flag, the cursor and a rename
  all name the same thing after any number of writes.
  It replaces the which-key palette that carried this list, and the letters go
  with it: a keyword is a single word committed from memory and a tag over a set
  of rows is a RECORD a reader has to read. The tell was the muted `3/5` aside —
  a palette entry that needs a note about itself is a record wearing a letter.
  The which-key machinery is now the state palette's alone (`prompting.sticky`,
  `letterMode` and `prompting.letters` are gone with it).
  Gestures are the ones this page already spells, borrowed rather than invented:
  `d` flags a tag red and a second `d` — or `D` — removes every flagged tag from
  every target carrying it, one `remove-tag` per tag and the flags spent, which
  is dired's and the table's own archive gesture; `u` unflags; `+` raises the
  value palette's completing field over the addable vocabulary and adds; `RET`
  opens the tag cell as a field over itself, which is the property panel's edit
  overlay over one cell.
- `POST /command rename-tag {from, to}` — the seventh command, and the write
  behind that `RET`. `Glance.Query.renameTagEdits` REPLACES the entry where the
  author put it, so `:a:work:b:` renamed to `projects` is `:a:projects:b:` and
  the run's order, its delimiters and every other byte stand. It is a command
  rather than a `remove-tag` and an `add-tag` fired in turn because of what those
  two edit sets compose to. They APPLY — removing a LAST entry ends where the
  addition inserts, and `applyEdits` rejects only overlap — and they write the
  wrong thing twice over: the addition's anchor is measured before the removal,
  so a lone tag lands flush against the title (`* TODO Ship itprojects:`), and
  independently of the anchor `add-tag` appends at the run's end, so an entry
  with neighbours comes back moved to it. The pair would also be two writes under
  two digests where this is one drift-locked splice per file. One tag once: the
  first entry spelling `from` becomes `to`, further ones are cut, and a row
  already carrying `to` loses `from` instead. Both ends take the parser's charset
  wall, a row not carrying `from` costs no edit, and rename-then-rename-back is
  the identity on the bytes.
- The view declares a SORT CHAIN rather than one key (`declaredSort`, over
  `Glance.Query.defaultSortChain` — whose keys are the entry above, state
  leading). SCHEMA.md's `sort` takes an array for exactly this and both
  renderers run every key of it, so the keys behind the first fire only where
  two rows are alike on everything ahead of them. The browser draws the chain as
  a chip per key beside the filter's chips; `table-view.el` prints it on its
  hint line.
  ONE list, read twice — `declaredSort` spells it onto the wire and
  `sortedForViewWith` arranges the rows by it — which is the whole reason a
  producer sorts at all: a page cut out of a different order than the one
  declared is a different set of rows than the table would have put there.
  The arrangement is the renderers' rules, term for term: empty cells last on
  each key and OUTSIDE that key's direction (a blank is a fact about a cell,
  never about a row), the state column by its badge PALETTE position with
  everything unlisted tying at the back, a stable sort so rows equal on every
  key keep walk order. Text compares case-FOLDED, the way the tags cell
  already folds: the browser collates with `localeCompare`, which is
  case-insensitive at its primary strength, and raw code-point order would put
  every capitalised title ahead of every lowercase one where the table shows
  them interleaved. Titles differing only by punctuation or script can still
  land elsewhere than `localeCompare` would put them — the residue of having no
  collation library on this side.
  `sortedForViewWith`/`orderedForViewWith` take the state palette; the
  palette-free `sortedForView` derives one from the records it is given, which
  orders those records correctly and can differ from the store's in one case
  (two files declaring the same keywords in opposite orders, and a filter that
  hides every row of the first). A caller holding the store's palette should
  pass it.
- **This daemon writes a file into org-glance's `meta/` for the first time:
  `EXTERNAL.jsonl`.** Every write to a BLOB — a document under
  `.org-glance/data/` — appends one JSON line, `{"id", "at"}`, naming the blob's
  first headline's `ORG_GLANCE_ID` and when it was written, so org-glance can
  see that its index is behind and refold that entry. It is the answer to a
  measured problem: a live store went from 21 to 39 drifted rows in ONE DAY of
  browser use, the index having no way to learn about a write it did not make.
  ONE DOOR carries it — `Glance.Query.replaceSpans`, which is the only
  `editFile` caller and the way all four write paths leave — and the note rides
  the SUCCESS branch alone. `Data.Org.Edit.editFile` itself was rejected as the
  site: it is content-agnostic by law and a note is content. `Data.Org.External`
  owns the format, the path and the append, and `isBlob` decides which writes
  note at all — documents, config, overviews and occurrences note nothing.
  Append-only, hand-assembled field order (so the line is a contract rather than
  whatever a `ToJSON` instance emits), and a failed note is SWALLOWED: the
  rename already happened, and a write that succeeded must not be reported as
  one that did not.
  Found by the concurrency test rather than by review: `BS.appendFile` is NOT
  atomic — eight concurrent writes produced FIVE lines, `AppendMode` writing at
  the offset the handle was opened at — so the append is an `openFd` in append
  mode and one `fdWriteBuf`.
- `glance scan` folds org-glance's write-ahead index and says where it and this
  parser disagree: `org-glance index: 21 rows disagree (20 state, 1 archived)`,
  with the store, the fold's counts, the blob counts, and up to ten disagreeing
  ids carrying both values. Read only — the one thing here that opens
  `.org-glance/meta/` at all, and it never writes, creates or seals anything.
  The fold is `org-glance-graph--latest-records` term for term: the MANIFEST's
  sealed segments oldest-first, the open `headlines.jsonl` last, the latest
  record per `ORG_GLANCE_ID` superseding every earlier one, tombstoned ids out,
  only the open segment's final line forgiven for being torn. It compares the
  TODO keyword always and the archive flag only where the record carries the
  key — `archived` joined the record schema late, so absent is a third answer
  rather than false. Stores are each root's own `.org-glance/meta` plus every
  `meta` the walk declined, so a nested store is compared without a second
  traversal; a tree org-glance never indexed prints no line.
  ~/sync/views at 2026-08-02: 6502 records read, 6071 live, 0 tombstones, 0
  malformed; 6063 blobs parsed; 21 rows disagreeing; 0 unindexed blobs.
- The same report counts what the instrument cannot compare: `blobs 6063
  parsed, 51 carrying no id` is blobs this parser read and found no
  `ORG_GLANCE_ID` in, which with the 8 parse failures accounts for all 59
  `records without blobs` — so none of that number is org-glance indexing
  something that is not there. 28 of the 51 are one parser gap: a non-English
  weekday in the planning line (`CLOSED: [2025-12-04 do 22:34]`) fails the
  planning parse, the property drawer is then no longer the next thing, and the
  headline loses its properties whole. Reported rather than fixed; without the
  count it read as index lag.
- A row whose subtree holds a link wears an UNDERLINED title, so which rows `o`
  has something to follow is on screen before the press. `/headlines` and every
  streamed row carry `"linked": true` where there is a link and carry nothing
  where there is not (SCHEMA.md's Row, additive and sparse); the renderer
  underlines the `title` cell and leaves its colour alone, which keeps the mark
  clear of the four row washes and the two selection bands — a linked row under
  the cursor still reads as linked. It is every link `GET /links` would report
  rather than the references `ref:` matches, since that is what `o` follows:
  ~/sync carries 4976 linked rows against 1824 referencing ones. An underlined
  row whose only link is a `mailto:` still warns on commit — the underline says
  there is a link, never that a tab can take it.
- `^` sorts the table by the column at point, which is the cell selection's:
  `f`/`l` pick a column and `^` orders by it, echoing `^ → toggle-sort
  (Scheduled ▲)` and, past one key, the length of the chain. A whole-row
  selection is refused rather than guessed at (`no column selected — f/l to pick
  one`), a column that declares no `sortable` is left alone, and a held `^`
  counts as one press. What the press DOES with the order is the query entry
  above: it writes the chain into `?q=` and the daemon answers in it.
- Every column of `/headlines` declares `sortable: true`. SCHEMA.md makes the
  field opt-in and this producer opts all six in — a state cycle, a priority
  letter, a title, the tags, and the two dates all mean something in order —
  where `title` and `tag` had been left out. It is what a header click and `^`
  read; a producer's own `sortBy` never consulted it.
- `:` manages tags over the MARKED rows, or the row at point, under dired's
  normalize-up rule: a tag every target carries comes off all of them, one only
  some of them carry goes on to the rows that lack it, and a partial entry says
  how far it reaches. It stays up across its own writes and refreshes from what
  each one answered. `+` opens a completing field over the tags the set can
  still be given — the tree's vocabulary less what every target already carries
  — and takes a tag nobody has used yet as typed. The SURFACE it draws on is the
  mutable mount above, which replaced the which-key palette this landed as.
- `POST /command` takes `add-tag {tag}` and `remove-tag {tag}`, batched per file
  and answered per id like every other command; a tag the org parser would not
  read back refuses the whole request.
- `GET /tags?ids=…` reports what the named rows are tagged with and the whole
  store's tag vocabulary.
- `d`, `D` and `u` delete from the materialize sheet's property panel, the way
  they archive a row in the table: `d` flags, a second `d` or `D` deletes every
  flagged row, `u` unflags, and a held key counts as one press. A property is
  dropped and one of org's three planning rows has its entry cleared instead.
- The arrows step a cell as well as a row: `<left>`/`<right>` are
  `previous-column`/`next-column` beside `b`/`h` and `f`/`l`, and the key line
  is unchanged — an arrow rides behind its letters the way `<up>`/`<down>`
  always have.
- The page says when what is on screen has gone stale: one wash — faded back,
  never blurred — over the table and anything open above it, armed by a view
  fetch out past 300 ms or a socket down past 400 ms, and cleared by the answer
  or the reconnect. The status corner, the event log and the key line stay
  bright, being where a reader finds out why.
- **`make run`, `make run-native`, `make run-wasm`.** All three read `.env`
  (committed; `GLANCE_DIR`, `GLANCE_PORT`, defaulting to `~/sync/views` and
  7777). `run` opens the browser flow, `run-native` the WebKitGTK window
  through its own project file, and `run-wasm` builds the new
  `glance-wasm-probe` and runs the core inside wasmtime over the tree —
  walk, parse, rows — with the directory preopened read-only.
- **`P` pins the applied view as the tree's default.** The query on screen —
  filter tokens and sort tokens alike — becomes `system.org`'s
  `#+GLANCE_DEFAULT_FILTER:` line, through the same drift-locked `/config`
  write the settings sheet rides. The sheet's "default view" field is
  read-only now: composing a query belongs to the table's own widget, and the
  field shows what is pinned.

### Changed

- **`q` closes the materialize sheet**, the way it closes the app's window from
  the table. It stays a letter inside an open edit.
- **`g` keeps point where the default view still holds the row**, dropping to
  the first row only when it does not.
- **In an open paragraph, `RET` commits and `S-RET` inserts a newline.** The
  region is a value being handed back rather than a buffer being typed into;
  `C-x C-s` still writes it.
- **`b` walks the headline's cells leftward**, mirroring `f`, instead of leaving
  them in one press whatever the column.
- **The materialize sheet and every small list are drawn by Elm.** The sheet's
  document pane (`Doc.elm`) owns the structure scanner, the parse, the splice
  that composes a body back, the two-axis cursor and the grain ladder. One
  widget (`Listing.elm`) serves the four small lists — the property panel, the
  link popup, the tags popup and the settings sheet's states table — where each
  used to be its own table-view mount. The table itself is unchanged and stays
  the renderer's. Every key, cursor and flag behaves as it did: `n`/`p` and
  `f`/`b`, `RET` to edit, `SPC` to toggle a checkbox, `+` to add,
  `d`/`dd`/`D`/`u` to delete. `assets/elm.js` is a committed build input like
  the vendored renderer; `make elm` refreshes it and needs no installed
  toolchain, and `make elm-test` runs the scanner's own tests.
- **`P` asks which saved view the applied query becomes.** It pinned the
  default and nothing else; it raises the value palette over the saved-view
  registry now — one entry per view (`default`, `agenda`, `archive`), its
  which-key letter
  marked inside its name, and the query that view holds now beside it — and the
  letter commits, the way the state palette's does: `P d` pins the default,
  `P a` the agenda. `ESC` pins nothing. So the
  agenda `a` applies is set from the table where it is composed, and a view the
  server grows is offered with nothing on the page naming it. The chip strip's
  pin button asks the same question. The command is `set-saved-view` (was
  `set-default-view`), and the echo names the view it landed in:
  `P → set-saved-view (agenda · tag:work)`.
- **A saved view can be reset to its built-in.** `-` in that palette is a flag,
  magit's shape: it toggles, the same list stands under it, and with it armed
  `d`/`a` put that view's built-in query back instead of pinning. The write
  takes the tree's line off, and the page re-reads what the built-in then is
  rather than guessing. A commit closes the palette, so the flag never outlives
  the question it was set on. Pinning an empty query is the same write and now
  says so.
- `DEL` steps out of the value palette wherever no entry claims the key,
  which is what it already did over the link and tag popups. The state
  palette is unmoved: its `*empty*` entry claims `DEL` and still commits a null
  keyword.
- **The theme decides the TODO and priority badge colours.** They were four
  warm and four cool hues baked into the wire, the same in every theme — and on
  the light theme's golden cursor row a mid-amber `TODO` pill was unreadable,
  which is the collision: one file decides the row highlight, another decided
  the badge. The wire carries a SLOT now (`var(--g-state-a0)`) and the theme
  declares it, so switching theme repaints the badges with no refetch. The
  light theme takes the deep end of each family — hues that read as ink over
  white and over the selection alike — and dark keeps the palette it had.
- **A tree can name its own colour for a state.**
  `#+GLANCE_STATE_COLORS: light TODO=#7B1FA2 DONE=#00695C` in `system.org` —
  the theme first, `KEYWORD=VALUE` pairs after, one line per theme. A keyword
  named twice takes its last spelling, and a value the tree does not name keeps
  the theme's own. It costs the wire nothing: a badge's colour is a CSS
  fallback chain (`var(--g-state-TODO, var(--g-state-a0))`), so the override is
  a declaration the page emits and the badge picks up — theme switching still
  repaints with no refetch.
- **Every theme colour comes from one file per theme.** The palette used to be
  spread across three places — the page's `--g-*` set, the renderer's `--tv-*`
  set, and the hand-copied literals plus comments that kept the two in step.
  `Glance.Web.Theme` now holds a palette of ROLES per theme and emits both
  namespaces from it, so a role both spell has one value and the table is
  drawn in the palette the page around it is. Adding a theme is a record in a
  file beside the default theme's and one registry entry; the stylesheet, the
  boot script and the theme selector all read that list. Two light values are
  corrected on the way, the hue held: muted text (3.5:1 → 5.1:1 on white) and
  the focus accent (2.3:1 → 5.0:1) now clear the contrast floor on the page as
  they already did in the table.
- **The sheet's light-theme cursor row matches the table's.** The structured
  document's selection wash was the theme's golden `#FFD600` where the
  table's is the renderer's honeydew `#F0FFF0` — two hues for one meaning,
  side by side. `--g-sel` now copies `--tv-sel` on both themes (dark already
  agreed at `#373D4F`), so the cursor row reads as the same selection
  everywhere it appears.
- **The tag manager wears the band width.** Three short columns — the tag,
  its coverage, its store-wide count — sat in a sheet-sized box 80% of the
  viewport wide; the popup now takes `.pop-band`'s 560px and grows with its
  content to the cap, the state palette's own dress.
- **The `d`/`D`/`u` gesture is ONE implementation over THREE surfaces.** The
  table joined the property panel and the tags popup on `flagKey`, which now
  owns the whole gesture: the cursor read, the two-press rule, the set-or-row
  choice, the spending of the flags before the take, the feature refusal and the
  walk after `u`. Gone are the table's own `archiveFlag`, the fork inside
  `archive` choosing between the flagged set and the row at point, and the flag
  branch inside `mark`. A surface DECLARES a shape — its mount, its cursor as an
  id, what "take these" means, what it LOGS when a flag moves, its walk, and four
  phrases — and WHO SPEAKS belongs to the caller rather than to the shape: the
  popups say `KEY → phrase` out of a listener holding no binding, the table says it
  through `said`, so `d` and `D` echo `archive-flag` and
  `org-glance-overview:delete` out of one gesture. Every echo is
  byte-identical to what it was. Two rules moved and are now uniform: the CURSOR
  is asked for before the FLAGS (so `D` on a renderer that never had flags takes
  the row at point on every surface, where the popups used to refuse), and the
  flags are SPENT inside the gesture rather than by each take. `u`'s
  flag-before-mark stays in `mark`, named as the table's own asymmetry: over the
  popups `u` is the flag key, over the table it is the mark key preferring a
  flag.
- **An edit overlay names its cells BY KEY.** A shape carries `cells: ["title",
  "url"]` beside the `cols` list the server declared, and `cellSpan` — pure and
  order-only — resolves the keys to the leftmost and rightmost indices the
  placement reads. Replaced a positional pair (`cells: [1, 2]`) with nothing
  tying it to the list it indexed: reordering `Glance.Query.linkColumns` put the
  box over the wrong cells, greenly. A key no column carries resolves to nothing
  and the placement is a NO-OP. The suite drives the resolution as the pure
  function it is, against the SERVER's own column declaration.
- **The modal surfaces are one ordered list.** `SURFACES` names the property
  panel (whose listener registers ahead of the dispatch), then the value
  palette, the link popup and the tags popup, in the order their listeners are
  written — rank IS registration order. Three readers where there were three
  restatements: `typing()` asks whether any is up, `cancel` walks the list for
  the rung `ESC` belongs to, and a listener asks `covered(NAME)` whether anything
  above it is up. The five listeners stay, and so does `prompting.raising` —
  `covered` is one surface declining for another, `raising` is one surface
  declining the keydown that raised it, and a rank says nothing about a race with
  one surface in it.
- **Every route resolves the store's ids ONCE, at its own door.**
  `Glance.Web.Store` no longer offers anything that takes a `Store` and answers
  about an id: `storeHeadline` and `storeHeadlines` are gone and `headlinesIn`
  takes the RESOLVED rows. `storeRecords` resolves the whole store each time it
  is named (~28 ms over a 10435-row tree), so the old shapes let a route owing
  two folds pay twice and a fold-per-id spend seconds over a marked set. It is a
  structural rule now rather than a convention, which retires the
  `TestSelfContained` grep that guarded `/tags`'s shape by reading its source
  lines — and the `codeOf` helper with it.
- **`csOne` folds into `csArgs`.** A command spec's shape check is handed the
  IDS beside the `args`, because a shape refusal is about the REQUEST rather than
  about the `args` object alone. Seven of the eight commands ignore the list;
  `edit-link` owns its own "names one row" message and puts it FIRST, the row
  count being the coarsest thing wrong with a request. One flag fewer for every
  entry to answer.
- `Data.Org.Walk.derivedDirs` names `Data.Org.Index.metaDir` rather than
  spelling `"meta"` a second time: that module owns the store layout, and a walk
  declining a directory the index no longer wrote to would be excluding nothing
  while reading as though it were.
- After `d`/`D` archives rows out of the view, point lands on the NEXT SURVIVING
  ROW rather than resetting to row one. dired's rule, and it needed two changes.
  THE ANCHOR: `anchorFor` takes it at FIRE time, since by the time the rows have
  gone the gap they left is exactly what a later read cannot see. It scans from
  POINT — down the page for the first row not leaving, else back up for the
  nearest one, else nothing at all — and carries `from` (the row point was on),
  `id`, `at` (the anchor's place among the SURVIVORS, the fallback for the
  anchor itself vanishing before the landing) and `on` (the page it was taken
  on). THE DOOR THE ROWS LEAVE BY is the FILTERED REFETCH behind the 250 ms
  debounce: `archive` puts an UPSERT on the wire — `Store.streamed` emits a
  delete only for an id absent from the store afterwards, and adding `:ARCHIVE:`
  leaves the row emitted under the same id — so an UNFILTERED client splices the
  row straight back in and point does not move at all. `resync`'s repaint is the
  only other, for a socket that was down while the write landed. All three call
  `settled`, which ALWAYS SPENDS the anchor and lands it only where something is
  owed: spending unconditionally is what keeps it describing ONE watch step,
  where an anchor left armed would let a page turn and somebody else's edit
  minutes later pull the cursor to a row this write had an opinion about. It
  declines to land while `from` is still in the view, and on any page but the one
  the anchor was taken on — `visible()` is ONE PAGE and can say nothing about a
  row outside it. `spent(mine)` drops the anchor when the answer says `from` was
  not archived (a refusal, and an archive over a set point is not in), keyed to
  the anchor it answers for so an earlier archive's answer cannot disarm a later
  one's, and deciding the anchor before `unmark`, which can throw on an asset
  carrying half the mark calls. A `commit` and a `remount` drop it outright: an
  anchor belongs to the view it was taken in.
  THE CARVE: `fetchRows` takes the landing as an argument and the watch's refetch
  passes `settled` where a commit passes nothing. A refetch is the view the
  reader already had arriving again because a file moved, so it is not a new
  question and lands nothing of its own — the renderer keeps the cursor and only
  an armed anchor overrides it. Before this, ANY watch event under a filter took
  a reader back to row one, which is the larger half of what this fixes.
  `land` grew the fallback index that makes the three landings one function
  (apply → row one, pop → the drill's row, archive → the anchor), so the
  first-row rule is now the general rule's default rather than a case beside it.
  What the anchor buys over the renderer's own `keepSelection` is the case where
  rows went from ABOVE point too: that keeps the visual PLACE, which is a row
  further down once they have gone, so it skips one. Its other branches — the
  up-scan, the empty view, a point row that survives — agree with the anchor
  exactly and are guaranteed twice, so nothing exercises the up-scan alone.
  The shell harness grew the socket path to prove it: `frame:upsert=IDS` and
  `frame:delete=IDS` deliver row frames through `socket.onmessage`, the page's
  own door, and `unserved:IDS` drops rows out of what `/headlines` answers — an
  archive being an upsert on the wire and an absence in the answer. The table
  mount grew `upsertRow`/`deleteRow` and models `keepSelection` verbatim,
  including its stale visual index, which is what makes the two halves separable
  at all. Sixteen cases in `TestServe`'s "Shell landing".
  FOUND HERE, FIXED UNDER Fixed below: a freshly mounted table has NO selection,
  the renderer's `selectFirstVisible` having one caller and it being the filter
  box handing over — so `d`, `D` and `RET` on a just-booted page said "no row"
  until the reader pressed `n`, and the harness answering `getSelection` with
  row 0 of the page is why the suite never saw it. The boot now takes the apply
  landing through this same `land`, and the stub models the empty selection.
- The settings sheet's keywords panel is ONE select over ONE box. It showed a
  `<textarea>` per config layer, stacked in `#clayers`, and a tree has as many
  config files as it has tags — the stack was as tall as that number, so the
  reader scrolled past every layer they were not editing to reach the one they
  were. It is now one native `<select id="clayer">` over the layers and one
  `<textarea id="ctext">` holding the SELECTED layer's `#+TODO:` lines verbatim,
  with `#clab` naming that layer (`system · PATH` / `tag · book · PATH`, plus
  ` · not created yet` where the digest is empty) and `#clerr` carrying whatever
  the server last said about a write to it. Order in the select is system first,
  then the tag layers by `localeCompare` (`byLayer`); `sort` is stable, so two
  system layers keep the order the server served them in, which is the walk's.
  The text lives on the LAYER (`crows[i].text`) and the box is a view of
  `crows[cat]`: `takeLayer()` copies the on-screen box back into its layer and
  every door calls it first — the select's `change`, `cdirty`, `flushConfig` —
  so an edit outlives every switch and a switch asks the server nothing. Sync
  semantics are unmoved: buttonless, `ESC` or the backdrop syncs the layers that
  moved and closes, a pristine sheet costs no request, `C-x C-s` syncs mid-edit,
  `conflict` and `error` wait for a keystroke, and it is still one drift-locked
  `POST /config` per FILE that moved, each awaited, each under its own digest.
  NEW: a refusal brings its layer with it — `flushConfig` remembers the first
  refused layer's index and selects it, so the box on screen is the file the
  message under it describes; every refusal is also a `config error` log line
  naming `SOURCE · PATH: message`, since only one can be shown. `SECTIONS`'s
  keywords entry is unchanged (`clayers`, `ceff`, `cfoot`) and its body is still
  markup the list wraps at boot; `.ctext` grew `height:3.4em` → `7em`, and
  `#clayer` shares `#themesel`'s select rule.
- A blob's occurrence history is no longer walked. org-glance snapshots a
  completed repetition as `.org-glance/data/<id>/occurrences/<STAMP>.org`, an
  immutable copy carrying the LIVE entry's `ORG_GLANCE_ID`; it sits inside
  `data`, so keeping `data` kept it, and `isCanonical` ranked it canonical for
  the same reason the live blob is — `beatsForId` called the pair a tie, walk
  order decided which one the table showed, and `POST /headline` would have
  written to whichever won. `Data.Org.Walk.isOccurrence` is the rule and
  `isDerived` covers it, so the watch declines it through the same predicate a
  file the walk never collected is declined by, and `isCanonical` excludes it so
  that under `--include-derived` — which walks it — it loses the id rather than
  tying for it. The name is asked for anywhere under `data`: a two-character id
  is unsharded, so no position test covers both layouts, and the cost is that a
  blob whose sharded remainder spells exactly `occurrences` would be declined
  too. Zero on disk under `~/sync`, so the corpus counts do not move and the
  hazard is closed before it is reachable.
- The link list `o` raises is a READ-ONLY LIST, where it was a which-key
  palette. Three columns — `type` as a badge, `title`
  as the entry's own description, `url` as the target — and the whole surface is
  `n`/`p` (`j`/`k`, the arrows) to move, `o` to open the link at point, `ESC` to
  leave. The mount is stated read-only: no marks, no flags, no page, no hint
  line. The doctrine it lands is a division of labour between the two shapes: a
  WHICH-KEY palette is for a fixed vocabulary a reader commits from memory (a
  keyword, a tag), where the letter IS the confirmation; a READ-ONLY MOUNT is for
  a list that has to be READ before it can be picked from, where letters are
  noise over the columns carrying the answer. So `t` and `:` keep their letters
  and the links lose theirs, `/` narrowing with them.
- `GET /links` gains a `type` per link: the target's SCHEME, lowercased, with
  the whole `org-glance-*` family folded into `glance`
  (`Glance.Query.linkType`). `https`, `http`, `mailto`, `id`, `file` and
  `glance` are the six the corpus spells and the six the popup declares badge
  hues for — the two a tab can follow warm, the four it cannot cool — and a
  scheme those six do not name travels under its own name rather than being
  flattened away — a 300-row sample of ~/sync answers `glance` 427, `https` 286,
  `file` 68, `http` 18, `elisp` 6, `attachment` 2 and `other` 1, so two types
  nothing declares came back named. A target with no scheme-shaped word before a `:` is `other`,
  which is org's internal `[[Title]]` and `[[*Title]]` and a relative path
  written without `file:`. The honest cost of reading the prefix alone: `[[Meeting:
  notes]]` reads `meeting`, because the alternative is a registry and then an
  unlisted scheme would read as prose. `followable` is now that word rather than
  a regex the page ran over the target a second time.
- The `tag` COLUMN sorts, case-folded (`Glance.Query.sortedTagsCell`):
  `:task:nl:finance:` reads `:finance:nl:task:`, so a tags cell is scanned in
  one order rather than in the author's typing order. Display only. The FILE
  keeps its spelling — the span is untouched, so materialize and the tag edits
  splice into the run as written — and so does `hrTags`, which is what
  `classify` reads and where the order DECIDES which tag's config governs the
  row. `hrSearch` inherits the sort by construction, `GET /tags` and the tag
  palette's first-seen union do not, and no predicate changes answer: `tag:x` is
  a substring of one tag and `tag:*archive*` is membership of the list.
- The materialize sheet's two panes wear one radius. `#mtext` was 4px against
  the panel's `.tv-root` 8px; 8px is the page's shared value, which the log strip
  and the sheet's logbook already wear.
- **BREAKING: combination is one rule — TOKENS AND, ALTERNATIVES OR.** Every
  `?q=` token narrows, whether or not another token names its key. `tag:a tag:b`
  is a row carrying both and `ref:a ref:b` one pointing at both, as before;
  `state:TODO state:DONE` now asks a cell holding one value to hold two, which is
  no row, **where it used to answer either state**. The replacement idiom is the
  new alternation: `state:TODO|DONE`. A predicate's VALUE splits on `|`
  (`Glance.Web.Filter.alternatives`) and each alternative is read as that key's
  own value, the results OR'd — uniform over every key and every kind of value,
  so `tag:work|home` carries either, `scheduled:2026-08|2026-09` is either month,
  `planned:A|B` is either date cell prefix-matching either, `ref:a|b` points at
  either, and a starred meta alternates like any other value
  (`state:*active*|DONE`, `tag:*web*|*archive*`). A negation covers the whole
  token, so `-tag:a|b` carries neither. **A saved URL or bookmark spelling a
  same-key OR now answers nothing; rewrite it with `|`.**
  Empty alternatives are DROPPED — `a|` is `a`, `|a` is `a`, `a||b` is `a|b` —
  and a value spelled with bars alone is left with none, which narrows nothing:
  one answer for `key:`, `key:|` and `key:||`. The bar is a PREDICATE's: free
  text is the text it spells, bar and all, and a predicate's value has had its
  quotes taken out by the scanner, so a literal bar is free text's alone.
  `namesArchive` reads the alternatives too, so `tag:*archive*|web` lifts the
  archive exclusion the way `tag:*archive*` does.
  What it buys is the arity rule's death: `multiValued` is gone, `compile` is
  `map inverted` over the terms with no grouping in it, and the `multi: true` the
  view declares is left saying only what its name says — the cells hold a list,
  which the whole-tag meta and the renderer's chips read. Parity is kept term for
  term (`table-view.js`'s `queryMatcher`/`tokenTest`), and the shared
  `fixtures/parity/filter-query.json` gains the alternation cases.
- **BREAKING: the empty cell is `key:*empty*`, and `key:none` is a literal
  value.** The bare word reserved a spelling a cell can hold, and that was
  exactly its cost: a state keyword `NONE`, a tag `none`, a title reading `none`
  were unreachable by predicate. The stars carry the meaning now and the word
  carries none — `state:*empty*` is the stateless row, `state:none` is a keyword
  spelled `NONE` — on every column key and on `planned`, so the agenda's query
  is `state:*active* -planned:*empty*`. **A saved URL or bookmark holding
  `key:none` now reads as an ordinary value and matches whatever holds that
  text, which is usually nothing.** No alias, no migration: the point is that no
  bare word is reserved.
- **BREAKING: the archive exclusion is lifted by `tag:*archive*` alone.**
  `tag:archive` is the ordinary substring predicate every other tag value gets:
  it filters, it lifts nothing, and `X-Glance-Archived` still reports what the
  default view withheld from it — so a tree that uses `archive` for something of
  its own can filter on the word without being handed the rows it files away.
  The meta matches the WHOLE tag, where the plain predicate is a substring of
  the tags cell (`:archived:` answers one and not the other). **A saved
  `?q=tag:archive` link stops showing archived rows**; add the stars. Over
  ~/sync at 2026-08-02: `tag:*archive*` serves the 322 archived rows,
  `tag:archive` serves 0 and reports all 322 withheld.
- **BREAKING: `state:active` / `state:inactive` are literal keywords.** The bare
  alias for the two group metas is gone with the rest of the bare words — and it
  was a parity divergence in its own right, since `table-view.js` never had it
  and matched those tokens as badge text. `state:*active*` and
  `state:*inactive*` are unchanged, and they are what the default view, the
  agenda and the state column's `values` have always spelled.
- **A starred word on the `tag` column is that whole tag.** `tag:*book*` is the
  tag `book` where `tag:boo` is any tag holding those letters — the whole-tag
  reading that left with the virtual tag keys, back as a meta on the one
  spelling, decided off the cell so the renderer answers it identically.
  `tag:*archive*` is one instance of it rather than a rule of its own.
- The state palette's take-the-keyword-off entry is `*empty*` rather than
  `*clear*`: it takes the state cell to exactly what `state:*empty*` then finds,
  which is one word for one thing. `DEL` is still its key, the commit is still a
  null keyword, and the log line is still `state cleared`; the pill now says
  `C-c C-t → org-glance-overview:todo (*empty* · 1)`.
- `assets/table-view.js` is resynced from the sibling checkout (`make
  sync-renderer`), which carries the renderer's half of all of the above —
  `*empty*` on every key, the whole-entry meta, `*empty*` at the foot of every
  value domain, a meta taking no sort position — and closes a skew that predated
  this work: the vendored copy still had the virtual tag keys the server dropped,
  and lacked `sortBy` and the title-offer tiers.
- The `tag` column declares `values: ["*archive*"]`, SCHEMA's route for a
  producer meta, so a renderer can offer it: typing `arch` in the filter box
  reaches `tag:*archive*` the way `act` reaches `state:*active*`.
- The settings sheet `,` raises is the page's one place for a preference, in
  three panels: **general** (the capture target and the log height),
  **theme**, and **keywords** (the per-layer `#+TODO:` boxes, which were the
  whole sheet). One list names the headers and what sits under each, so a
  fourth panel is an entry there; the list order is the tab order. Every sync
  rule is unmoved — buttonless, `ESC` or the backdrop syncs the layers that
  moved, `C-x C-s` syncs mid-edit, a conflict waits for a keystroke — and the
  two general fields still ride the system layer's own write.
- The theme selector moved out of the status corner and into that sheet's theme
  panel. Same `auto`/`light`/`dark`, same `localStorage`, same pre-paint boot,
  and it applies as it is picked without closing the sheet.
- A view now swaps on its answer: `g`, `a`, `@` and a walk back out of a drill
  ask for the whole set once and put it up in one mount, so a complete table is
  no longer replaced by a page of rows and reflowed a moment later. The
  page-sized first fetch stays where it earns its keep, on the boot.
- The event log spells its severity in upper case — `14:03:22 INFO cmd …` —
  which is what a reader scans a screenful of chatter for.
- Whichever pane of the materialize sheet holds the keys says so on its own
  frame, so crossing with `TAB` moves one mark rather than losing it.
- The property panel is a LIST WIDGET rather than markup of its own: the rows,
  the stripe, the cursor and the flag wash are drawn for it, and the panel keeps
  the model alone. The sheet's edit fields now sit over the row
  they belong to rather than inside it.
- `@` asks before it applies: a row nothing refers to leaves the table, the
  filter and the trail exactly where they were, with one log line saying so.
- `@` out of an empty filter leaves no crumb — "all rows" already is the empty
  query, which `DEL` reaches without one.
- The value palette drops its key-token column: a keyword's committing letter is
  marked inside the word, bold and underlined in that state's own badge colour.
- The take-the-keyword-off entry commits on `DEL` instead of claiming a letter,
  so the whole `a`-`z`
  pool goes to keywords and a wide cycle keeps the letter the entry used to take.
- The row's search text is DERIVED from `viewColumns`: a column's cell is now
  `HeadlineRecord -> Maybe Text`, `rowJSON` encodes it (`Nothing` is the same
  `null` it always sent, `Just ""` the same `""`), and `recordOf` ties the record
  through `viewCells` instead of writing the six cells out a second time. What
  went green before this is an APPEND — a seventh column left the haystack six
  fields long and every predicate past it reading the wrong field — and it is
  closed by construction plus a `TestFilter` case quantified over the columns
  there are. A reorder was already caught, by the layout guard, whose hardcoded
  list stays as the now-real oracle. Byte-identical over ~/sync: 12594 rows of
  view JSON unchanged.
- `Glance.Web.Filter` reads a predicate's CELLS as a set (`fieldCells`): a column
  is its one cell and `planned` is the two date columns, so `*empty*` is every
  named cell empty and a value is any of them passing — one arm where the virtual
  key had a matcher of its own. The whole-tag meta stays keyed by cell index, so
  `planned` can never reach it.
- `namesArchive` drops its vocabulary parameter: `/headlines` already asks
  whether the tree carries the tag, and asking twice was the same conjunct twice
  (`V && not (V && N)` is `V && not N`). The claim it used to state moved to
  `TestServe`, over a tree with nothing archived, where it is a fact about the
  answer rather than about the parser.
- `POST /command` is ONE table, name to `{argument shape, dated, edits}`:
  `commandNames` is its keys, `parseCommand` resolves the name before anything
  else and builds a `Command` out of the entry it found, and the per-name guards
  are each command's own `csArgs`. The wildcard that made an unknown name
  `archive` is gone with the case it lived in — the edits are read off the entry,
  and the one command with no row function is the one that makes a row. Every
  refusal message is unchanged, verbatim.
- The HTTP route table declares its METHODS: each entry carries the handler per
  method and how it spells a 405, `HEAD` aliases `GET` in one place, and the JSON
  refusal sentence is derived from the entry's own method names. CLAUDE.md's
  "fixed route table, each entry declaring whether it needs a loaded store and
  whether it is read-only" is true as written now. The 405 surface is byte-
  identical over ten method/path pairs.
- ONE BUTTONLESS SHEET drives both the materialize sheet and the settings sheet:
  one state word per sheet through one writer, one `C-x C-s` ladder, one
  ESC/backdrop ladder, one backdrop registration, and the `C-x C-s retry · ESC
  discard` line spelled once where it had three copies. Each sheet supplies
  `{dirty, flush, refresh, shut, scope}` and nothing else; `activeSheet()` is
  total, since neither sheet opens over the other. Behaviour is unchanged — the
  harness drives both sheets through pristine, dirty, conflict and discard.
- **The vendored renderer's stylesheet is one declaration per line**
  (table-view `cc9dd70`, synced). Same rules and values; a re-layout.
- **The shell's script is a real JavaScript file.** 5.2k lines of JS lived as
  a Haskell string list for the sake of nine interpolations; `assets/glue.js`
  is now a file compiled into the binary the way the renderer is, with every
  server value riding one `cfg` JSON blob the page emits (eight per-build
  constants and the per-request default view). Editor modes, linters and
  honest diffs apply to the shell for the first time, and `--assets` gives
  live glue hacking with no rebuild. The migration is byte-proven: the old
  output and the new file differ in exactly the nine known lines.
- **`DEL` takes the sort chip whole.** The chain used to give up one
  tie-breaker per press (`sort:title->priority` → `sort:title`); a chip
  erasing by a different rule than its neighbours made `DEL` a thing to
  think about. One rule now: the last chip goes whole, sort or not.
- **The settings sheet's default view is the main page's own filter widget.**
  A table-view composer — the omnibox bar and the chips, completion and DEL
  included, with no table behind them — replaces the plain field. It opens
  showing the served value, offers the tree's own values, and a composed
  query rides the system layer's drift-locked write.
- **The material document walks on two axes.** `n`/`p` step siblings at the
  cursor's grain and never dive — a list, block or table is ONE stop, so
  holding `n` skims the document at reading grain — and `f`/`b` move the grain
  itself: `f` enters a composite's leaves or a headline's cells (and refuses,
  with an echo, at the finest), `b` re-selects the whole in one press and is a
  spoken no-op at the element grain — never a close. `l`/`h` and the
  horizontal arrows keep the within-grain cell walk. The table's own habit —
  `n`/`p` rows, `f`/`b` cells — now serves both surfaces; the earlier one-walk
  grain (where `n` stepped into every item) is retired.
- **Capture is one form.** `+` used to run a chain of palettes — tag, each
  template prompt, the line — and every step closed and reopened the overlay,
  which read as a blink. One popup now holds the whole flow: the tag field
  with the vocabulary narrowing under it, the template's fields grown in place
  when the tag settles (RET or TAB), and the line last. RET moves forward and
  captures at the line; ESC anywhere leaves with nothing sent; a refusal keeps
  the form up with everything typed, so fixing a line is an edit rather than a
  retype.

### Removed

- **BREAKING: `?order=` is gone from `/headlines`.** `?order=document` and
  `?order=scheduled` were the ordering's own parameter, and the ordering is the
  query's now: **`?q=sort:*none*` is the replacement for `order=document`, and
  naming nothing is the replacement for `order=scheduled`.** The parameter is
  REFUSED rather than ignored — any `order=` at all is a 400 naming its
  replacement — which is exactly why it was spelled out in the first place: one
  silently dropped would serve the default order and read as a working request.
  Gone with it are `pageParams`' `ordering` arm, its two words, and the base
  parameter of `Glance.Web.Sort.sortChainIn`, which now reads
  `defaultSortChain` itself and is a function of the query alone. Neither the
  shell nor the agenda ever asked for `order=`, so nothing on the page changes.
- The status corner is gone whole. `#corner` carried the connection dot (`#dot`
  with `.live`/`.wait`/`.down`) and the coarse-pointer settings gear (`#gear`,
  its `display:none` and the 44px rule in the `pointer:coarse` block); swept with
  them are `const dot`, its four call sites (`socket.onopen`, `socket.onclose`,
  `indexing`, `start`'s catch), the gear's click handler and the
  `#corner`/`#corner:hover`/`#dot*`/`#gear` CSS. The socket's state was already
  said twice over — the stale wash (the whole page fading back once a socket is
  gone, armed at 400 ms) and the strip's own `ws` lines — so a dot was a third
  spelling of one fact, and it cost a fixed box, a z-level and a top padding to
  keep clear of. The indexing state is the strip's `boot info` line alone.
  Consequences: the body's padding goes `34px 24px 24px` → `24px`, so the table
  starts where the page does; the z-index bands are THREE rather than four (echo
  `2`, modal backdrop `100`, sheet `101`, with `3` unused and forbidden by the
  suite); and the stale-wash exemption list is now the event strip and the key
  line. KNOWN GAP, worth stating rather than burying: the gear was the coarse
  pointer's ONLY door to the settings sheet, `,` being untypable there. A touch
  reader can filter and read; they cannot open the settings, and the page has no
  other affordance to offer them. The `pointer:coarse` block keeps its other
  rules — the 44px chip row, its empty-state label, the stacked sheet panes and
  the 16px fields — and the comment owning the question lives inside that block.
- Virtual tag keys leave `?q=`. An org tag no longer names a filter key:
  `course:text` is free text, colon and all, and `tag:course text` is the one
  spelling — the predicate reads the tags cell, the free text reads the row, and
  nothing expressible is lost. It kills the worst parity divergence the grammar
  had: the keys a query could name were the WHOLE STORE's tags here and the
  LOADED ROWS' tags in `table-view.js`, so one token was a predicate on one side
  of the wire and free text on the other. It also takes `contact:none` with it,
  which meant "tagged contact and the row text holding none" and read like the
  empty-cell rule it was not — a rule now spelled `key:*empty*`, see Changed.
  Two differences are written down rather than papered over: `tag:` matches its column by SUBSTRING where a tag key matched
  whole-tag (`tag:glan` finds `:glance:`), and org spells a tags cell `:web:`,
  so the free text `web:` is still inside every row carrying the tag.
- The archive exclusion is named through the `tag` column rather than by
  `archive:` (`Glance.Web.Filter.namesArchive`), the archive tag having been an
  ordinary virtual key. Any spelling of that predicate counts — negated, quoted,
  beside other tokens. (The spelling is `tag:*archive*` as of the meta entry
  under Changed below; it was `tag:archive` when this landed.)
- `Glance.Web.Filter` sheds the machinery the feature alone consumed: the `Tag`
  field constructor and its arity, `parseFilter`'s vocabulary parameter,
  `FilterEnv`'s tag list (`tagsEnv` is now the tag-free `emptyEnv`), and the
  `tagsOfCell` import. `Glance.Web.Store.storeTags` stays — it is `tag:`'s value
  domain, the tag palette's vocabulary and `namesArchive`'s "is anything
  archived" guard.
- **The settings sheet no longer edits the saved views.** Its general panel
  carried a filter composer and a select naming which view it stood on; both are
  gone, and the panel is the capture target and the log height. A query is
  written where a reader already composes one — the table's own filter, with its
  completion, its chips and its `DEL` — and `P` is what saves it. Gone with the
  widget: `#cwhich`, `#cfbox` and its two style rules, the composer mount, the
  `views` region of the settings sheet's own `/config` write, and the shell's
  `vrows`/`ViewRow` bookkeeping (the live views are one map keyed by the
  registry's ids now).

### Fixed

- **The paragraph editor grows with what you type,** to ten lines, and the
  document moves down around it rather than being covered — the edit reads as
  inline. It was the height of the block it covered, so a second line went out
  of sight.
- **A flag in the materialize sheet is dressed the way the table dresses one.**
  The document pane drew one in the warning orange at a strength of its own,
  so the same gesture over the same queue looked like two different things.
  It now takes `--tv-flag`'s hue at the theme's own measured wash, with the
  renderer's inset left edge — so a flagged row under the cursor still says
  it is flagged.
- **Closing the capture form takes `?page=capture` off the URL.** It opened
  through the shared popup door and closed around it, so the parameter the
  raise wrote was never taken off: a URL copied afterwards reopened a form the
  reader had left.
- **A tree's saved default filter is applied even when the page opens during
  indexing.** The shell renders while the walk runs, so a page served before it
  landed carried the built-in `state:*active*` and nothing re-read it — the
  reader's own view arrived only on a manual refresh, which a native window
  cannot do.
- **The link popup's `Type` values wear their badge colours again**, and its
  `Headline` column is now `Title`.
- **The materialize sheet's state badge wears the theme's colour again**, and
  the priority badge now wears its own — it never had one.
- **Re-opening the sheet draws its document.** Closing it used to take the
  pane's contents away for the rest of the session.
- **Setting a state on a headline that had none now shows in the material
  sheet.** The `/command` wrote the file and the re-read behind it took the
  store's copy, which the watch had not refreshed yet — so the pane redrew the
  entry exactly as it was before the write. The stale-answer drop refuses that
  reading and the retry behind it brings the real one.
- **The property panel (and the tags popup) lost the empty leading column.**
  The renderer's gutter served both the checkbox and the flag's inset edge, so
  a mount that flags without marking — the panel, the tags popup — paid a
  blank 3ch column for an edge. The gutter is the checkbox's alone now; the
  flag's edge rides the row's first cell, and the key column starts flush
  left. Marking tables are pixel-identical, their first cell being the gutter.
- **A property just added is a full-height row while it is edited.** A `+` in
  the panel opened the edit over a row whose two cells were still empty, and
  an empty cell forms no line box — the row collapsed to its padding and the
  overlay anchored to its rect squashed with it, springing to size only on
  RET. The renderer holds the line now (a zero-width space after every empty
  cell), so the fresh row stands as tall as its neighbours from the first
  frame.
- **A cell edit from the sheet no longer poisons its digest.** A `set-title`,
  state, tag or priority write from the materialize sheet goes through
  `/command`, whose per-id 200 carries the file's new digest — but the sheet
  kept the old one until the watch frame re-read it, and that re-read is
  guarded off under an open edit or the panel's keys. Every subtree commit
  inside the window — a checkbox, `C-x C-s`, the panel's flush — 409'd at
  `conflict` for the reader's own landed write. The sheet now re-pins off the
  command's own answer, the tags popup's documented rule one surface over.
- **Org links no longer flash raw on a sheet refresh — the links ride the
  materialize now.** They travelled on a second request (`GET /links` beside
  `GET /headline`), so every fill had an async gap and the frames in between
  drew `[[url][desc]]` where the reader had been reading `desc`. The gap is
  gone structurally: the materialize answer carries the row's whole link scan
  beside the text it describes — one request instead of two, links atomic
  with their text, compact from the first frame on every fill — and the
  element's `o` opens off the held answer without asking the server. `/links`
  stays as the table popup's and `edit-link`'s route, built by the same
  `linkJSON` the materialize rider uses.
- **An element commit no longer reverts the sheet to the store's stale copy.**
  The re-read a successful commit fires reaches the store before the watch
  has re-parsed the file, so it answered with the PRE-write subtree — the pane
  flipped back to what the file just stopped saying, and the stale digest it
  carried poisoned the sheet's pin, so the NEXT write landed at `conflict`.
  A body-only edit emits no socket frame, so nothing ever corrected either.
  The reload now drops any answer whose digest is not the write's own receipt
  — the model the write was built from stands, redrawn — and retries once for
  the server's canonical reading after the watch has caught up. Found by the
  checkbox toggle, whose flip made the revert visible; it guarded every
  paragraph, table-line and deletion commit the same way.
- **`set-planning` takes `+1y`.** Org spells four units and its parser reads all
  four, but this one reader hand-wrote three — so `+1y` parsed everywhere and
  was refused here alone, by a message that did not mention `y` either way.
  `unitOf` reads `unitChar` backwards, so the parser's units and this reader's
  are one list, and the refusal sentence is derived from it.
- **A repeating entry whose date org did not zero-pad now repeats.** `<2026-08-8
  Sat +2d>` is a timestamp org reads — its parser takes each part as a run of
  digits — but the shift scanned a fixed ten-character window, missed the date
  entirely and wrote the stamp back unmoved. The entry reset its keyword and
  stayed due, forever. The scanner reads the date the way the parser does now.
- **A linked title shows its description alone.** The material headline drew
  the raw `[[url][desc]]` as the cell's own text and appended the rendered
  description beside it, so a browser showed both. Exactly one path writes
  the cell now; the harness models the browser's reading (own text plus
  children) so the double can never go green again.
- **A held key is one press, even when the event lies.** WebKitGTK's
  auto-repeat can arrive with `repeat` unset, which disarmed every
  once-per-press guard in the native window — a held `DEL` stripped the whole
  query, and a held `d` could flag and archive in one press. Repeat is now
  derived from the missing key release, whatever the event says.
- **The pin actually writes.** Two faults hid each other: the server still
  required `lines` on `POST /config`, so the pin's request was a 400 — and the
  shell's fetch resolves refusals, so the pin logged "pinned" while the file
  never moved. Absent `lines` now leaves the `#+TODO:` block standing, and a
  refused pin is a thrown error and one config error line.
- **`f` recurses into a nested list.** A deeper item used to ride inside its
  parent as opaque text, so the grain stopped one rung short. The grain is a
  LADDER now: an item carrying a nested run is itself a parent — `f` descends
  one rung, `b` climbs to the immediate owner, `n`/`p` clamp to one parent's
  run, a flag on any rung deletes its whole range, and the draw shows each
  rung inside the one above it.
- **A link listed under two descriptions serves both.** `/links` deduplicated
  by target alone, so one `elisp:` command written under `pnl` and under
  `alpha:grafana` served the first and silently swallowed the second — which
  read as the link not parsing. The dedup key is now the (target, shown) pair a
  reader can see: repeats under one look still collapse to the first spelling
  (and its span), distinct looks are distinct entries in the popup, the
  document render, and `o`.
- **A warning cookie no longer costs the planning line.** org's agenda
  warning/delay (`<2026-01-01 Mon +1m -3d>`, first-only `--7d`) used to block
  the timestamp's closing bracket: the stamp failed, the planning entry
  backtracked, and the line — with the drawer and the id behind it — demoted to
  body (the Dutch-weekday loss class). A timestamp now takes one repeater and
  one warning cookie in either order, stores the warning (`tsWarning`), and
  re-renders it byte for byte; a lone `-3d` is read as org reads it — a
  warning, not a minus-signed repeater — with the render unchanged either way.
- **`#+SEQ_TODO:` and `#+TYP_TODO:` configure the cycle.** org's two older
  spellings fell through to a generic pragma, so `* NEXT Foo` under
  `#+SEQ_TODO: NEXT | DONE` kept NEXT as title text. All three spellings land
  in the same cycle now.
- **Tags accept `%`.** org's own `org-tag-re` carries it; `:50%:` used to take
  the whole tags run down into title text. The parser's set is now org's plus
  the hyphen the wild corpus writes.
- **The sheet cycles a bracketless priority cell like the table does.** `S-<up>`
  and `S-<down>` over the materialize sheet read the cell with a regexp of their
  own that accepted only `[#A]`, where the table's reading takes a bare letter
  too — so an entry whose priority cell had lost its brackets started every
  cycle from `none`. One reading now (`priorityIn`), and both keys call it.
- **A CRLF config file keeps its own line endings.** `POST /config` spliced its
  `#+TODO:` block with LF whatever the file used, so one settings write left a
  CRLF `system.org` speaking two conventions with the line the reader had just
  typed as the odd one out. The block and the opening a header-only file owes are
  both the file's own ending now, read by the same `eolOf` a capture already used.
- **A tagged capture refuses a line, or a template answer, that is not one
  headline.** The untagged path has always refused an empty or multi-line
  `text`; the tagged path took both straight to its template's `%?` and its
  `%^{PROMPT}` holes, so a newline landed a column-1 star org reads as a second
  entry — and a blob holds ONE entry, the headline org-glance keys it by. Both
  the line and every `fields` answer now take that wall, and a refusal is a 400
  naming the field with nothing written.
- **Writes into a tagged capture's blob now reach the table, and the first
  `.org-glance/config` in a tree reseeds it.** Both create their own directories,
  and fsnotify arms a newly created directory without traversing into it —
  measured: one new level under a watched directory fires an event, two do not,
  and pausing between them does not help. A blob at
  `data/<shard>/<rest>/data.org` therefore sat somewhere nothing was watching for
  as long as the daemon ran, so the captured row never appeared AND every later
  edit to it was lost too: setting a state wrote `* STARTED` to the file while
  the table went on saying `TODO`. The first config layer in a tree was invisible
  the same way. The daemon knows the path at write time, so every write now
  queues the one it just wrote — into the watch's own debounce map, drained by
  the same serial loop through the same step, so a nudge plus the real event
  still costs one parse and the watch is still the only thing that updates the
  store. A path the walk would decline is dropped at that door exactly as an
  event is, so nothing can arrive by nudging that could not arrive by saving.
  KNOWN GAP, stated rather than buried: this covers what the daemon itself
  writes. A blob created into a fresh shard by ANOTHER process — org-glance's own
  Emacs side — still waits for a restart.
- **A property key may hold a digit, an underscore or a non-Latin letter, and
  the drawer survives.** `propertyKeyP` is org's own rule now — any run without
  whitespace or a colon — where it had been the narrow keyword charset, so
  `:TELE2:` and `:ZhKH:` stopped the drawer dead and everything under them
  became body text. Reported against a real tree. Deliberately WIDER than
  `keywordTextP`, whose narrowness is what makes a starred meta undeclarable:
  the two walls guard different things, and a property key is the author's word
  where a TODO keyword is a value this producer has semantics for. The reserved
  `PROPERTIES`/`END` guard is untouched, being what terminates the drawer.
- **An archived row spends its MARK along with its flag.** A mark is the
  renderer's and survives a `setRows` and a filter that hides its row — which is
  what makes it useful, and what left an archived row marked where no reader
  could see it: `markedCount()` counted it, `U` and `M` answered about it, and it
  came back marked the moment anyone looked at `tag:*archive*`. Only the rows the
  answer says LANDED are spent, and none at all where the request itself failed;
  the spending is the ARCHIVE gesture's rather than `fire`'s, since a name test
  in the shared path would be one every command added after it has to be read
  against.
- **A freshly booted page has a row under the keys.** A mount has no cursor of
  its own — the renderer selects nothing until it is asked to, `selectFirstVisible`
  having one caller and it being the filter box handing over — so `d`, `D` and
  `RET` on a just-opened page all answered `no row` until the reader pressed
  `n`. The landing table already said an APPLIED VIEW lands on row one, and a
  boot IS a view applied: `start` now lands through the same `land`, so row one
  is spelled in exactly one place rather than growing a boot rule beside the
  three. It lands on the MOUNT, which is the `?limit=100` first paint, and the
  full set arriving behind it lands nothing more — `paint` keeps the cursor the
  way the renderer keeps every selection, so it is one landing per mount. A
  caller that PASSES an `after` lands inside it and this door stands aside,
  which is what leaves a pop's remembered row untouched; a `view-changed`
  remount passes none and takes row one like any other apply. An empty answer
  still selects nothing, and the keys say so (`d → archive-flag (no row)`,
  `no row focused — n or p picks one`).
  THE HARNESS LIE THAT HID IT: `shell-harness.js` answered `getSelection` with
  row 0 of the page whatever had happened, so ~170 cases pressing a row key as
  their first act were testing a selection the browser would not have had. The
  stub now models `state.selected === null` where the renderer does —
  `keepSelection` returns at the guard, `indexOfSelected` answers -1,
  `getSelection` answers a null id, and `selectStep` from nothing lands on the
  end it steps away from — and a `total` of 0 is an EMPTY STORE, the count the
  server reports being the count of the set it answers with, which is the one
  store state no act can reach. Flipping the stub alone fails those ~170 cases;
  the landing is what makes them honest rather than lucky. Five cases open
  `TestServe`'s "Shell landing".
- **A weekday in any language keeps its headline's property drawer.** The
  timestamp parser took exactly three letters in the weekday slot, so ~/sync's
  Dutch stamps — `CLOSED: [2025-12-04 do 22:34]`, with `ma`, `zo`, `vr` and `za`
  beside `do` — failed the timestamp, failed `planningP`, and left the drawer no
  longer the next thing after the title line, taking the headline's properties
  and its `ORG_GLANCE_ID` whole. The slot now takes a run of LETTERS in any
  script, of any length, and drops it the way it always did: the weekday is
  display-only, recomputed from the date on every render, so a locale's word
  carries nothing the parser needs. Letters alone is what holds a repeater out
  of the slot — `.+3d` opens with `.` — which leaves the trailing dot French and
  Catalan abbreviate with (`lun.`) still refused, a boundary the corpus does not
  reach. This is the gap the drift instrument reported and did not fix: over
  ~/sync/views the blobs carrying no id go **49 → 21** and the records without
  blobs **57 → 29**, both by exactly the 28 it named. The corpus oracle is
  unmoved — 12596 headlines, 0 span violations over ~/sync.

## 0.4.0.0 - 2026-08-02

The interaction surface: one keymap, a palette in front of every write, marks
and flags, a navigable filter stack, and a binary that carries its renderer.

### Added
- One keymap for the whole page, carried to the shell as data and read by the
  echo widget, so a binding is spelled once and echoes its elisp command name.
- Row marking — `m`/`u`/`U`/`M` over the renderer's id-keyed set, so a mark
  survives a filter, a page and a refetch.
- Archive flags — `d` flags the row at point and a second `d` archives every
  flagged row; `D` is that second press without the flagging one.
- `t`/`C-c C-t` raise a which-key state palette drawn from `GET /keywords`: the
  resolver's own chain as a Source | Active | Inactive table, one letter per
  keyword committing on its own, `*clear*` at the foot, `/` for a completing
  read.
- `+` captures an entry and `C-c C-s`/`C-c C-d` reschedule, each through the
  palette's text mode; an empty line clears the planning entry.
- `a` applies the agenda view — active rows carrying a date, sorted by schedule.
- `o`/`!` open a row's links, off `GET /links` and the same display rule the
  table renders by; several links raise the palette.
- A navigable filter stack: `@` drills into a row's references (`ref:ROWID`),
  `DEL` strips a token and pops the trail where that empties the query, and the
  trail crosses a remount through `?crumbs=`.
- `planned`, a filter key over the date columns, in parity with the renderer.
- An append-only log strip naming every row a command landed on, and a status
  corner carrying the connection dot and an auto/light/dark theme selector.
- `glance desktop` opens its own WebKitGTK window under the manual
  `native-window` flag (`make native`, plus `vendored/` bindings repointed at
  WebKit 4.1). Closing the window stops the daemon; `--keep-serving` restores
  the borrowed-browser behaviour.

### Changed
- The binary carries the renderer: `assets/table-view.js` is compiled in, so a
  copied binary reads no path off this checkout. `--assets DIR` replaces the
  whole asset set and stays a development flag.
- Keyword classification is one four-rank chain, widest first — the built-in
  `TODO | DONE`, then system, then the row's tags, then the file — read forwards
  by the resolver and by the palette, so what a palette offers is what a write
  accepts.
- `state:*active*` covers the stateless row, so an entry nobody has stated shows
  in the default view.
- The two movement profiles collapsed into one map carrying both spellings:
  `n`/`p` and `j`/`k` step a row, `f`/`b` and `l`/`h` step a cell.
- A row is a top entry with something to show — a level-one headline carrying at
  least one column — numbered `FILE#K` after both filters, so an edit above a row
  keeps its id.
- The table drops the renderer's per-row action hint; the resident key line
  already says it, for every command.

### Fixed
- A headline's star run must end at horizontal space or the line's end, so a
  body line opening `*bold*` stays emphasis (251 corpus lines were rows before).
- A held `t` no longer commits through the palette that press opened.
- `hrDeclared` is forced when stored, so a file's keyword set stopped pinning
  that file's whole element tree.

## 0.3.0.0 - 2026-08-01

The write path: org files edited from the browser, byte for byte.

### Added
- `Data.Org.Edit`, the span-edit engine — char-span splice, optimistic digest
  lock, atomic same-dir temp+rename; untouched bytes stay byte-identical.
- Materialize: `GET`/`POST /headline?id=` serves and replaces a headline's whole
  subtree under a pinned digest; drift is a 409 with the file untouched.
- The subtree lens — body, properties, planning and logbook as four regions with
  one owner per byte. Decompose and recompose is byte-identical; the row id and
  the creation stamp are server-preserved and never travel to the client.
- A buttonless materialize sheet that syncs itself: two panes over one subtree,
  a modal properties panel with keys of its own, a read-only logbook strip, and
  `C-c '` to swap between two-pane and raw org.
- `POST /command` — `set-state`, `set-planning`, `archive` and `capture`. Ids
  group by file and each file is one drift-locked write, answered per id.
- `capture` appends an entry to `#+GLANCE_CAPTURE_TARGET:` (default
  `<root>/inbox.org`, created on demand) under an `:ORG_GLANCE_CREATION_TIME:`
  stamp; a target the walk would decline is refused where the config is read.
- Layered keyword config under `<root>/.org-glance/config/`: system, per-tag and
  per-file `#+TODO:` lines, recognized as a union and classified nearest-scope.
  A config change reseeds and reloads the world.
- `GET`/`POST /config` and a settings sheet (`,`) editing one layer's `#+TODO:`
  block through the ordinary drift-locked write path, so a `#+TITLE:`, a comment
  and a capture template come back byte for byte.
- Tree-wide `#+GLANCE_DEFAULT_FILTER:` and `#+GLANCE_CAPTURE_TARGET:` lines in
  `system.org`, edited from that same sheet and spliced in one write.
- Archived rows are hidden from `/headlines` unless the query names the
  `archive` key; `X-Glance-Archived` counts what was taken.

### Changed
- Streamed frames are id-resolved like every other answer, so editing the loser
  of a shared id streams nothing and a winner going away re-points the id.
- The write routes never touch the store — the file watch stays the sole updater,
  so a browser write and an Emacs write arrive by the same door.

## 0.2.0.0 - 2026-07-31

`glance serve`: org headlines in a browser tab, live.

### Added
- `glance serve --dir DIR` — a local daemon on 127.0.0.1 serving one row per top
  entry: state badge, priority, title, tags, scheduled and deadline.
- `Glance.Query`, the public facade producing the table-view View object; cells
  are sliced from spans and no internal type reaches the wire.
- Live rows: a per-path debounced file watch re-parses one file per event and
  streams row ops over `/ws`, with the bootstrap snapshot taken inside the
  subscribing transaction.
- Server-side filter, paging and ordering — `?q=`, `?limit=`, `?offset=`,
  `?order=` — the query grammar a term-for-term port of the renderer's, so both
  halves answer a query alike.
- `ETag`/304 over a tree fingerprint and a generation, plus gzip: the full view
  went 3.06 MB to 580 KB, and a revalidation costs 0.56 ms against 102 ms.
- The shell — a keyboard-driven page with a resident key line, an echo widget,
  pager and cell-movement keys, and the applied query in the URL.
- `glance desktop` — the same daemon with an app-mode browser window opened as
  soon as the socket listens; `--browser`, `$GLANCE_BROWSER` and `--dry-run`.
- Bind-before-load: the server binds first and answers 503 + `Retry-After: 1`
  while it indexes, so the shell renders an indexing state and polls out of it.
- org-glance's derived mirrors stay out of the walk and the watch;
  `--include-derived` turns that off.
- Per-file reads run on a pool of `getNumCapabilities`, results reassembled by
  input index.
- Shared `ORG_GLANCE_ID`s resolve to one row — a `.org-glance/data/` path wins,
  else walk order — and the losers are counted in `X-Glance-Id-Collisions`.

### Fixed
- Emacs sidecars (the `.#name.org` lock symlink and `#name.org#` autosaves) are
  out of the walk and the watch, so a dangling lock costs no read failure.
- A dropped socket revalidates `/headlines` and re-attaches instead of
  remounting, so a filter, an open sheet and the selection survive a reconnect.
- Residency is bounded: cells are copied out of the document they were sliced
  from, so a loaded store stopped retaining every file it parsed.

## 0.1.0.0 - 2026-07-31

The parser base.

### Added
- An org parser over headlines (stars, TODO keyword, priority, title, tags),
  property drawers, logbook and generic drawers, code blocks, pragmas,
  timestamps, links and tokens.
- A REPL over parsed org with readline history; `glance FILE` seeds its context
  from that file.
- Half-open char spans retained through the parse, tight per headline component,
  which makes the source text losslessly addressable.
- `glance scan DIR` — a corpus oracle reporting files, headlines and ids, and
  validating every span it retained.
- Planning lines: `SCHEDULED:`/`DEADLINE:`/`CLOSED:` in any order on the line
  under the title, last wins per keyword.
- Timestamp ranges, spelled `<a>--<b>` or compactly as `<date wd 10:30-11:30>`,
  preserved as written.
- Context keyword sets, so a `#+TODO:` affects the headlines below it.

### Fixed
- Headlines parse at column 1 alone, so a mid-line `*bold*` stopped being a row.
- TODO keywords are matched case-sensitively and stored verbatim.
- Trailing horizontal space silently destroyed a headline or an indented drawer.
- A date-only timestamp rendered a phantom `00:00`.
