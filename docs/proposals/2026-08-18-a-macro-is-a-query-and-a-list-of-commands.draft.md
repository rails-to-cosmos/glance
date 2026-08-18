# Proposal — a macro is a query and a list of commands

**Status:** draft — the model is settled enough to argue with; the spelling, the
undo story and the page-automation leash are open · **Date:** 2026-08-18 ·
**Origin:** user — *"one of the best things we can make on top of our
emacs/webkit stack is an emacs-like macro system."*

Agreed, with one condition that decides everything below: **record commands,
never keystrokes.**

## Why emacs's own design does not port

Emacs records keys because in emacs a key IS a command and the buffer is
synchronous — the tape replays into the same point model that produced it.

Glance is a client-server app. Writes are `POST /command`
(`src-web/Glance/Web/Routes.hs:134`), each carrying ids and digests, each able
to come back 409 against a file that moved under it (`AGENTS.hs:2080`). Row
identity belongs to the renderer, and the row under point after a filter, a
sort, or a page turn is not the row that was there when the key was pressed.

A keystroke tape replayed against a drifted page does the wrong thing, to the
wrong row, silently, at speed. That is the whole argument.

## The model

**A macro is a row set and an ordered list of commands.** Every part already
exists and none of it needs inventing:

| part | where it lives today |
|---|---|
| a closed set of twelve verbs, each with args, ids and digests | `src-web/Glance/Web/Commands.hs:114-145` |
| a row SET as a query, and named ones | `AGENTS.hs` `savedViews`, `filter-rows` |
| marks and flags, keyed by id, surviving `setRows`, a filter and a page turn | `AGENTS.hs:3501` |

So the recorder records what the shell already posts. It adds no
representation, no second vocabulary, and no second write path.

```haskell
-- | What was recorded: how the rows were chosen, and what was done to them.
data Macro = Macro
  { mcName  :: !Text
  , mcRows  :: RowChoice          -- ^ how the run finds its rows
  , mcSteps :: [Step]
  , mcCount :: Maybe Int          -- ^ the counter's seed, when a step spends one
  }

-- | Marks are ids; a filter is a query.  A recording keeps WHICH, because
-- replay over new rows is the point.
data RowChoice = Marked [Id] | Filtered Query | AtPoint

-- | One recorded command, its args holding `${n}' where the counter was spent.
data Step = Step { stName :: !Text, stArgs :: Value }
```

## Keys author, commands are saved

Press record, work by hand, stop. Navigation keys record nothing — they move
point and nothing else. What lands is the trace of commands the shell actually
posted, with the row choice captured as `Marked` when marks drove it and
`Filtered` when the filter did.

That distinction is the design. The reader's hands stay in the emacs idiom; the
artifact is data.

## What this buys that emacs kmacro cannot

- **Replay over a different row set.** `apply-macro-to-region-lines` (`C-x C-k r`)
  is kmacro's celebrated idiom and its most awkward one. Here the row set is a
  query held IN the macro, so re-aiming is a re-run against a new filter. The
  default case, with no special form.
- **A macro is org text.** One headline: the name in the drawer, the steps in
  the body. Editing a macro is editing your own notes — diffable, in git,
  shareable, and layerable per tag under `.org-glance/config/` like every other
  setting the tree carries. `edit-kbd-macro` offers a buffer of `C-x` gibberish
  by comparison.
- **A dry run.** Steps carry ids and digests, so a run resolves before it
  writes: *twelve rows, three will 409, one row lost its id.* No emacs macro can
  be previewed.
- **Tests with no keys and no pixels.** `/command` is already driven in process
  by the suite (`Network.Wai.Test`, `test/TestServe.hs`). A macro is a list of
  those posts, so the whole feature is testable at the layer the suite is
  strongest at.

## The spelling problem, which is real and already documented

Emacs's `C-x (` / `C-x )` / `C-x e` cannot ship as written:

- **`C-x` is cut while a selection lives.** `frontend/glue/70-shell.js:88` —
  *"a live selection makes C-c and C-x copy and cut"* — and the dispatch
  declines to claim a prefix there. This is the exact wall that killed `C-c @`
  in `2026-08-15-a-relation-is-a-link-with-a-kind.done.md`.
- **`C-u` is reserved.** `AGENTS.hs:3239` leaves it to the browser for
  view-source, and the same relations proposal ruled it out as a prefix.
  kmacro's repeat count therefore has no prefix to ride on.

So macros take one key on the table and a popup owns the rest, the way `l` owns
links (`frontend/glue/40-popups.js:25`). Checked against `bindings`
(`AGENTS.hs:3136`): `K`, `e`, `s`, `w`, `y`, `c`, `v`, `z` are unbound on the
table; `n` and `p` are taken (`:3137-3138`), as are `j`, `k`, `m`, `u`, `U`,
`M`, `d`, `D`, `x`, `t`, `g`, `P`, `o`, `!`, `A`, `@`, `+`, `q`.

Proposed: **`K`** raises the macro popup — `k` is previous-row, so `K` is its
shifted sibling, and kmacro is what it stands for. Inside: `r` record and stop,
`e` execute, `n` name, `s` save into org, `d` drop. Repeat count is typed into
the popup, since no prefix key survives the browser.

If org-glance carries a macro map of its own, mirror that instead — the keymap's
rule is that glance's keys are org-glance's keys.

## The counter is what makes it earn its keep

kmacro's counter (`C-x C-k C-i`) is why macros number things. Here it is a
template variable in a step's args — `${n}` in a title or a capture's text,
incremented once per row. It is the difference between a macro that renames
forty rows and a macro that enumerates them, and it costs one substitution pass
over the args before the post.

## How a macro is written down

One headline, so the config layers and the query engine already read it:

```org
* Ship the reading queue                                     :macro:
  :PROPERTIES:
  :GLANCE_MACRO: ship-reading-queue
  :GLANCE_MACRO_ROWS: tag:*book* state:*active*
  :GLANCE_MACRO_COUNT: 1
  :END:
  1. set-state {"keyword": "NEXT"}
  2. add-tag {"tag": "queued"}
  3. set-planning {"keyword": "SCHEDULED", "date": "+${n}d"}
```

Open: whether steps are a numbered list read by line, or one source block.
The list is greppable and hand-editable; the block is unambiguous. The list is
probably right, and the parser owes it a strict reading with a refusal that
names the line.

## The webkit half, and its leash

With `2026-08-18-a-session-is-a-name-a-row-points-at.proposed.md` landed, a step
like *open this row's links in session `petshop-home`* is an ordinary glance
command with a session argument. Cheap, safe, and it ships in the same layer as
everything else here.

Going further — click this, type that, read the result back — means injecting
JS into a third-party page. The machinery is present:
`src-desktop-native/Glance/Desktop/WebKit.hs:66-70` already installs a user
script at document start, and `webViewRunJavascript` would run the steps.

**Stated plainly, because this is the part that can hurt someone:** a recorder
pointed at a live page records what is typed into it, and what is typed into a
login page is a password. Page automation must therefore be opt-in per session,
off by default, never started implicitly by the row-level recorder, and must
exclude password fields from capture outright. It belongs in the last layer,
well behind the org-side macro, which carries none of this hazard.

## Undo, which is the hard part

Emacs macros are undoable because a buffer has one undo list. Glance writes
files. A macro touching forty rows needs one retreat, and the honest version is
an inverse command list built from the pre-image digests the run already
collected.

State the scope. **v1 refuses a run over more than N rows until
its dry run has been seen**, and every run writes a receipt — the steps, the
rows, the digests before and after — so a retreat is at worst hand-drivable.

## The laws worth testing

1. A recording captures commands and not motion: a session of pure navigation
   records an empty macro.
2. A macro recorded over marked rows replays over the rows marked NOW, and one
   recorded under a filter replays over what that filter matches now.
3. A dry run writes nothing — the tree is byte-identical afterwards — and its
   report names every row that would 409.
4. The counter is spent once per row, in row order, and a macro with no `${n}`
   never consumes one.
5. A macro read back from org is the macro that was written: round-trip the org
   spelling through the parser and the recorder's output.
6. A step naming an unknown command is refused when the macro is READ, not when
   it is halfway through running.

## Shipping order

| layer | what | why it stands alone |
|---|---|---|
| **0** | run a hand-written macro from org over a query; dry run only | no recorder, no keys — pure server, fully covered by the suite |
| **1** | the same, writing; receipts and the N-row wall | the useful half |
| **2** | the recorder: `K`, the popup, save into org | the authoring half, once the artifact is proven |
| **3** | the counter and templated args | numbering |
| **4** | session-aware steps (open links in a named session) | rides the session proposal |
| **5** | page automation, opt-in, leashed | last, and only if wanted |

Layer 0 is worth landing alone: a macro that can only be written by hand and can
only be previewed is already a way to apply a checklist to a query, and it puts
the representation under test before any key is bound.

## Why it is cheap

Almost all of it is composition of things already built — the command registry
(`2026-08-10-generalize-command-kinds.done.md`), the popup surfaces
(`2026-08-18-generalize-popup-surface-registry.partial.md`), the keymap dialects
(`2026-08-15-generalize-keymap-dialects.proposed.md`), the query language, and
the digest protocol. The new code is a reader, a runner, and a recorder.

## Open questions

1. **Steps as a numbered list, or one source block?** Greppable against
   unambiguous.
2. **What does a macro do with a row that 409s mid-run** — stop, skip and
   report, or retry once against the fresh digest?
3. **Does a macro nest?** A step naming another macro is obvious and is also
   how a macro system grows a loop it cannot terminate.
4. **Where do macros live** — `.org-glance/config/macros/`, or anywhere in the
   tree found by the `:macro:` tag? The tag reading is more org, and means a
   macro travels with the notes that motivated it.
5. **`AtPoint` recordings** — is a macro recorded on one row with no marks worth
   saving at all, or is the row choice always a query?
6. **The repeat count** has no prefix key left. Typed into the popup is the
   fallback; whether that is enough for the idiom is a use question.
