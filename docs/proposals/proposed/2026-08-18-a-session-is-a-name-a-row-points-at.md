# Proposal — a session is a name, and a row points at it

**Status:** proposed · **Date:** 2026-08-18 · **Origin:** user — *"isolated
sessions, like incognito but with configurable cookies/history, per headline or
per tag; a headline could share a session with another headline; and I want the
history for that session searchable inside the headline's material doc."*
Sharpened mid-session: *"maybe even multiple sessions per headline — a pet-shop
row buying to two different addresses."*

That last sentence decides the model. If one row can hold two sessions, a
session is not a property OF a row. **A session is a named profile; a row points
at one.** Sharing is two rows pointing at the same name. Isolation is two rows
pointing at different ones. Both fall out of naming, and neither needs its own
mechanism.

## Where the code stands today

`glance desktop` opens `WebKitWebView`s with no context of their own, so every
view in the process shares WebKitGTK's default `WebKitWebContext` — one cookie
jar, one localStorage, one cache, for the app page and every link the reader
follows.

| what | where | today |
|---|---|---|
| the app view | `src-desktop-native/Glance/Desktop/WebKit.hs:41` | `WK.webViewNew` — default context |
| a followed link's popup | `WebKit.hs:118` | `WK.webViewNew` — the same default context |
| what the page tells the window | `WebKit.hs:79-85` | one string: the URI, and nothing else |
| which links are followable | `src-query/Glance/Query.hs:516` | `http`, `https` — and `WebKit.hs:102` admits it copies the rule because *"this layer cannot see it"* |
| where a followed link comes from | `frontend/glue/30-capture.js:386` | `window.open(link.target, "_blank")`, overridden at `WebKit.hs:74` |
| stored browsing history | — | **none.** WebKitGTK keeps no history database; `WebKitBackForwardList` is per-view and dies with the view |

So both halves of the ask are unbuilt, and the second one cannot be borrowed
from the engine — nobody records visits but us.

## The model

Three types, and one resolution rule the repo already owns.

```haskell
-- | A profile's id: the directory under `.org-glance/sessions', and the name a
-- row points at.  Sharing is two rows naming the same one.
newtype SessionName = SessionName String

-- | What a scope may declare.  The default is 'Shared' — today's behaviour.
data SessionRef
  = Shared                  -- ^ the tree's one common profile
  | Own                     -- ^ a persistent profile keyed by ORG_GLANCE_ID
  | Incognito               -- ^ ephemeral: no bytes, discarded with the last view
  | Named SessionName       -- ^ the profile by name; the sharing spelling
  | Seeded SessionName SessionName  -- ^ `NAME from OTHER': cookies copied ONCE

-- | What the window is handed.  Fully resolved: the window makes no policy.
data Session = Session
  { seName    :: SessionName
  , seData    :: Maybe Path   -- ^ `Nothing' is ephemeral, and is the whole of it
  , seCache   :: Maybe Path
  , seKeeps   :: Bool         -- ^ does it record visits
  }
```

**Resolution reuses `classify`'s ladder rather than inventing a second one.**
`AGENTS.hs:1288-1302` already ranks `Default < System < TagScope < FileScope`
and answers with *the first scope holding an opinion*; `keywordScopes` builds
the chain and `classify` folds it. `Scope` carries four constructors today
(`AGENTS.hs:1286`); sessions add a fifth, below `FileScope`:

```haskell
data Scope = Default | System | TagScope Tag | FileScope | HeadlineScope
```

The full ladder, widest first:
`Default < System < TagScope < FileScope < HeadlineScope`.

`sessionScopes` mirrors `keywordScopes` line for line, and `sessionOf` mirrors
`classify` — first opinion wins, no opinion falls through to `Shared`. A reader
asking *why this session* gets the same answer shape `GET /keywords` already
gives for states: **which scope decided, by name** (`keywordSources`,
`AGENTS.hs:1316`).

## How it is spelled

| scope | file | line |
|---|---|---|
| system | `.org-glance/config/system.org` | `#+GLANCE_SESSION: shared` |
| tag | `.org-glance/config/tags/book.org` | `#+GLANCE_SESSION: book` |
| file | any org file | `#+GLANCE_SESSION: research` |
| headline | the properties drawer | `:GLANCE_SESSION: petshop-home petshop-office` |

The tag layer is the user's *"use cookies from tag `:book:`"*, and it needs no
new config machinery: `Layer` (`AGENTS.hs:1338`) is already one file per tag,
already digest-locked, already merged widest-first.

**The headline property is a LIST, and that is the pet-shop case.** Two names
means the row has two profiles; the first is what `RET` opens. A second key —
`S-RET` on a link, or `s` inside the links popup (`frontend/glue/40-popups.js:25`) —
raises the session picker, which is the popup machinery that already exists and
already filters. Keyboard-first, no button.

`Own` on a row with no `ORG_GLANCE_ID` cannot key a directory, and 42% of rows
carry no id (`docs/proposals/done/2026-08-15-a-relation-is-a-link-with-a-kind.md`).
Two honest answers: refuse with the message the link picker already gives, or
mint an id the way `capture` does. **Recommended: mint** — the reader asked for
a private profile, and the id is the mechanism, so minting it needs no question.

## Two readings of "use cookies from tag :book:", and both ship

- **Alias** (`Named "book"`) — the row uses the `book` profile itself. Logins
  made under the row land in `book` and every other `:book:` row sees them. One
  profile, many referrers, live.
- **Seed** (`Seeded "petshop-home" "book"`) — the row gets its OWN profile,
  its cookie jar filled once from `book` at creation, diverging after.
  `cookieManagerGetAllCookies` out of one, `cookieManagerReplaceCookies` into
  the other, and the fact that it ran is recorded in the new profile's
  metadata so it never runs twice.

Alias is the default reading and the cheaper one. Seed is the one worth having
when a shop login should not be poisoned by what the row does next.

## What WebKit2 4.0 actually gives us

Every call below was checked against the vendored bindings; nothing here needs a
newer WebKitGTK than `make native` already builds against.

| need | call | in the vendored bindings |
|---|---|---|
| a profile's storage root | `new WebsiteDataManager [#baseDataDirectory := d, #baseCacheDirectory := c]` | `WebsiteDataManager.hs:706,749` — construct-only properties; the C constructor is varargs and un-introspectable, so `Data.GI.Base.new` is the door |
| incognito | `websiteDataManagerNewEphemeral` / `webContextNewEphemeral` | `WebsiteDataManager.hs:17`, `WebContext.hs:1619` |
| the session object | `webContextNewWithWebsiteDataManager` | `WebContext.hs` |
| a view in that session | `webViewNewWithContext` | `WebView.hs` |
| `window.open` keeping the opener's session | `webViewNewWithRelatedView` | `WebView.hs` |
| the cookie jar | `webContextGetCookieManager`, `cookieManagerSetPersistentStorage` (sqlite or text) | `CookieManager.hs` |
| third-party policy per session | `cookieManagerSetAcceptPolicy` | `CookieManager.hs` |
| seeding, exporting | `cookieManagerGetAllCookies`, `addCookie`, `replaceCookies` | `CookieManager.hs` |
| forgetting a session | `websiteDataManagerClear` / `Remove` / `Fetch` — per data type | `WebsiteDataManager.hs` |
| the visit feed | `onWebViewLoadChanged` + `webViewGetUri` + `webViewGetTitle` | `WebView.hs` |

**The unit of isolation is the `WebKitWebContext`, and everything hangs off its
data manager** — cookies, localStorage, IndexedDB, service workers, cache. There
is no finer knob and no coarser one.

Costs, stated rather than discovered later:

- **One network process per context**, plus at least one web process. Ten live
  sessions is ten extra processes and a few hundred MB. Contexts must be created
  **lazily on first open** and dropped when their last view closes.
  A cap, with the oldest idle context evicted, belongs in layer 1.
- **The data directories are construct-only.** A live context cannot be
  re-pointed; changing a row's session takes effect on the next open.
- **WebKit 6.0 / GTK4 moves this to `WebKitNetworkSession`.** The model
  survives the port unchanged; two constructor calls move.

## The plumbing: policy stays where the config lives

Today the page posts a bare URI string to the `popup` handler
(`WebKit.hs:79-85`) and `WebKit.hs:102` duplicates the followable rule because
it cannot see the page's. Do not repeat that mistake at ten times the size — the
scope ladder, the layers and the tag chain must not be re-implemented inside
GTK.

**The page resolves; the window obeys.** The message becomes one JSON object:

```js
window.webkit.messageHandlers.popup.postMessage(JSON.stringify(
  { uri, session: { name, data, cache, keeps } }))
```

`WebKit.hs` holds `IORef (Map SessionName WebContext)`, creates on miss, reuses
on hit, and knows nothing about tags, layers or properties. **Two rows naming
one session get the same context object** — identity, not equality — and that is
a testable law.

The window and the daemon are **one process**: `runNative` (`Glance/Desktop/Native.hs`)
forks warp onto a thread and keeps GTK on the main one. A recorded visit
therefore needs no HTTP round trip and no auth story — it is an in-process
append that the daemon already serves from.

Under `--browser` (a real Chrome, `Glance.Desktop`), sessions cannot be
enforced: the profile belongs to that browser. Say so out loud — resolve the
session, **report** it in the answer and the dry run, and open links as today.
Scope honesty beats a half-kept promise.

## The history half

WebKitGTK stores nothing, so the shape is ours to pick. Three candidates:

**(a) Into the org subtree, a drawer per visit.** Searchable by everything
glance has today and travels with the file — and a browsing session writes two
hundred URLs into the reader's own document, each write a digest round against
`AGENTS.hs:2080`'s 409 protocol. **A browser must not rewrite the notes.**
Rejected.

**(b) A side store, in org.** `.org-glance/sessions/<name>/history.org`, one
entry per visit under a day headline. Cheap append, no conflict with the
reader's file, and still org — the parser reads it and the query engine can
index it. But it is not "inside the material doc", which is where the reader
asked to look.

**(c) (b) stored, (a) addressed.** The visits live beside the tree; the
material sheet SHOWS them. This is the answer.

`GET /headline` already carries `links`, `logbook`, `properties` and `org`
(`src-web/Glance/Web/Routes.hs:392-409`); it grows `session` (the resolved name
and the scope that decided it) and `visits` (that session's history, newest
first, paged). The sheet grows one pane behind one key — `h`, sitting beside
`l` for links — and the pane is the existing popup surface, which means it
inherits the tier, the veil and the filter for free once
`../partial/2026-08-18-generalize-popup-surface-registry.md` lands. The query
language grows `session:` and `visited:` so a search from outside the sheet
finds the row through what was read under it.

**And one key promotes a visit into the document.** Automatic history stays out
of the file forever; a visit the reader deliberately keeps is spliced in as a
plain org link, which is exactly `edit-link`'s existing write path
(`src-web/Glance/Web/Commands.hs:124`). That line — *recorded beside, kept
inside, only by hand* — is the spine of the design.

Details worth pinning now:

- Record at `LoadEventFinished` so redirects have settled to one URI; amend the
  title on `notify::title`, which arrives later.
- Roll up by host and day in the pane by default — two hundred rows is not a
  reading. Search unrolls, the way material grain navigation already does.
- Retention per session (`#+GLANCE_SESSION_KEEP: 500` or `30d`), truncated when
  the daemon starts.
- `Incognito` keeps nothing by default. A reader who wants the trail without
  the cookies writes `incognito keep`, and that pair is legal.

## Security, stated plainly

A session profile holds live login cookies. It is a credential store sitting
next to the reader's notes.

- `.org-glance/sessions/` must be created `0700`, must be excluded from the
  walk, and must be added to `.gitignore` by the same code that creates it —
  the org tree is frequently a git repository.
- Cookies are never written into an org file, not even by `seed`. The org side
  holds names; the profile holds secrets.
- `glance sessions forget NAME` must actually clear it —
  `websiteDataManagerClear` over every data type, then remove the directory.

## The laws worth testing

Split `WebKit.hs` into a pure half (`SessionRef` + scopes → `Session` + paths)
and a GTK half. The pure half is what the suite drives, and
`test/TestDesktop.hs` already fakes the window as a plain `String -> String -> IO ()`
(`Native.hs:desktopWith`) — the fake becomes a **recording** window, and the
whole routing story is testable with no display and no pixel.

1. Resolution is first-opinion-wins over the ladder `classify` uses; a session
   no scope names is `Shared`.
2. Two rows naming one session are handed **one** context — assert the count of
   contexts created, not their contents.
3. An ephemeral session leaves no byte under `.org-glance/sessions`.
4. A visit is recorded exactly once per finished load; a redirect chain records
   its settled URI and nothing else.
5. **The reader's org file is byte-identical after a browsing session.** The
   strongest test in the set, and the one that keeps (a) rejected.
6. `--browser` reports the resolved session and isolates nothing, and says so in
   `--dry-run`.

## Shipping order

| layer | what | why it stands alone |
|---|---|---|
| **0** | resolve and REPORT: `GET /headline` answers `session` + deciding scope; the sheet shows it | no GTK, no storage, fully covered by the existing suite |
| **1** | one context per session in the window: `shared`, `NAME`, `incognito`, `own` | the pet-shop case works end to end |
| **2** | visits recorded, `history.org` per session, the `h` pane, `session:` / `visited:` in search | the ask's second half |
| **3** | `seed:`, `glance sessions list/forget/export`, retention, idle eviction | housekeeping, once there is something to keep |

Layer 0 is worth landing on its own even if nothing follows: it makes the
policy visible before anything acts on it, which is how the keyword chain was
built.

## Open questions

1. **Context cap and eviction.** A number, or pressure-driven? Ten live
   sessions is real memory.
2. **`own` on an id-less row** — mint, or refuse? Recommended: mint.
3. **Does the app view itself take a session?** It serves `127.0.0.1` and holds
   no cookies worth keeping; leaving it on the default context is simplest, and
   means the app page can never be reached by a site's storage.
4. **`session:` and `visited:` as query columns** touch the column registry —
   check the grain against `2026-08-15-one-row-per-column.md` before
   spelling them.
5. **Downloads** are per-context too (`webContextDownloadUri`). Do they follow
   the session, and where do the files land?
