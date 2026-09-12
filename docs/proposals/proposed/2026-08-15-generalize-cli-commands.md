# Proposal — a CLI command is spelled five times, and nothing joins them

**Status:** proposed · **Date:** 2026-08-15 · **Origin:** `/generalizer`, the
future-variant angle.

## The family, and the marginal cost

`glance` has four subcommands. Each is spelled in five places, none of them
joined and none enforced:

| # | where                       | the spelling                                                                                             |
|---|-----------------------------|----------------------------------------------------------------------------------------------------------|
| 1 | `app/Main.hs:70,86,92,94`   | the dispatch equation, `parse ("scan":args) = …`                                                         |
| 2 | `app/Main.hs:63-66`         | the help ladder, `("scan":_) -> scanUsage`                                                               |
| 3 | `app/Main.hs` `glanceUsage` | the summary line, and the usage block included under it                                                  |
| 4 | `AGENTS.hs:1071`            | `data Cli = CliScan \| CliServe \| CliDesktop \| CliRepl`                                                |
| 5 | `test/TestCli.hs:40`        | `commands = [("serve","--dir"), ("desktop","--browser"), ("scan","--include-derived"), ("repl","FILE")]` |

**Nothing goes red when a spelling is missed.** `parse (arg:_)` catches an
unregistered name and prints "unknown command", so a subcommand added to the
help ladder and forgotten in the dispatch is a working help entry for a command
that does not run. The reverse — dispatched but absent from `glanceUsage` — is
an undocumented command that works. `AGENTS.hs`'s `Cli` is compared to nothing
(`2026-08-15-a-registry-nothing-walks.md` is the general case; this is
one of its members).

## Proposed change

One row per command, in `app/Main.hs`, carrying what the five sites each hold:

```haskell
data Command = Command
  { cmdName  :: !String              -- ^ what the user types.
  , cmdBlurb :: !String              -- ^ the one line `glanceUsage' prints.
  , cmdUsage :: !String              -- ^ the full usage block.
  , cmdRun   :: ![String] -> IO a    -- ^ the dispatch, args past the name.
  }

commands :: [Command]
commands =
  [ Command "serve"   "serve an org tree over HTTP"        serveUsage   (run "serve" serveUsage serve . serveOptions)
  , Command "desktop" "the same daemon in an app window"   desktopUsage (run "desktop" desktopUsage runDesktop . desktopOptions)
  , Command "scan"    "parse a corpus and report what drifted" scanUsage runScanArgs
  , Command "repl"    "the org parser at a prompt"         replUsage    runReplArgs
  ]
```

`parse` becomes a lookup; `glanceUsage` becomes a fold over `cmdName`/`cmdBlurb`
with the blocks under it; the help ladder becomes `cmdUsage <$> lookup`. The
`repl` case keeps its two equations behind `runReplArgs`, the bare-vs-filename
split being that command's own business rather than the table's.

`test/TestCli.hs:40`'s hand-typed list becomes a read of `commands`, so a fifth
subcommand is tested the moment it is registered rather than when someone
remembers the test. `AGENTS.hs`'s `Cli` gains a `TestSpec` case comparing its
constructors to `map cmdName commands`.

## LOC

Added ~14 (the record and its rows). Removed ~20 now (the help ladder, the
usage intercalate, the test's literal list). **Saved per future subcommand:
four registration sites become one row, and the two that were silent become a
compile error and a red test.**

## Risk

`app/Main.hs` only, plus one test read and one spec case. `parse`'s type is
`[String] -> IO a`, so the table's `cmdRun` shares it and nothing about the
exit-code behaviour moves. No wire fields, no org bytes, no on-disk layout.
Usage TEXT must come out byte-identical — `TestCli.hs:26` asserts
`"usage: glance"` is printed and `:36` asserts the per-command line, so a
reordering of `glanceUsage` is caught.

## Existing precedent

`src-web/Glance/Web/Commands.hs:98` — `CommandSpec` with `csKind`, its registry,
and `namesRows` dispatching on it, which is the same shape one layer in.
`src-web/Glance/Web/Keymap.hs:14` — `KeyBinding` and `keyBindings`, a registry
whose JSON the page parses so "no key is bound undocumented".

## What would say this was wrong

`cmdRun`'s four inhabitants turn out not to share a type without contortion —
`repl`'s two equations and `scan`'s flag parsing resist the uniform
`[String] -> IO a`. Write the table for `serve` and `desktop` first; if `scan`
and `repl` need a wrapper each, the table has bought one site instead of four
and is still worth it, but say so rather than forcing them.

## 2026-09-12 update — the roster landed, the WORD did not

The `Command` roster this file asked for is in the tree: `app/Main.hs:145`
`data Command = Command { cWord, cSummary, cUsage }` and `commands`
(`:147-155`), with `glanceUsage` (`:158-165`) folding it for the index and the
help lookup folding it at `:69-70`. Six subcommands, one table. Sites 2 and 3 of
the five this file counted are closed.

**The six WORDS are still spelled six times, and only one of them is that
table.**

1. **The parse equations**, `app/Main.hs:64-106`: `"repl"` `:72`, `:74`;
   `"doctor"` `:88`; `"backfill-created"` `:93`; `"serve"` `:98`; `"mcp"`
   `:100`; `"desktop"` `:102` — each a bare literal in a pattern, and three of
   them spelled twice on their own line (`parse ("doctor":args)` then
   `takeFlags "doctor" args`).
2. **`cWord`** (`:145`), the table's own spelling.
3. **`Flag.fCommands :: [String]`** (`:226`) — every flag names the subcommands
   that take it as strings: `daemons = ["serve", "mcp", "desktop"]` (`:248`),
   `(daemons <> ["doctor"])` (`:238`), `["desktop", "backfill-created"]`
   (`:242`), `["desktop"]` (`:240`, `:244`), and a bare `"serve"` at `:333`
   inside `serveOptions`.
4. **Five `commandFlags "…"` literals** in the usage bodies — `:177`, `:185`,
   `:195`, `:204`, `:213`. (`replUsage` takes no flags and calls it not at all.)
5. **`AGENTS.hs:1121`** — `data Cli = CliScan | CliServe | CliDesktop |
   CliBackfill | CliRepl`. Five constructors for six subcommands: **`mcp` is
   absent from the model**, and `CliScan` is the retired name of what the binary
   now calls `doctor`. Read by `permissiveArgs` (`:1123-1128`) and by nothing
   else — the registry-nothing-walks case
   (`2026-08-15-a-registry-nothing-walks.md`), caught here in the act.
6. **`test/TestCli.hs:50-52`** — `commands = [("serve","--dir"),
   ("mcp","--dir"), ("desktop","--browser"), ("doctor","--include-derived"),
   ("repl","FILE"), ("backfill-created","--dry-run")]`, six pairs typed by hand.
   This is the file's site 5, still open; the suite pins the usage screens
   against a list the binary does not own.

### What the string lists cost

`takeFlags` (`:259-260`) decides which argv words are FLAGS and which are TREES
by string-list membership:

```haskell
takeFlags name = partition (`elem` map fName (commandFlags name))
```

so a word no `Flag` claims for `name` falls to the other half, which `treesIn`
(`:140`) reads as a directory. **Drop `"backfill-created"` from `--dry-run`'s
`fCommands` (`:242`) and `glance backfill-created --dry-run` migrates a
directory named `--dry-run`** — no flag error, no usage line, a run over a tree
that is not there. Same for `--include-derived` and `doctor` (`:238`). Nothing
is red: the list is data, and nothing joins it to the roster.

### The five usage skeletons are one

`serveUsage` `:174-180`, `mcpUsage` `:182-190`, `desktopUsage` `:192-198`,
`doctorUsage` `:200-207`, `backfillUsage` `:209-217` are each
`intercalate "\n" $ [the usage line, optional described rows] <> flagLines
(commandFlags "NAME") <> ["", prose…]`. Only the first line, the `described`
rows and the prose differ.

### Proposed

```haskell
-- | THE SUBCOMMAND WORD, once.  Every membership below is a `Sub', so a word the
-- parser takes and no table names is a compile error rather than a directory.
data Sub = Serve | Mcp | Desktop | Doctor | Backfill | Repl
  deriving (Eq, Show, Enum, Bounded)

subWord :: Sub -> String
subWord Serve    = "serve"
subWord Mcp      = "mcp"
subWord Desktop  = "desktop"
subWord Doctor   = "doctor"
subWord Backfill = "backfill-created"
subWord Repl     = "repl"

subOf :: String -> Maybe Sub
subOf w = listToMaybe [ s | s <- [minBound .. maxBound], subWord s == w ]
```

- `Flag` takes `fCommands :: [Sub]`, `commandFlags :: Sub -> [Flag]`,
  `takeFlags :: Sub -> …`; `serveOptions`'s bare `"serve"` (`:333`) becomes
  `Serve`.
- `parse` dispatches once — `parse (w : rest) | Just s <- subOf w = …` — so six
  patterns become one lookup and the `unknown command` arm (`:104-107`) is what
  `subOf` returning `Nothing` means.
- `Command` gains the parts that differ, and the skeleton folds:

```haskell
data Command = Command
  { cSub     :: !Sub
  , cSummary :: !String
  , cLine    :: !String     -- ^ the @usage:@ line, e.g. @glance serve --dir DIR [options]@
  , cExtra   :: ![String]   -- ^ @described@ rows above the flags, e.g. @DIR...@
  , cProse   :: ![String]   -- ^ what runs after the flag block
  }

cUsage :: Command -> String
cUsage c = intercalate "\n" $
  ["usage: " <> cLine c] <> cExtra c
  <> flagLines (commandFlags (cSub c))
  <> ("" : cProse c)
```

Five `*Usage` bindings become five table rows; `cWord = subWord . cSub`.

- `AGENTS.hs:1121`'s `Cli` is renamed to the binary's own six and gains a
  `TestSpec` case diffing `map show`-of-word against `map cWord commands`, the
  way `TestSpec.hs:849-852` diffs `viewColumns`. That case is what would have
  caught the missing `mcp`.
- `TestCli.hs:50-52`'s pairs become `[(cWord c, …) | c <- commands]` with the
  flag read off `commandFlags`, so a seventh subcommand is tested when it is
  registered.

**LOC**: added ~30 (`Sub`, `subWord`, `subOf`, the widened `Command`), removed
~55 (five `*Usage` skeletons, six parse patterns, the string lists, the two
hand-typed rosters). **Per future subcommand: one constructor the compiler
demands at `subWord`, one table row — instead of a parse equation, a table row,
an `fCommands` membership, a `*Usage` binding, an `AGENTS` constructor and a
`TestCli` pair.**

**Risk**: `app/Main.hs`, plus one `AGENTS.hs` type and one `TestCli.hs` list.
`TestCli` drives the binary by argv and asserts on its output, so the usage text
is the baseline to keep byte-identical; `AGENTS.hs`'s `permissiveArgs`
(`:1123-1128`) is the only reader of `Cli` and moves with it.
