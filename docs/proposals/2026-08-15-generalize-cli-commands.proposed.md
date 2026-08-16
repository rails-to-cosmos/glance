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
(`2026-08-15-a-registry-nothing-walks.proposed.md` is the general case; this is
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
