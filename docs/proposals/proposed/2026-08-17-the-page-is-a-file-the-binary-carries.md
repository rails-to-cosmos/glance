# Proposal — the page is a file the binary carries

**Status:** proposed · **Date:** 2026-08-17 · **Origin:** asked directly — is
HTML in Haskell source good practice, and does Haskell have a Jinja.

## The two questions, answered first

**Is HTML-in-Haskell good practice?** It is defensible in exactly the shape this
repo has it, and it pays four costs it need not pay.

Defensible because `Page.hs` is 174 lines of *static shell* that the glue fills;
because the only data that flows in goes through `escape`
(`src-web/Glance/Web/Page.hs:163`, `:171`); and because the repeated markup is
already behind combinators — `popupFrame`, `field`, `crow`, `clab`
(`Page.hs:135-157`). That is the good version of markup in a host language.

The costs: no HTML tooling touches it (no formatter, no linter, no editor mode,
no `tidy`), an unclosed tag compiles, and `\"` on every attribute makes diffs
hard to read.

`Style.hs` is the sharper case, and it is **not** the pure-CSS file it looks
like. It assembles the whole `<head>`: 252 of its 330 lines are literal CSS, and
eight sites interpolate — the escaped `<title>` (`:38`), the mono stack
(`:41`), `logLinesDefault` (`:64`), `themeCSS <> themeOverrides colours` (`:50`),
and the theme-boot `<script>` with its `themeIds` (`:316`, `:327`). So the
question there is where the seam goes, rather than whether the file moves whole.

**Does Haskell have a Jinja?** Yes — `ginger` (0.10.6.0) is a Jinja2 dialect.
It is also the wrong tool here, along with the rest of its class, for the reason
in the next section.

## What Haskell actually offers

| | kind | checked | ships |
|---|---|---|---|
| `blaze-html` (0.9), `lucid` (2.11) | combinator EDSL | compile time; escaping by construction | in the binary |
| `shakespeare` (2.2) — `hamlet`/`lucius`/`julius`/`cassius` | QuasiQuoted templates | compile time, typed interpolation | in the binary |
| `file-embed` (0.0.16) | not a template engine — a file, spliced in | the file is whatever it is | in the binary |
| `ginger` (0.10.6) | Jinja2 | runtime | template files beside the binary |
| `stache` (2.4) | Mustache | runtime | template files |
| `heist` (1.1) | XML splices | runtime | template files |

The line that matters runs across the middle: **compile time or runtime**. A
runtime engine turns a markup mistake into a 500 on a page a user is looking at,
and it means the binary is no longer the program — a `templates/` directory has
to arrive with it. For a single-binary CLI that serves one page, that is a
straight downgrade. `ginger` answers the question and loses the argument.

`shakespeare`'s `lucius` deserves a note: it is a **CSS superset**, so real
stylesheet text pastes in unchanged and gains `#{}` interpolation. If
per-theme CSS generation ever outgrows `Theme.hs:117-121`, that is the tool.

## The recommendation, and it needs no new dependency

`file-embed` is **already a dependency**, and `embedFile` is **already the
pattern** for every other asset: the renderer (`Routes.hs:96`), the eight glue
parts (`:101`), Elm (`:105`). The page and the stylesheet are the two assets
that never got the treatment every other one has.

### 1. `Style.hs`'s 252 literal lines → `assets/glance.css`, spliced in

The seam is between the **stylesheet** and the **`<head>` that carries it**.
The stylesheet moves; `Style.hs` keeps the head assembly and shrinks to what is
actually a program: the escaped `<title>`, the four interpolated token values,
`themeCSS <> themeOverrides colours` (config-driven, so a program by
definition), and the theme-boot script.

Four of the eight interpolations are single token values —
`--glance-mono:<monoStack>`, `--g-logn:<logLinesDefault>` — which the stylesheet
can declare as its own defaults with Haskell writing an override line after it,
or which `lucius` would take as `#{}` with no seam at all.

What it buys:

- CSS tooling on 76% of the file: a formatter, a linter, `@media`/`@container`
  support in the editor, stylelint if it is ever wanted;
- the `--g-*` token block that
  [the sizing proposal](2026-08-17-the-box-is-measured-rather-than-guessed.md)
  wants becomes an ordinary block at the top of a stylesheet;
- one fewer file where `\"` is load-bearing.

What stays in Haskell either way: `Theme.hs`'s per-theme `--g-state-KEYWORD`
blocks, generated from the user's config.

### 2. `Page.hs`'s shell → `assets/index.html`, spliced in

The static half — the surfaces, their boxes, the two document editors — becomes
a real HTML file. The computed half stays where it is: `popupFrame` and friends
build the five popup frames, and the three `<script src>` lines carry
`rendererAsset` / `elmAsset` / `glueAsset` from the Haskell constants that name
them. Those fill a handful of named placeholders in the file.

### 3. Do not adopt a runtime template engine

Stated so the question is closed rather than re-asked.

## The one fact that decides it

`TestServe.hs` pins the served page by **exact substring** — dozens of `glue` /
`Glue` cases asserting on literal markup and literal CSS declarations
(`:5296`, `:5682`, `:5687`, and the new `#dtin`/`#dtext` case). The file-embed
route leaves every one of them passing byte for byte, because the bytes are the
same bytes. `blaze-html` or `lucid` would rewrite all of them, since attribute
order and whitespace become the library's business.

That asymmetry is worth more than the escaping-by-construction that `lucid`
would buy — and escaping is already handled at the one place data enters.

## Risk

- **`--assets` is a runtime override directory** (`ServeOptions.soAssets`,
  `Base.hs:61`), and today it replaces `table-view.js`. Adding `index.html` and
  `glance.css` to it is either the natural extension or a way to serve a page
  the tests never saw. **Recommend: embed only, no `--assets` lookup for these
  two**, so the shell is always the shell the suite pinned.
- **Two more entries in `sdistExtras` / `extra-source-files`**, which
  `TestSelfContained` already checks — "assets holds what the spec says it
  embeds and no more" is a passing case, so the spec's list moves with it.
- **`AGENTS.hs` names the asset set**; step 1 and step 2 each move one line
  there, and the existing oracle turns red if they do not.

## Staging

1. `assets/glance.css` + splice + `AGENTS.hs`/cabal entries, with the head
   assembly staying in `Style.hs`. The served bytes are unchanged, so the
   substring cases stand.
2. The sizing proposal's token block, now that it lands in a stylesheet.
3. `assets/index.html`, same shape, placeholders for the computed parts.

Step 1 is worth doing on its own even if 3 never happens: it is 252 lines
leaving a language that was doing nothing for them.
