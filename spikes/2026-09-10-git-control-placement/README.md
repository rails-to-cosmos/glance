# Spike — where the git control should live

**Date:** 2026-09-10 · Brainstorm, to pick a redesign. Not wired into the main code.

**Open `index.html`.** Pick a placement, then **re-render strip** (table-view's
`chipsEl.innerHTML = …`) or **re-mount table** (what `g` does) and watch the
"control present" verdict.

## The reason for the instability (root cause)

The current control is a foreign node **injected into `.tv-chips`, which
table-view owns**. table-view reclaims that subtree two ways:

- `renderChips()` sets `chipsEl.innerHTML` on every chip render — wipes the node.
- Re-mount (`g`) does `container.innerHTML=""; appendChild(root)` — a *new*
  `.tv-chips` element, orphaning any observer on the old one.

So every fix is a *reaction* with a gap: a strip observer (misses re-mount), an
`#app` observer (catches re-mount), inline-style re-apply for the pin (fights
table-view's CSS on a recreated node), a 15 s poll (the visible flicker).
Consistency can't be guaranteed by reacting to a component built to reclaim its
DOM. The control's host node must be one table-view **never touches**.

Flip to **Old (injected)** and press re-mount: the verdict goes `no`. That is the bug.

## The three redesigns (each guarantees consistency — no observers)

| | Where it lives | Repos | Look | Trade-off |
|---|---|---|---|---|
| **A · table-view slot** | a persistent `.tv-aside` table-view owns and re-offers on re-mount | glance **+** `../table-view` | exact in-bar, at the strip's end | cross-repo change + `make sync-renderer` + AGENTS.hs |
| **B · shell header row** | `#ghead` above the table, in the shell's own DOM | glance only | a right-aligned row above the bar | its own row, not inside the bar |
| **C · corner badge** | fixed to the top-right, in the shell's own DOM | glance only | a floating pill in the corner | detached from the bar |

All three put the control's host node outside table-view's owned/rewritten
subtree, so re-render and re-mount leave it untouched — consistency by
construction, no observers, no CSS fight, no poll flicker.

## The recommendation

**A** if the in-bar look matters and touching `../table-view` is fine — it is the
correct "host owns the slot" design and keeps exactly what you built. table-view
already does this for its own pin (`onPin`); a general `barAside` option is the
same shape.

**B** if you want it shipped glance-only today with zero table-view risk — the
simplest thing that is guaranteed correct.

## What a real build would do (per option)

- **A** — `../table-view`: build a `.tv-aside` into the strip row once at mount
  (a flex sibling of `.tv-chips`, never rewritten by `renderChips`), and call
  `o.barAside(asideEl)` after mount and after each re-mount. `make sync-renderer`
  into `assets/table-view.js`; AGENTS.hs notes the renderer bump. glance's
  `00-core.js` mount passes `barAside: el => gitControl(el)`; `80-git.js` renders
  into the given `el` and drops all the inject/observe/place code.
- **B** — `Page.hs`: add `<div id="ghead"></div>` to the skeleton; `80-git.js`
  fills it, `#ghead:empty { display:none }` collapses it off a non-repo dir.
- **C** — `Page.hs`: add `<div id="gitctl" hidden></div>`; `page.css` fixes it
  top-right; `80-git.js` fills it. No table-view interaction at all.
