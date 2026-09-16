# Improvement recommendations — 2026-09-16

This is the ordered stabilization plan from the 2026-09-16 repository review.
The order matters: settle persistence and data integrity before rearranging the
large modules that implement them.

1. **Choose the persistence direction.** Treat Org files and blobs as the log,
   keep derived state in a local SQLite cache, and let git carry only source
   files. The decision is recorded in
   `proposals/done/2026-09-16-the-files-are-the-log.md`.
2. **Clear the data-integrity bugs.** Keep `EXTERNAL.jsonl` and
   `COMPLETIONS.jsonl` out of git and out of the union merge driver; make a
   stale `index.lock` visible and let auto-sync finish its child on shutdown;
   make git actions a closed, total sum; move a newly tagged inbox jot into its
   blob; and preserve the peer-adoption interop oracle until org-glance lands
   its half.
3. **Make printed registries verifiable.** Compare the model's key and popup
   registries directly with `Glance.Web.Keymap` and
   `Glance.Web.Page.Popups` in `TestSpec`.
4. **Guard the riskiest `Unguarded` notes.** Start with digest-locked writes,
   index rotation and replay, git synchronization, editor lifecycle, and
   cross-language registries. Convert a small risk-ranked batch rather than
   optimizing the count.
5. **Join duplicated registries.** Compare `rowSummaryPairs` with
   `viewColumns`, and enforce membership in the JavaScript `DOCEDITS` roster.
6. **Separate environmental gates.** Keep the deterministic suite green where
   sockets, Chromium, a corpus, or Emacs are absent; run each environmental
   tier in CI where its prerequisite exists and make every skip explicit.
7. **Extract only proven hotspots.** Preserve `Glance.Query` as the public
   facade while moving cohesive implementation behind it. Split
   `20-sheet.js` by complete editor lifecycle, and keep `Routes.hs` as the
   route table while moving substantial handlers behind it.
8. **Verify embedded assets.** Rebuild and compare Elm output, and compare the
   vendored table renderer with its source checkout when that checkout exists.

The intended sequence is persistence decision, integrity fixes, registry
checks, high-risk unguarded rules, then focused extraction.
