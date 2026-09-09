# Bug — the live store tracks `EXTERNAL.jsonl` in git and union-merges every `*.jsonl`

**Status:** open · **Reported:** 2026-09-10 (`~/sync/views`, a git-synced
store) · **Surface:** `--ensure-gitignore`, `.gitattributes`, the fold on a
second host

## Symptom

- `git ls-files .org-glance/meta` lists `EXTERNAL.jsonl`, `COMPLETIONS.jsonl`,
  `MANIFEST`, the open segment and both sealed segments.
- `.org-glance/meta/.gitattributes` reads `*.jsonl merge=union`.
- `doctor` reports *30 org-glance records with no blob*.

## Why it is wrong

- org-glance's contract says the notification family is a LOCAL hint,
  git-ignored (`org-glance-graph.el:880-882`, `--ensure-gitignore` `:1545`),
  and invariant 8 says the union resolver names `headlines.jsonl` and
  `seg-*.jsonl` and nothing else — the `*.jsonl` glob is the spelling the
  invariant text says was retired because it *"gave the union driver every
  excluded"* file.
- With the note file synced, host B folds host A's notes. A note is an id;
  the blob may not have synced yet, or may never (A deleted it after noting a
  write). The fold produces a record for a blob B does not have — the
  *records with no blob* count — and `doctor` cannot tell that from a real
  orphan.
- `COMPLETIONS.jsonl` (the daemon's repeat ledger) travels the same way.

## Steps to reproduce

1. On host A, capture through `glance mcp` (a note lands in `EXTERNAL.jsonl`).
2. Commit and push meta before the blob's shard, or delete the blob.
3. On host B, pull and open Emacs: the fold adopts the note; the record has
   no blob.

## Proposed fix

- In the store: `git rm --cached .org-glance/meta/EXTERNAL.jsonl
  .org-glance/meta/COMPLETIONS.jsonl`, replace the `.gitattributes` glob with
  the two predicates inv 8 names, and let `--ensure-gitignore` write its
  ignore lines. A one-time repair; `doctor` should then recount.
- In org-glance: `--ensure-gitignore` runs only when the file is ABSENT
  (invariant 8's last paragraph); it should also REPAIR a `.gitattributes`
  that carries the retired glob, or at least warn.
- In glance: `doctor` grows a check — *notification files tracked by git* —
  with the `git ls-files` answer, so a synced store says so.
