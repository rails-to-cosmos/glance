# Proposal — one writer per file, and a version the client can check

**Status:** proposed · **Date:** 2026-08-18 · **Origin:** user — *"what
concurrency guarantees do we provide right now in the material doc? At least we
should ensure the ordering of operations. What else? Serializability?"* — after
a read of the whole write path. The durability half of the answer LANDED the
same day and is out of scope here (see "What already landed").

Serializability is the wrong target. There are no multi-object transactions
worth serializing, and buying them costs a write-ahead log or a lock manager
over a tree of files that emacs also writes. The right target is **per-file
linearizability**, which the digest CAS almost provides, plus **a version the
client can check**, which nothing provides today.

## What already holds

| guarantee | mechanism | where |
|---|---|---|
| atomic snapshot reads | every handler takes one immutable `Store` | `Routes.hs:450`, `:498` |
| store swap and frames are one transaction | `publish` writes the TVar and enqueues to every client inside one `atomically` | `Store.hs:332-347` |
| total order of store versions | one `TVar Store`, so all publishes linearize | `Store.hs:274` |
| per-client FIFO, gap-free or dead | `TBQueue` of 1024; overflow marks the client dropped and removes it, never skips a frame | `Store.hs:292-293`, `:346-347` |
| the file never tears | temp file beside it, `hFlush`, `fileSynchronise`, permissions copied, `renameFile` | `src/Data/Org/Edit.hs:225` |
| a refused write leaves the file byte-identical | the only mutation is the rename | `Query.hs:1088-1093` |
| the write is a compare-and-swap on the content digest | `currentText` re-digests the bytes and refuses on mismatch | `Edit.hs:205-213` |
| two distinct refusals | 409 `stale` is the store's digest; 409 `drift` is the file's own re-digest | `Routes.hs:463`, `Base.hs:148` |
| one clock read per command | a marked set cannot straddle midnight | `Commands.hs:255` |
| the sheet will not double-save | `saveSheet` returns early while `state === "syncing"` | `20-sheet.js:884` |

That is a decent floor: files never corrupt, clients never see a torn store, and
a client that falls behind is disconnected rather than quietly skipped.

## What already landed

**The rename was atomic and not durable.** `writeAtomically` fsynced the temp
file and renamed it, and never fsynced the directory the entry lands in, so a
crash could take back a write that had answered 200 — silent loss of an edit the
reader watched turn "synced". Fixed 2026-08-18: the containing directory is
fsynced after the rename, and a write that had to create directories syncs each
new one's parent (`Edit.hs`, `syncDirectory` and `unmadeAncestors`;
`AGENTS.hs` `cmdNotes`; a case in `TestEdit.hs` covers the created-directory
branch).

## The defect this proposal opens with

**The CAS is not atomic, so a concurrent write is a silent lost update.**

```
currentText snap    -- read the bytes, compare the digest      Edit.hs:205-213
applyEdits doc      -- pure                                    Edit.hs:198
writeAtomically     -- temp, fsync, rename, fsync the dir      Edit.hs:200
```

There is no lock. `grep -n 'MVar' src-web/Glance/Web/*.hs` finds none on the
write path, and warp forks a thread per connection. Two requests pinning the
same digest D both pass the check, both build from D, both rename. **The second
rename wins, the first edit vanishes, and both clients get 200 with a digest.**

The window is read + parse + splice + fsync + rename, so milliseconds rather
than microseconds — the fsync widens it. Two connections is the
browser's default, and `beforeunload`'s keepalive POST (`20-sheet.js:924`)
overlaps a live save by construction.

This is bug-shaped and could equally be a file under `docs/bugs/`. It leads here
because the fix is the same one line of design as everything below.

## What else is unguaranteed

**No serializability across files, and none is claimed.** `Commands.hs:204` —
*"one drift-locked write per file, no rollback across files"* — and
`AGENTS.hs` says it again: *"a 200 means the command RAN, never that every row
moved."* The atom is the file. The request is not an atom. Results are per id,
which is the honest half.

**The store digest is a lagging cache.** `prepare` (`Routes.hs:459-466`) checks
the client's digest against `hrDigest r`, which is the last **parse** — updated
by the watcher after `debounceDelay = 0.1`, a 25 ms tick and the parse itself.
So the early-out can refuse a client that wrote and correctly re-pinned. The
client papers over it: *"THE STORE LAGS THE WRITE: any digest but the 200's own
is dropped, retried once"*, a 300 ms `setTimeout` at `20-sheet.js:254-266`. A
workaround standing where a guarantee should be.

**No read-your-writes.** Same cause. `GET /headline` after a 200 may still
answer the pre-write digest.

**No write ordering across requests.** The sheet blocks a second save, not
save-against-command. Press `t` while a save is in flight and two writes race
one file; HTTP over several connections preserves nothing. The client already
patches around it by re-pinning from the per-id 200 (`20-sheet.js:953-956`).

**Reseed can walk the store backwards.** `AGENTS.hs:2062` — *"reseed builds the
fresh store OUTSIDE the transaction and installs it wholesale; make the loop
concurrent and any edit that landed during the walk is silently reverted."*
Files stay safe; the in-memory view can regress, and clients watch it regress.

**Capture ids are admittedly racy.** `Commands.hs:278-280` — *"A RACE, honestly:
`/command` never writes the store, so K is the last load's count."* Two captures
into the inbox compute the same K. The digest CAS usually saves it; if the
reparse lands between them, both succeed with a duplicate id and
`storeRecords` drops the loser.

**`beforeunload` is fire-and-forget.** `20-sheet.js:924` posts and reads no
answer, so a conflict there is a silently discarded edit.

**Conflict resolution is last-writer-wins by keypress.** `saveSheet` on conflict
does `refresh().then(flush)` — take the fresh digest, then write your buffer
over it. The sheet writes the WHOLE subtree span, so the overwrite clobbers a
concurrent external edit anywhere under that headline, wholesale, unmerged.
Commands write narrow spans, which is why they interleave better. A defensible
choice for a single-reader tool; it should be stated, and the reader should see
what the second `C-x C-s` is about to bury.

## The change

**1. A per-path write lock.** `TVar (Map FilePath (MVar ()))`, or an `MVar`
table, taken around read-check-splice-rename. It turns the digest CAS from
advisory into real and closes the lost update. The door is `editFile`
(`Query.hs:1099-1100` calls it *"THE DOOR every write leaves through"*), so the
lock belongs there and covers the REPL and the scanner as well as the daemon.

Stated up front: this serializes **this process**. An emacs write racing the
same file is still caught by the digest check rather than prevented; preventing
it would need the read and the rename under one inode guard, which is more than
a single-reader tool needs.

**2. Publish the reparse on the write path.** `writeSpans` holds the fresh bytes
and the fresh digest on the success branch (`Watch.hs:67-72`). Parsing that one
file before answering gives read-your-writes, and deletes the 300 ms retry, the
"store lags the write" comment, and most of the capture-id race in one move.
Cost: one parse on the request thread.

**3. A monotone store version, in every frame and every write receipt.** Then a
client can say *this frame predates my write* instead of inferring it from
digests. It is what the sheet's retry is approximating by hand, and it is the
"ordering of operations" the question asked for — ordering the client can
CHECK.

**4. Mint capture ids from the write, not from the store's row count.** The
write knows what landed.

**5. Show the conflict before burying it.** On the second `C-x C-s`, draw what
arrived — at minimum which fields differ — so last-writer-wins is a decision
rather than a reflex.

## The guarantees, written down

Into `AGENTS.hs` as notes with tests, since the spec is the contract:

- **Per file: linearizable.** Accepted writes to one file are totally ordered;
  a refused write leaves it byte-identical; a landed write survives a crash.
- **Across files: nothing.** N files is N writes, per-id results, no rollback
  and no isolation.
- **Store: monotone, versioned, and after change 2, read-your-writes.**
- **Clients: total order of versions, per-client FIFO, overflow disconnects
  rather than skips.**

## The tests

The suite already drives the WAI application in process (`Network.Wai.Test`,
`TestServe.hs:21`), so the concurrent case is cheap and is the one that matters:

1. **N threads, one pinned digest, one file.** Assert **exactly one 200 and
   N-1 409s**, and that the final bytes are one of the N intended documents and
   never a mix. This fails today.
2. A command across three files where the middle one drifts: two rows report
   ok, one reports the conflict, and the two that landed stay landed.
3. A reseed publishing while a write lands: the store does not regress past the
   write.
4. Read-your-writes: `GET /headline` immediately after a 200 answers the
   digest the 200 returned.
5. Two captures into one inbox from two threads mint two distinct ids.

## Shipping order

| step | what | why first |
|---|---|---|
| **1** | the per-path lock, and test 1 with it | closes silent data loss |
| **2** | reparse-and-publish on the write path | deletes two client-side workarounds |
| **3** | the store version in frames and receipts | makes ordering checkable |
| **4** | capture ids from the write | closes the last admitted race |
| **5** | the conflict shown before the overwrite | a decision instead of a reflex |

Steps 1 and 2 are worth doing together: the lock makes the write authoritative,
and the synchronous reparse makes the answer authoritative, and between them the
client can stop guessing.
