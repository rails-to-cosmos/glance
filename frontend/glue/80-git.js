    // The git sync control: a one-glance status of the served dir at the end of
    // table-view's own chip strip.  A click runs the state's one safe step
    // (clean fetches, behind pulls, ahead pushes, dirty commits + pushes,
    // diverged syncs); auto-sync (Model B) is a second, opt-in control.  It
    // appears ONLY when the served dir is a git work tree, so a non-git tree
    // (every test fixture) sees no added DOM.  Backend: GET/POST /git.
    {
      const GIT_POLL = 15000;   // ms between background re-reads
      /** @type {any} */ let ctl = null;
      let elDir = null, elBranch = null, elGlyph = null,
          elDot = null, elN = null, elAuto = null, elFlash = null;
      /** @type {any} */ let status = null;
      /** @type {any} */ let watched = null, obs = null;
      let placing = 0;

      // The one obvious step per state — mirrors the backend's `actionFor'.
      const glyphFor = (s) => {
        const dirty = s.staged + s.unstaged + s.untracked;
        if (s.detached || !s.upstream)
          return { dot: "⚠", n: "", cls: "g-detached", action: null,
                   label: "detached / no upstream — nothing safe to one-click" };
        if (dirty > 0 && !s.ahead && !s.behind)
          return { dot: "●", n: String(dirty), cls: "g-dirty", action: "commit-push",
                   label: `${dirty} uncommitted — commit + push` };
        if (s.ahead && s.behind)
          return { dot: "↕", n: `${s.behind}↓ ${s.ahead}↑`, cls: "g-diverged", action: "sync",
                   label: "diverged — pull --rebase, then push" };
        if (dirty > 0)
          return { dot: "●", n: String(dirty), cls: "g-dirty", action: "commit-push",
                   label: `${dirty} uncommitted — commit + push` };
        if (s.behind)
          return { dot: "↓", n: String(s.behind), cls: "g-behind", action: "pull",
                   label: `${s.behind} behind — pull` };
        if (s.ahead)
          return { dot: "↑", n: String(s.ahead), cls: "g-ahead", action: "push",
                   label: `${s.ahead} ahead — push` };
        return { dot: "✓", n: "", cls: "g-clean", action: "fetch", label: "up to date — fetch" };
      };

      const baseName = (p) => {
        const parts = String(p || "").split("/").filter(Boolean);
        return parts.length ? parts[parts.length - 1] : "";
      };

      // The last meaningful line of a git command's output, dropping ssh/library
      // chatter (e.g. openssh's post-quantum warning) that is not the result.
      const NOISE = /post-quantum|decrypt later|openssh\.com\/pq|need to be upgraded|^\*\*/i;
      const tidyLine = (s) => String(s || "").split("\n").map((l) => l.trim())
        .filter(Boolean).filter((l) => !NOISE.test(l)).pop() || "";

      // Build the control once (only reached when the dir is a repo).
      function build() {
        ctl = document.createElement("span");
        ctl.id = "gitctl";
        ctl.innerHTML =
          '<span class="g-at">@</span>'
          + '<span class="g-loc"><span class="g-vc">⎇</span> '
          + '<span class="g-dir"></span>:<span class="g-branch"></span></span>'
          + '<button class="g-glyph" type="button">'
          + '<span class="g-dot"></span><span class="g-n"></span></button>'
          + '<button class="g-auto" type="button">⟳</button>'
          + '<span class="g-flash"></span>';
        elDir = ctl.querySelector(".g-dir");
        elBranch = ctl.querySelector(".g-branch");
        elGlyph = ctl.querySelector(".g-glyph");
        elDot = ctl.querySelector(".g-dot");
        elN = ctl.querySelector(".g-n");
        elAuto = ctl.querySelector(".g-auto");
        elFlash = ctl.querySelector(".g-flash");
        elGlyph.addEventListener("click", act);
        elAuto.addEventListener("click", toggleAuto);
      }

      // Attach the control to the visible chip strip; the strip is rewritten on
      // every chip render, so a MutationObserver re-attaches it.  Returns false
      // when the strip is not mounted yet.
      // Table-view pushes its pin to the right with margin-left:auto; move it to
      // the far left instead, so the control sits at the strip's end alone.
      // Inline styles, re-applied on every re-render, beat the vendored
      // stylesheet without a fragile cascade fight.
      function place(strip) {
        if (!strip.contains(ctl)) strip.appendChild(ctl);
        const pin = strip.querySelector(".tv-pin");
        if (pin) { pin.style.order = "-1"; pin.style.marginLeft = "0"; }
      }

      function attach() {
        const strip = document.querySelector("#app .tv-chips");
        if (!strip) return false;
        if (!ctl) build();
        if (watched !== strip && typeof MutationObserver === "function") {
          if (obs) obs.disconnect();
          obs = new MutationObserver(() => {
            const s = document.querySelector("#app .tv-chips");
            if (s) place(s);
          });
          obs.observe(strip, { childList: true });
          watched = strip;
        }
        place(strip);
        return true;
      }

      function detach() {
        if (obs) { obs.disconnect(); obs = null; watched = null; }
        if (ctl && ctl.parentNode) ctl.parentNode.removeChild(ctl);
      }

      // Render the control from the last /git, attaching or removing it as the
      // dir is or is not a repo.  Retries attachment while the mount is async.
      function render() {
        if (!status || !status.repo) { detach(); return; }
        if (!attach()) {
          if (placing++ < 40) setTimeout(render, 150);
          return;
        }
        placing = 0;
        const s = status;
        elDir.textContent = baseName(s.dir);
        elBranch.textContent = s.branch || "(detached)";
        const g = glyphFor(s);
        elGlyph.className = "g-glyph " + g.cls;
        elDot.textContent = g.dot;
        elN.textContent = g.n;
        elGlyph.title = g.label + (g.action ? " (click)" : "");
        elGlyph.disabled = !g.action;
        const on = !!(s.autosync && s.armed);
        elAuto.className = "g-auto" + (on ? " g-on" : "");
        elAuto.title = on ? "auto-sync on — click to turn off"
          : s.autosync ? "auto-sync set — click again to allow the first push"
          : "auto-sync off — click to enable";
      }

      function flash(msg) {
        if (!elFlash) return;
        elFlash.textContent = msg;
        setTimeout(() => { if (elFlash) elFlash.textContent = ""; }, 4000);
      }

      async function poll() {
        try { status = await getJSON("/git"); } catch (e) { return; /* keep the last glyph */ }
        render();
      }

      async function act() {
        const g = status && status.repo ? glyphFor(status) : null;
        if (!g || !g.action) return;
        try {
          const r = await postJSON("/git", { action: g.action }).then((x) => x.json());
          flash(r.ok ? `${g.action} ✓` : `${g.action} failed`);
          // A concise line, never the raw subprocess dump: the glyph shows the
          // new state; the log just says what ran and, on failure, why.
          if (typeof append === "function")
            append("git", r.ok ? "info" : "warn",
                   r.ok ? `${g.action} ✓`
                        : `${g.action} failed: ${tidyLine(r.error || r.output) || "see the server log"}`);
        } catch (e) { flash("failed"); }
        poll();
      }

      // Off → enable (set, not yet armed) → arm (allow the first push) → off.
      // Two deliberate clicks to publish, no blocking dialog.
      async function toggleAuto() {
        if (!status || !status.repo) return;
        const step = status.autosync && status.armed ? "autosync-off"
          : status.autosync ? "arm"
          : "autosync-on";
        try { await postJSON("/git", { action: step }); }
        catch (e) { flash("failed"); }
        poll();
      }

      poll();
      window.addEventListener("focus", poll);
      setInterval(poll, GIT_POLL);
      // `g' (and any full re-mount) replaces #app's child with a fresh table, so
      // a new .tv-chips the strip-scoped observer never saw.  Watch #app's own
      // children and re-place at once, rather than waiting for the next poll.
      const app = document.getElementById("app");
      if (app && typeof MutationObserver === "function")
        new MutationObserver(() => { if (status && status.repo) render(); })
          .observe(app, { childList: true });
    }
