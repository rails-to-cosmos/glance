    // Lives in #ghead OUTSIDE #app, so table-view re-renders/re-mounts never touch it.
    {
      const GIT_POLL = 15000;   // ms between background re-reads
      const head = document.getElementById("ghead");
      /** @type {any} */ let ctl = null;
      let elDir = null, elBranch = null, elGlyph = null,
          elDot = null, elN = null, elAuto = null, elFlash = null;
      /** @type {any} */ let status = null;

      // Mirrors the backend's `actionFor'.
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

      // Last meaningful output line, dropping ssh/library noise.
      const NOISE = /post-quantum|decrypt later|openssh\.com\/pq|need to be upgraded|^\*\*/i;
      const tidyLine = (s) => String(s || "").split("\n").map((l) => l.trim())
        .filter(Boolean).filter((l) => !NOISE.test(l)).pop() || "";

      function build() {
        ctl = document.createElement("span");
        ctl.id = "gitctl";
        ctl.innerHTML =
          '<span class="g-at">@</span>'
          + '<span class="g-loc"><span class="g-vc">⎇</span> '
          + '<span class="g-dir"></span>:<span class="g-branch"></span></span>'
          + '<button class="g-glyph" type="button">'
          + '<span class="g-dot"></span><span class="g-n"></span></button>'
          + '<button class="g-auto" type="button">⇄</button>'
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
        head.appendChild(ctl);
      }

      function render() {
        if (!head) return;
        if (!status || !status.repo) { if (ctl) { ctl.remove(); ctl = null; } return; }
        if (!ctl) build();
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
          // Log a tidied one-liner; the glyph already shows the new state.
          if (typeof append === "function")
            append("git", r.ok ? "info" : "warn",
                   r.ok ? `${g.action} ✓`
                        : `${g.action} failed: ${tidyLine(r.error || r.output) || "see the server log"}`);
        } catch (e) { flash("failed"); }
        poll();
      }

      // Off → enable → arm → off: two deliberate clicks guard the first push.
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
    }
