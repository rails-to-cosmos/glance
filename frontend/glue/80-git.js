    // Lives in #ghead OUTSIDE #app, so table-view re-renders/re-mounts never touch it.
    // The glyph is a static readout; only the auto-sync toggle takes a click.
    {
      const GIT_POLL = 15000;   // ms between background re-reads
      const head = document.getElementById("ghead");
      /** @type {any} */ let ctl = null;
      let elDir = null, elBranch = null, elGlyph = null,
          elDot = null, elN = null, elAuto = null, elFlash = null;
      /** @type {any} */ let status = null;

      // The dot, the count and the colour class per state; the order mirrors
      // the backend's `actionFor'.
      const glyphFor = (s) => {
        const dirty = s.staged + s.unstaged + s.untracked;
        if (s.detached || !s.upstream) return { dot: "⚠", n: "", cls: "g-detached" };
        if (dirty > 0 && !s.ahead && !s.behind)
          return { dot: "●", n: String(dirty), cls: "g-dirty" };
        if (s.ahead && s.behind)
          return { dot: "↕", n: `${s.behind}↓ ${s.ahead}↑`, cls: "g-diverged" };
        if (dirty > 0) return { dot: "●", n: String(dirty), cls: "g-dirty" };
        if (s.behind) return { dot: "↓", n: String(s.behind), cls: "g-behind" };
        if (s.ahead) return { dot: "↑", n: String(s.ahead), cls: "g-ahead" };
        return { dot: "✓", n: "", cls: "g-clean" };
      };

      // The glyph's hover text, spelled out of the `/git' fields.
      const titleFor = (s) => {
        const dirty = s.staged + s.unstaged + s.untracked;
        const hd = s.detached ? "detached HEAD" : s.branch || "no branch";
        const parts = [s.upstream ? `${hd} tracking ${s.upstream}` : hd];
        if (!s.upstream) parts.push("no upstream");
        if (dirty)
          parts.push(`${dirty} uncommitted (${s.staged} staged, `
            + `${s.unstaged} unstaged, ${s.untracked} untracked)`);
        if (s.behind) parts.push(`${s.behind} behind`);
        if (s.ahead) parts.push(`${s.ahead} ahead`);
        if (s.upstream && !dirty && !s.ahead && !s.behind) parts.push("up to date");
        return parts.join(" · ");
      };

      const baseName = (p) => {
        const parts = String(p || "").split("/").filter(Boolean);
        return parts.length ? parts[parts.length - 1] : "";
      };

      function build() {
        ctl = document.createElement("span");
        ctl.id = "gitctl";
        ctl.innerHTML =
          '<span class="g-at">@</span>'
          + '<span class="g-loc"><span class="g-vc">⎇</span> '
          + '<span class="g-dir"></span>:<span class="g-branch"></span></span>'
          + '<span class="g-glyph">'
          + '<span class="g-dot"></span><span class="g-n"></span></span>'
          + '<button class="g-auto" type="button">⇄</button>'
          + '<span class="g-flash"></span>';
        elDir = ctl.querySelector(".g-dir");
        elBranch = ctl.querySelector(".g-branch");
        elGlyph = ctl.querySelector(".g-glyph");
        elDot = ctl.querySelector(".g-dot");
        elN = ctl.querySelector(".g-n");
        elAuto = ctl.querySelector(".g-auto");
        elFlash = ctl.querySelector(".g-flash");
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
        elGlyph.title = titleFor(s);
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
