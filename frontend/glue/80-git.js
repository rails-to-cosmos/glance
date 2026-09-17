    // Lives in #ghead OUTSIDE #app, so table-view re-renders/re-mounts never touch it.
    // The glyph is a static readout; only the auto-sync toggle takes a click.
    /** @param {{pageAddress: () => PageAddress, onPageBack: () => void}} deps */
    const createGitControl = (deps) => {
      const { pageAddress, onPageBack } = deps;
      const GIT_POLL = 15000;   // ms between background re-reads
      const head = document.getElementById("ghead");
      /** @type {any} */ let ctl = null;
      let elPage = null, elDir = null, elBranch = null, elGlyph = null,
          elDot = null, elN = null, elAuto = null, elFlash = null;
      /** @type {GitStatus | null} */ let status = null;

      const baseName = (p) => {
        const parts = String(p || "").split("/").filter(Boolean);
        return parts.length ? parts[parts.length - 1] : "";
      };

      function build() {
        ctl = document.createElement("span");
        ctl.id = "gitctl";
        elPage = part(ctl, "button", "g-page", "default");
        elPage.type = "button"; elPage.disabled = true;
        part(ctl, "span", "g-at", " @");
        const loc = part(ctl, "span", "g-loc");
        part(loc, "span", "g-vc", "⎇");
        loc.appendChild(document.createTextNode(" "));
        elDir = part(loc, "span", "g-dir");
        loc.appendChild(document.createTextNode(":"));
        elBranch = part(loc, "span", "g-branch");
        elGlyph = part(ctl, "span", "g-glyph");
        elDot = part(elGlyph, "span", "g-dot");
        elN = part(elGlyph, "span", "g-n");
        elAuto = part(ctl, "button", "g-auto", "⇄");
        elAuto.type = "button";
        elFlash = part(ctl, "span", "g-flash");
        elAuto.addEventListener("click", toggleAuto);
        elPage.addEventListener("click", onPageBack);
        head.appendChild(ctl);
        renderPage();
      }

      function renderPage() {
        if (!elPage) return;
        const address = pageAddress();
        elPage.textContent = address.label;
        elPage.disabled = !address.back;
        elPage.title = address.back ? "back to the default view" : "";
      }

      function render() {
        if (!head) return;
        if (!status) return;
        if (!ctl) build();
        ctl.classList.toggle("g-norepo", !status.repo);
        renderPage();
        if (!status.repo) return;
        const s = status;
        elDir.textContent = baseName(s.dir);
        elBranch.textContent = s.branch || "(detached)";
        const dirty = s.staged + s.unstaged + s.untracked;
        const count = s.locked ? ""
          : dirty ? String(dirty)
          : s.ahead && s.behind ? `${s.behind}↓ ${s.ahead}↑`
          : s.behind ? String(s.behind)
          : s.ahead ? String(s.ahead) : "";
        elGlyph.className = "g-glyph " + s.cls;
        elDot.textContent = s.glyph;
        elN.textContent = count;
        elGlyph.title = s.label;
        const on = !!(s.autosync && s.armed);
        elAuto.className = "g-auto" + (on ? " g-on" : "");
        elAuto.title = on ? "auto-sync on — click to turn off"
          : s.autosync ? "auto-sync set — click again to allow the first push"
          : "auto-sync off — click to enable";
        renderPage();
      }

      function flash(msg) {
        if (!elFlash) return;
        elFlash.textContent = msg;
        setTimeout(() => { if (elFlash) elFlash.textContent = ""; }, 4000);
      }

      async function poll() {
        try {
          status = await /** @type {Promise<GitStatus>} */ (getJSON("/git"));
        } catch (e) { return; /* keep the last glyph */ }
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

      build();
      poll();
      window.addEventListener("focus", poll);
      setInterval(poll, GIT_POLL);
      return { pageChanged: renderPage };
    };
    const GitControl = createGitControl(
      { pageAddress: () => Pages.address(), onPageBack: () => leaveSession() });
