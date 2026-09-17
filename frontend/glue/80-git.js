    // Lives in #ghead OUTSIDE #app, so table-view re-renders/re-mounts never touch it.
    // The widget is a static readout; git writes stay explicit API actions.
    const createGitControl = () => {
      const GIT_POLL = 15000;   // ms between background re-reads
      const head = document.getElementById("ghead");
      /** @type {any} */ let ctl = null;
      let elDir = null, elBranch = null, elGlyph = null, elDot = null, elN = null;
      /** @type {GitStatus | null} */ let status = null;

      const baseName = (p) => {
        const parts = String(p || "").split("/").filter(Boolean);
        return parts.length ? parts[parts.length - 1] : "";
      };

      function build() {
        ctl = document.createElement("span");
        ctl.id = "gitctl";
        ctl.hidden = true;
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
        head.appendChild(ctl);
      }

      function render() {
        if (!head) return;
        if (!status) return;
        if (!ctl) build();
        ctl.hidden = !status.repo;
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
      }

      async function poll() {
        try {
          status = await /** @type {Promise<GitStatus>} */ (getJSON("/git"));
        } catch (e) { return; /* keep the last glyph */ }
        render();
      }

      build();
      poll();
      window.addEventListener("focus", poll);
      setInterval(poll, GIT_POLL);
    };
    createGitControl();
