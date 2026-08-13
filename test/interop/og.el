;;; og.el --- The Emacs half of glance's interop harness  -*- lexical-binding: t -*-

;; ONE JOB PER INVOCATION, and the whole interface is the environment: `ICMD'
;; names the step, `IROOT' the store, `IID'/`ITEXT' whatever it needs.  Every
;; step prints ONE JSON object on stdout and nothing else, so the driver parses
;; a line rather than scraping a session.  A step that cannot run prints
;; {"error": …} and exits 1.
;;
;; It lives in glance's repo and reads org-glance's LIVE sources -- never the
;; copies `eask install' left under .eask/elpa, which are as old as the last
;; release.  `OG_HOME' names the checkout; the deps beside it are eask's.
;;
;; THIS FILE ONLY REPORTS.  Every assertion is the driver's -- it prints what
;; Emacs says and lets the other side compare, so a step that quietly agreed
;; with itself is impossible.

(setq load-prefer-newer t)          ; a stale .elc must never answer for a .el

(defun og-load-path ()
  "Put org-glance's live sources and eask's dependencies on `load-path'.
Reads `OG_HOME'.  The installed org-glance copies under .eask are skipped: the
checkout is the thing under test."
  (let ((home (or (getenv "OG_HOME") (error "OG_HOME is unset"))))
    (unless (file-directory-p (expand-file-name "src/data" home))
      (error "no org-glance sources under %s" home))
    (let ((deps (file-expand-wildcards (expand-file-name ".eask/*/elpa/*" home))))
      (unless deps
        (error "no dependencies under %s/.eask -- run `eask install-deps' there" home))
      (dolist (dir deps)
        (when (and (file-directory-p dir)
                   (not (string-match-p "/org-glance-[0-9]" dir)))
          (add-to-list 'load-path dir))))
    (dolist (dir (list home
                       (expand-file-name "src/view" home)
                       (expand-file-name "src/data" home)))
      (add-to-list 'load-path dir))))

(defun og-say (plist)
  "Print PLIST as one line of JSON on stdout."
  (princ (json-serialize plist))
  (terpri))

(defun og-env (name)
  "Value of environment variable NAME, or an error naming it."
  (or (getenv name) (error "%s is unset" name)))

(defun og-graph ()
  "The graph at `IROOT'."
  (org-glance-graph (og-env "IROOT")))

(defmacro og-unfolding (&rest body)
  "Run BODY with the read path's automatic external fold disabled.
A read folds pending notifications in by itself, which is exactly what a step
asking WHETHER a notification is pending must not do."
  (declare (indent 0))
  `(let ((org-glance-graph-external-poll-seconds 1.0e12)) ,@body))

(defun og-capture (graph id text)
  "Add TEXT to GRAPH under ID, through the public capture path."
  (with-temp-buffer
    (org-mode)
    (insert text)
    (goto-char (point-min))
    (org-entry-put nil "ORG_GLANCE_ID" id)
    (org-glance-graph:capture graph (current-buffer)))
  id)

(defun og-meta-files (graph)
  "GRAPH's `meta/' as an alist of (NAME . SHA1 of its bytes), name-sorted.
Dotfiles included: `.gitattributes' is org-glance's own and glance must leave
it where it is like everything else here."
  (let ((dir (org-glance-graph:meta-path graph)))
    (sort (mapcar (lambda (f)
                    (cons (file-name-nondirectory f)
                          (secure-hash 'sha1 (with-temp-buffer
                                               (set-buffer-multibyte nil)
                                               (insert-file-contents-literally f)
                                               (buffer-string)))))
                  (seq-filter #'file-regular-p (directory-files dir t)))
          (lambda (a b) (string< (car a) (car b))))))

(defun og-meta-digest (graph)
  "GRAPH's `meta/' digest as a vector of \"NAME SHA1\" strings."
  (vconcat (mapcar (lambda (kv) (format "%s %s" (car kv) (cdr kv)))
                   (og-meta-files graph))))

(defun og-field (graph id)
  "What GRAPH says about ID: liveness, keyword, title and tags."
  (let ((meta (org-glance-graph:get-headline graph id)))
    (cond
     ((null meta) (list :live :false :tombstone :false))
     ((eq meta 'tombstone) (list :live :false :tombstone t))
     (t (list :live t
              :tombstone :false
              :state (or (org-glance-headline-metadata:state meta) "")
              :title (or (org-glance-headline-metadata:title meta) "")
              :archived (if (org-glance-headline-metadata:archived? meta) t :false)
              :tags (vconcat (org-glance-headline-metadata:tags meta)))))))

(defun og-external-entry (entry)
  "One `--read-external' entry as an (ID . KIND-NAME) cons, either peer's shape.
A peer that reads the third field answers an (ID . KIND) cons per id; one that
predates it answers the bare id string, which is `edit' -- such a reader has no
tombstone to tell apart, so every line it yields is a re-derivation hint.  This
harness asserts the FILE stays readable across that boundary and is held to the
same standard, or the step dies with a type error where the target promises to
skip loudly."
  (if (consp entry)
      (cons (car entry) (symbol-name (cdr entry)))
    (cons entry "edit")))

(defun og-external-pending (graph)
  "GRAPH's unfolded notification bytes, every source in the fold's own order.
Each source's tail, which is what the fold would take next -- rotated
generations first and the live file last."
  (mapconcat (lambda (path) (car (org-glance-graph--external-tail path)))
             (org-glance-graph--external-sources graph) ""))

(defun og-external-cursor (graph)
  "How many of GRAPH's live notification file's bytes have been folded.
ABSENCE IS REPORTED AS ABSENCE: a peer that keeps no cursor answers `:null'
rather than 0, 0 being a real offset a fold can leave.  Reporting the two alike
let a step assert a cursor had reached the end of a file nothing had folded."
  (if (fboundp 'org-glance-graph--external-folded)
      (org-glance-graph--external-folded (org-glance-graph:external-path graph))
    :null))

(defun og-external-pending-p (graph)
  "Whether GRAPH's READ PATH would fold, as `t' or `:false'.
The other half of the cursor, and the one the fold cannot answer for: a read
folds only what this predicate says is owed, so a peer that reads the file
correctly and POLLS it by size alone leaves a re-laid file unfolded forever.
`:null' where the peer has no such predicate at all."
  (if (fboundp 'org-glance-graph--external-pending-p)
      (if (org-glance-graph--external-pending-p graph) t :false)
    :null))

(defun og-main ()
  "Run the step `ICMD' names."
  (let* ((cmd (og-env "ICMD")))
    (pcase cmd
      ;; A STORE ORG-GLANCE MADE, not one this harness hand-wrote: the tag's
      ;; own config layer, then two entries through the public capture path.
      ("seed"
       (let* ((graph (og-graph))
              (tag (og-env "ITAG"))
              (layer (org-glance-graph:config-file graph (format "tags/%s.org" tag))))
         (make-directory (file-name-directory layer) t)
         ;; The layer org-glance itself writes for a tag: the cycle, then the
         ;; bare `* %?' skeleton (`org-glance-tag-config' calls it the stub).
         (with-temp-file layer
           (insert (format "#+TITLE: %s\n#+TODO: TODO READING | READ\n\n* %%?\n" tag)))
         (og-capture graph (og-env "IALPHA") "* TODO alpha\n")
         (og-capture graph (og-env "IBETA") (format "* beta :%s:\n" tag))
         (og-say (list :store (org-glance-graph:store-path graph)
                       :external (org-glance-graph:external-path graph)
                       :alpha (org-glance-graph:content-path graph (og-env "IALPHA"))
                       :beta (org-glance-graph:content-path graph (og-env "IBETA"))))))

      ;; Where org-glance looks for ID's blob, and where it looks for the
      ;; notification file.  Both as STRINGS, for the other side to compare.
      ("paths"
       (og-unfolding
         (let ((graph (og-graph)))
           (og-say (list :content (org-glance-graph:content-path graph (og-env "IID"))
                         :external (org-glance-graph:external-path graph))))))

      ;; The notification file as org-glance READS it -- the ids it has still to
      ;; fold, in file order, the KIND it read each one as beside them, and the
      ;; pending text.  No fold: this only reports.  `og-external-entry'
      ;; normalizes whichever shape the peer answers, so the two vectors are
      ;; that list split -- `ids' stays what it always was and `kinds' is the
      ;; third field's whole visible effect on this side, reading "edit"
      ;; throughout against a peer that cannot see the field.  `bytes' is the
      ;; file's own size and `cursor' how much of it is spent (null where the
      ;; peer keeps none), so the driver can read BOTH facts the fold moved: the
      ;; file keeps its bytes, and the ids in them stop being owed.  `pending'
      ;; is the READ PATH's own answer beside them -- what an ordinary read
      ;; would do, which is not recoverable from the other three.
      ("read-external"
       (og-unfolding
         (let* ((graph (og-graph))
                (path (org-glance-graph:external-path graph))
                (there (file-exists-p path))
                (entries (mapcar #'og-external-entry
                                 (plist-get (org-glance-graph--read-external graph)
                                            :entries))))
           (og-say (list :exists (if there t :false)
                         :bytes (if there (org-glance--file-size path) -1)
                         :cursor (og-external-cursor graph)
                         :pending (og-external-pending-p graph)
                         :inode (if there
                                    (format "%s" (file-attribute-inode-number
                                                  (file-attributes path)))
                                  "")
                         :text (og-external-pending graph)
                         :ids (vconcat (mapcar #'car entries))
                         :kinds (vconcat (mapcar #'cdr entries)))))))

      ;; The fold itself, unthrottled.
      ("refresh"
       (let ((graph (og-graph)))
         (og-say (list :n (org-glance-graph:refresh-external graph)))))

      ;; What the WAL says about ID, with no fold in front of it.
      ("field"
       (og-unfolding (og-say (og-field (og-graph) (og-env "IID")))))

      ;; org-glance writing a blob of its own -- the Emacs->browser leg, and it
      ;; names no notification file at all.
      ("put"
       (og-unfolding
         (let* ((graph (og-graph))
                (id (og-env "IID"))
                (path (org-glance-graph:content-path graph id))
                (text (og-env "ITEXT")))
           (unless (file-exists-p path) (error "no blob at %s" path))
           (org-glance-graph:put-content
            graph (org-glance-headline--from-string text))
           ;; The clock AFTER the rename, so the driver's latency is the watch's
           ;; own and not this process's start-up.
           (og-say (list :path path :bytes (length text)
                         :at (round (* 1000 (float-time))))))))

      ("content"
       (og-unfolding
         (let* ((graph (og-graph))
                (text (org-glance-graph:get-content graph (og-env "IID"))))
           (og-say (list :text (or text "") :exists (if text t :false))))))

      ("meta-digest"
       (og-unfolding (og-say (list :files (og-meta-digest (og-graph))))))

      (_ (error "no step named %s" cmd)))))

(og-load-path)
(require 'org-glance)

(condition-case err
    (og-main)
  (error (princ (json-serialize (list :error (format "%S" err)))) (terpri)
         (kill-emacs 1)))

;;; og.el ends here
