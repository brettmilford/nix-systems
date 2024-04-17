;;; +pkm.el --- Personal Knowledge Management config -*- lexical-binding: t; -*-
(setq org-directory "~/org")
(when (file-directory-p org-directory)
(add-to-list 'auto-mode-alist '("\\.org_archive\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.org.gpg\\'"     . org-mode))

(when window-system
  (load-theme 'modus-operandi t)
  (modus-themes-load-operandi)
  (setq
   doom-font (font-spec :family "Iosevka")
   doom-variable-pitch-font (font-spec :family "Iosevka Aile"))
  (after! org-modern
    (set-face-attribute 'org-modern-symbol nil :family "Iosevka"))
  (doom/reload-font)

  (after! org
    (global-org-modern-mode)
    (setq writeroom-fringes-outside-margins nil)
    (global-writeroom-mode)
    (setq
     org-auto-align-tags nil
     org-catch-invisible-edits 'show-and-error
     org-special-ctrl-a/e t
     org-insert-heading-respect-content t
     org-hide-emphasis-markers t
     org-ellipsis "…"
     org-agenda-tags-column 0
     org-agenda-block-separator ?─
     org-agenda-time-grid
     '((daily today require-timed)
       (800 1000 1200 1400 1600 1800 2000)
       " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
     org-agenda-current-time-string
     "◀── now ─────────────────────────────────────────────────")))

(remove-hook 'text-mode-hook #'vi-tilde-fringe-mode)
(after! org
  (org-clock-persistence-insinuate)
  (add-hook! 'org-mode-hook #'+word-wrap-mode)
  (remove-hook 'org-mode-hook #'auto-fill-mode)
  (defun org-capture-clocked ()
    (interactive)
    (let ((org-capture-templates '(("c" "clocked" entry (clock) "* %?\n%i\n%a"))))
      (+org-capture/open-frame nil "c")))
  (map!
   "C-x S" 'org-save-all-org-buffers
   :map org-mode-map
   :localleader
     "TAB" #'org-insert-structure-template
     :prefix ("c" . "clock")
       :desc "Capture clocked" "x" #'org-capture-clocked)

  (map!
   :leader
   :desc "Org Agenda" "A" #'(lambda () (interactive) (org-agenda nil "n")))

  ;(setq org-capture-templates
  ;      '(("t" "Todo" entry
  ;         (file+headline org-default-notes-file "Inbox")
  ;         "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+1d\"))\n%i\n%a")))
  (setq org-todo-keywords
        '((sequence
           "TODO(t!)"
           "INPROGRESS(i!)"
           "WAIT(w@/!)"
           "HOLD(h@/!)"
           "|"
           "DONE(d)"
           "CANCELED(c)"))
        org-todo-keyword-faces
        '(("INPROGRESS" . +org-todo-active)
          ("WAIT" . +org-todo-onhold)
          ("HOLD" . +org-todo-onhold)
          ("CANCELED" . +org-todo-cancel)))

  (setq org-capture-templates
        '(("t" "Personal todo" entry
           (file+headline +org-capture-todo-file "Backlog")
           "* TODO %?\n%i\n%a" :prepend t)
          ("n" "Personal notes" entry
           (file+headline +org-capture-notes-file "Inbox")
           "* %u %?\n%i\n%a" :prepend t)

          ("p" "Templates for projects")
          ("pt" "Project-local todo" entry  ; {project-root}/todo.org
           (file+headline +org-capture-project-todo-file "Backlog")
           "* TODO %?\n%i\n%a" :prepend t)
          ("pn" "Project-local notes" entry  ; {project-root}/notes.org
           (file+headline +org-capture-project-notes-file "Inbox")
           "* %U %?\n%i\n%a" :prepend t)
          ("pc" "Project-local changelog" entry  ; {project-root}/changelog.org
           (file+headline +org-capture-project-changelog-file "Unreleased")
           "* %U %?\n%i\n%a" :prepend t)

          ("o" "Centralized templates for projects")
          ("ot" "Project todo" entry
           (function +org-capture-central-project-todo-file)
           "* TODO %?\n%i\n %a"
           :heading "Backlog"
           :prepend nil)
          ("on" "Project notes" entry
           (function +org-capture-central-project-notes-file)
           "* %U %?\n %i\n %a"
           :heading "Notes"
           :prepend t)
          ("oc" "Project changelog" entry
           (function +org-capture-central-project-changelog-file)
           "* %U %?\n %i\n %a"
           :heading "Changelog"
           :prepend t)))

  (setq org-agenda-files (append (list org-directory)
                                 (directory-files (expand-file-name "projects" org-directory) t org-agenda-file-regexp)))
  (setq org-agenda-window-setup 'reorganize-frame)
  (setq org-columns-default-format "%25ITEM %3PRIORITY %TODO %SCHEDULED %DEADLINE %TAGS")
  (setq org-fontify-done-headline t)
  (setq org-agenda-view-columns-initially nil)
  (setq org-refile-targets '((nil :maxlevel . 3)
                             (org-agenda-files :maxlevel . 3)))
  (setq org-refile-use-cache nil)
  (setq org-refile-target-verify-function
        (lambda ()
         "Filters out Archive nodes"
         (if (string= (nth 4 (org-heading-components)) "Archive")
                 (unless (ignore-errors (org-forward-element))
                   (goto-char (point-max))) t)))
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-startup-folded t)
  (setq org-cycle-open-archived-trees t)
  (setq org-adapt-indentation nil)
  (setq org-log-done 'time)
  (setq org-enforce-todo-dependencies t)
  (setq org-latex-bib-compiler "biber")
  (setq org-latex-pdf-process
        '("%latex -interaction nonstopmode -output-directory %o %f"
          "%bib %b"
          "%latex -interaction nonstopmode -output-directory %o %f"
          "%latex -interaction nonstopmode -output-directory %o %f"))
  (setq org-export-date-timestamp-format "%B %e, %Y")
  (setq org-log-into-drawer t)
  (setq org-log-done t)
  (setq org-table-duration-custom-format 'minutes)
  (setq org-clock-persist t)
  (setq org-clock-continuously nil) ;; TODO: Check shouldn't be 't
  (setq org-clock-persist-query-resume nil)
  (setq org-clock-out-when-done t)
  (setq org-clock-report-include-clocking-task t)
  (setq org-html-self-link-headlines t)
  (setq org-use-tag-inheritance nil)
  (setq org-crypt-key "brettmilford@gmail.com")
  ;; org-modern
  (setq org-startup-indented nil)
  (setq org-tags-column 0)
  (setq org-pretty-entities t))

(setq org-roam-directory org-directory)
(after! org-roam
  ;; BUG: org-roam/pull/2141
  (map!
   :leader
   :prefix ("n" . "notes")
   (:prefix ("r" . "org-roam")
    :desc "Tag file" "t" #'org-roam-tag-add ;; NOTE: 'SPC m m o t' as well.
    (:prefix ("d" . "by date")
     :desc "Goto date" "d" #'(lambda () (interactive) (org-roam-dailies-goto-date nil "d"))
     :desc "Goto tomorrow" "m" #'(lambda () (interactive) (org-roam-dailies-goto-tomorrow nil "d"))
     :desc "Goto today" "n" #'(lambda () (interactive) (org-roam-dailies-goto-today "d"))
     :desc "Goto yesterday" "y" #'(lambda () (interactive) (org-roam-dailies-goto-yesterday nil "d"))
     :desc "Capture template today" "x" #'org-roam-dailies-capture-today-w-tmpl))
   :map org-mode-map
   :localleader
   :prefix ("m" . "org-roam")
   (:prefix ("d" . "by date")
    :desc "Goto date" "d" #'(lambda () (interactive) (org-roam-dailies-goto-date nil "d"))
    :desc "Goto tomorrow" "m" #'(lambda () (interactive) (org-roam-dailies-goto-tomorrow nil "d"))
    :desc "Goto today" "n" #'(lambda () (interactive) (org-roam-dailies-goto-today "d"))
    :desc "Goto yesterday" "y" #'(lambda () (interactive) (org-roam-dailies-goto-yesterday nil "d"))
    :desc "Capture template today" "x" #'org-roam-dailies-capture-today-w-tmpl))

  ;; makes id links work, if org-mode hasn't cached them
  ;; TODO: kills startup performance.
  ;; (org-id-update-id-locations (org-roam-list-files) 't)

  (setq org-roam-mode-sections
        '((org-roam-backlinks-section :unique t)
           org-roam-reflinks-section))
  (setq org-roam-buffer-no-delete-other-windows 't)
  (setq org-roam-completion-system 'ivy)
  (setq org-roam-db-gc-threshold most-positive-fixnum)
  (setq org-roam-tag-sources '(prop all-directories))

  (setq org-roam-capture-templates
        '(("d" "default" plain
          "* ${title}\n%?"
          :target (file+head "./roam/${cxt}/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n- topics ::\n")
          :unnarrowed t)))

  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry "* %? :crypt:\n%U\n"
           :if-new (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%A the %e of %B %Y>\n#+filetags: %<:%Y:%B:>\n")
           :unnarrowed t)))

  (defun org-roam-dailies-capture-today-w-tmpl ()
    (interactive)
    (let* ((tmpl-dir (expand-file-name "./daily/tmpl" org-roam-directory))
           (files (directory-files-recursively tmpl-dir "" t))
           (org-roam-dailies-capture-templates
            (mapcar (lambda (file)
                 (let* ((f (file-name-nondirectory file))
                        (key (replace-regexp-in-string "_.*" "" f))
                        (desc (replace-regexp-in-string "\\(^.*_\\|\.org$\\)" "" f)))
                   (if (file-directory-p file)
                     `(,key ,desc)
                     `(,key ,desc entry
                       (file ,file)
                       :if-new (file+head "%<%Y-%m-%d>.org"
                                          "#+title: %<%A the %e of %B %Y>\n#+filetags: %<:%Y:%B:>\n\n")))))
               files)))
      (org-roam-dailies-capture-today)))
)

(after! org-roam-graph
  (if IS-MAC
   (setq org-roam-graph-viewer "open")
   (setq org-roam-graph-viewer "xdg-open")))

(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam
    :config
    (map!
     :leader
      (:prefix ("n" . "notes")
        (:prefix ("r" . "roam")
          :desc "Org Roam UI" "u" (lambda () (interactive)
                                    (if (member '(org-roam-ui-mode " org-roam-ui") minor-mode-alist)
                                                 (org-roam-ui-open)
                                                 (org-roam-ui-mode))))))

    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

;; TODO: revisit: only being used for keyword formatting
;(use-package! org-roam-bibtex
;  :after org-roam
;  ;:hook (org-roam-mode . org-roam-bibtex-mode)
;  :config
;  ;(require 'org-ref)
;  (setq orb-preformat-keywords
;   '("citekey" "title" "url" "author-or-editor" "keywords" "file"))
;  (setq orb-process-file-field 't)
;  (setq orb-insert-link-description 'citation-org-ref-2)
;;  (add-to-list 'org-roam-capture-templates
;;        '("r" "bibliography reference" plain "%?
;;"
;;           :target (file+head "annotations/${citekey}.org"
;;                              "#+TITLE: ${title}
;;#+FILETAGS: ${keywords}
;; topics ::
;;
;;* ${title}
;;:PROPERTIES:
;;:Custom_ID: ${citekey}
;;:URL: ${url}
;;:AUTHOR: ${author-or-editor}
;;:NOTER_DOCUMENT: ${file}
;;:NOTER_PAGE:
;;:END:")
;;           :unnarrowed t))
;)

(setq reftex-default-bibliography (expand-file-name "references.bib" org-directory))

(after! bibtex-completion
  ;(advice-add 'bibtex-completion-candidates
  ;            :filter-return 'reverse)
  (setq bibtex-completion-notes-path (file-name-as-directory (expand-file-name "annotations" org-directory))
        bibtex-completion-library-path (file-name-as-directory (expand-file-name "fulltext" org-directory))
        bibtex-completion-bibliography reftex-default-bibliography)
  (setq bibtex-completion-notes-template-multiple-files
        (concat
         ":PROPERTIES:\n"
         ":ROAM_ALIASES: ${=key=}\n"
         ":ROAM_REFS: @${=key=}\n"
         ":END:\n"
         "#+TITLE: ${title}\n"
         "#+FILETAGS: ${keywords}\n\n"
         "- keywords :: \n"
         "* ${title}\n"
         ":PROPERTIES:\n"
         ":Custom_ID: ${=key=}\n"
         ":URL: ${url}\n"
         ":AUTHOR: ${author-abbrev}\n"
         ":NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n"
         ":NOTER_PAGE:"
         ":JOURNAL: ${journaltitle}\n"
         ":DATE: ${date}\n"
         ":YEAR: ${year}\n"
         ":DOI: ${doi}\n"
         ":END:\n\n"))
  (add-hook 'bibtex-completion-notes-mode-hook #'org-id-get-create))

(use-package! org-ref
  :after bibtex-completion)

(after! bibtex
  (setq bibtex-autokey-year-length 4
        bibtex-autokey-name-year-separator ""
        bibtex-autokey-year-title-separator ""
        bibtex-autokey-titleword-separator ""
        bibtex-autokey-titlewords 1
        bibtex-autokey-titlewords-stretch 1
        bibtex-autokey-titleword-length nil))

(after! org-noter
  (setq org-noter-notes-search-path bibtex-completion-notes-path))

(use-package! ox-reveal
  :after org-mode)

(use-package! nov
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(after! anki-editor
       (setq anki-editor-create-decks t))

(after! org-mode-incremental-reading
  (add-hook! 'incremental-reading-mode-hook #'anki-editor-mode)
  (add-to-list
   'org-protocol-protocol-alist
   '("org-open-file" :protocol "open-file" :function org-protocol-open-file))
  (defun org-protocol-open-file (fname)
  "Process an org-protocol://open-file?url= style URL with FNAME.
  Change a filename by mapping URLs to local filenames as set
  in `org-protocol-project-alist'.
  The location for a browser's bookmark should look like this:
  javascript:location.href = \\='org-protocol://open-file?url=\\=' + \\
        encodeURIComponent(location.href)"
    (let ((f (org-protocol-sanitize-uri
              (plist-get (org-protocol-parse-parameters fname nil '(:file))
                         :file))))
      f)))

(use-package jiralib2)

(defun my/org-links ()
  (pushnew! org-link-abbrev-alist '("jira" .  "https://jira/browse/%s"))
  (org-link-set-parameters "gh"
                           :follow (lambda (path)
                                     (let* ((org (car (split-string path "/")))
                                            (repo (cadr (split-string path "/")))
                                            (issue (caddr (split-string path "/"))))
                                       (browse-url (format "https://git/%s/%s/issues/%s" org repo issue))))
                           :export (lambda (path desc backend)
                                     (let* ((org (car (split-string path "/")))
                                            (repo (cadr (split-string path "/")))
                                            (issue (caddr (split-string path "/"))))
                                       (cond
                                        ((eq backend 'html)
                                         (format "<a href='https://git/%s/%s/issues/%s'>%s</a>" org repo issue desc))
                                        ((eq backend 'latex)
                                         (format "\\href{https://git/%s/%s/issues/%s}{%s}" org repo issues desc)))))))

(defun +pkm/org-capture-templates ()
  (defun jira-capture-enrichment ()
    (when-let* ((pt (point))
                (issue-key (and (org-at-heading-p)
                                (org-entry-get pt "JIRAISSUEKEY"))))
      (let-alist (jiralib2-get-issue issue-key)
        ;; Update headline
        (let ((headline (format "[[jira:%s][%s]] %s" .key .key .fields.summary)))
          (message "Updating headline to : %s" headline)
          (org-edit-headline headline))
        ;; Update properties
        (message "Updating Property Drawer")
        (cl-loop
         for (property value)
         on (list
             "JiraAssignee" .fields.assignee.displayName
             "JiraCreate" .fields.created
             "JiraIssueKey" .key
             "JiraIssueType" .fields.issuetype.name
             "JiraPriority" .fields.priority.name
             "JiraProjectKey" .fields.project.key
             "JiraReporter" .fields.reporter.displayName
             "JiraStatus" .fields.status.name
             "JiraSummary" .fields.summary)
         by #'cddr
         do (org-entry-put pt property value))
        (message "Inserting org-roam-node")
        (org-end-of-subtree)
        (insert "\n")
        (let* ((node (org-roam-node-create :title .key))
               (id (org-roam-node-id node)))
          (org-roam-capture-
           :node node
           :keys "d"
           :templates '(("d" "default" plain "* {title} Notes"
                         :target (file+head "./jira/${slug}.org"
                                            "#+title: [[jira:${title}][${title]]\n")))
           :props (append
                   (list :link-description (format "%s Notes" .key)
                         :jump-to-captured nil
                         :immediate-finish t
                         :finalize 'insert-link)))))))
  (add-to-list 'org-capture-templates
               '("i" "Jira Issue" entry
                 (file+headline +org-capture-todo-file "Backlog")
                 "* TODO %^{JiraIssueKey}p\n%i\n%a"
                 :immediate-finish t
                 :jump-to-captured t
                 :empty-lines-after 1
                 :hook jira-capture-enrichment
                 :prependt)))

(load! "lisp/org-notification")
) ;; closes (when (file-directory-p "~/org")
