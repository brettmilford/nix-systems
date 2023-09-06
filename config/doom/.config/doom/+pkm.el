;;; +pkm.el --- Personal Knowledge Management config -*- lexical-binding: t; -*-
(setq org-directory "~/org")
(add-to-list 'auto-mode-alist '("\\.org_archive\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.org.gpg\\'"     . org-mode))

(after! org
  (org-clock-persistence-insinuate)
  (add-hook! 'org-mode-hook #'+word-wrap-mode)
  (remove-hook 'org-mode-hook #'auto-fill-mode)
  (pushnew! org-link-abbrev-alist
            '("case" . "https://jira/browse/"))
  (defun org-capture-clocked ()
    (interactive)
    (let ((org-capture-templates '(("c" "clocked" entry (clock) "* %?\n%i\n%a"))))
      (+org-capture/open-frame nil "c")))
  (map!
   "C-x S" 'org-save-all-org-buffers ;; NOTE: 'SPC h .' does the same
   :map org-mode-map
   :localleader
     "TAB" #'org-insert-structure-template
     :prefix ("c" . "+clock")
       :desc "Capture clocked" "x" #'org-capture-clocked)

  (setq org-capture-templates
        '(("t" "Todo" entry
           (file+headline org-default-notes-file "Inbox")
           "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+1d\"))\n%i\n%a")))
  (setq org-agenda-files (list org-directory))
  (setq org-agenda-window-setup 'reorganize-frame)
  (setq org-columns-default-format "%25ITEM %3PRIORITY %TODO %SCHEDULED %DEADLINE %TAGS")
  (setq org-fontify-done-headline t)
  (setq org-agenda-view-columns-initially nil)
  (setq org-agenda-custom-commands
        '(("d" "Todo and Due" ((org-ql-block '(or (and (todo) (scheduled :to 0))
                                              (and (todo) (priority "A")))
                                         ((org-ql-block-header "Todo and Due")))
                                         (agenda)))))
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
  (setq org-tags-column -77)
  (setq org-cycle-open-archived-trees t)
  (setq org-adapt-indentation nil)
  (setq org-startup-indented t)
  (setq org-pretty-entities t)
  (setq org-log-done 'time)
  (setq org-enforce-todo-dependencies t)
  (setq org-latex-bib-compiler "biber")
  (setq org-latex-pdf-process
        '("%latex -interaction nonstopmode -output-directory %o %f"
          "%bib %b"
          "%latex -interaction nonstopmode -output-directory %o %f"
          "%latex -interaction nonstopmode -output-directory %o %f"))
  (setq org-export-date-timestamp-format "%B %e, %Y")
  (setq org-log-into-drawer nil) ;; NOTE: beorg compat
  (setq org-log-done t)
  (setq org-todo-keywords-for-agenda org-todo-keywords)
  (setq org-table-duration-custom-format 'minutes)
  (setq org-clock-persist t)
  (setq org-clock-continuously nil) ;; TODO: Check shouldn't be 't
  (setq org-clock-persist-query-resume nil)
  (setq org-clock-out-when-done t)
  (setq org-clock-report-include-clocking-task t)
  (setq org-html-self-link-headlines t)
  (setq org-use-tag-inheritance nil)
  (setq org-crypt-key "brettmilford@gmail.com"))

(setq org-roam-directory org-directory)
(after! org-roam
  ;; BUG: org-roam/pull/2141
  (map!
   :leader
   :prefix ("n" . "notes")
   (:prefix ("r" . "org-roam")
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
  (org-id-update-id-locations (org-roam-list-files) 't)
  (setq org-roam-mode-sections
        '((org-roam-backlinks-section :unique t)
           org-roam-reflinks-section))
  (setq org-roam-db-location "~/.emacs.d/.local/cache/org-roam.db")
  (setq org-roam-buffer-no-delete-other-windows 't)
  (setq org-roam-completion-system 'ivy)
  (setq org-roam-db-gc-threshold most-positive-fixnum)
  (setq org-roam-tag-sources '(prop all-directories))
  (setq org-roam-capture-templates
        '(("d" "default" plain
          "%?"
          :target (file+head "./roam/${cxt}/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags:\n\n- topics ::")
          :unnarrowed t)))  (setq org-roam-dailies-capture-templates

        '(("d" "default" entry "* %? \t:crypt:\n%U\n"
           :if-new (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%A the %e of %B %Y>\n#+filetags: %<:%Y:%B:>\n"))))

  (defun org-roam-dailies-capture-today-w-tmpl ()
    (interactive)
    (let* ((tmpl-dir "./daily/tmpl")
           (files (directory-files (expand-file-name tmpl-dir org-roam-directory) nil "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)"))
           (key-desc (mapcar (lambda (file)
                               (let ((key (replace-regexp-in-string "_.*" "" file))
                                     (desc (replace-regexp-in-string "\\(^.*_\\|\.org$\\)" "" file)))
                                 (cons key desc)))
                             files))
           (org-roam-dailies-capture-templates (mapcar (lambda (a)
                                                         (let ((key (car a))
                                                               (desc (cdr a)))
                                                           (key desc entry
                                                             (file (format "%s/%s_%s.org" tmpl-dir key desc))
                                                             :if-new (file+head "%<%Y-%m-%d>.org"
                                                                                "#+title: %<%A the %e of %B %Y>\n#+filetags: %<:%Y:%B:>\n"))))
                                                       key-desc))
           )
      (message "%s" key-desc)
      ;(org-roam-dailies-capture-today)
      ))
)

(after! (org org-roam)

 (defun org-roam-capture-case-notes (title)
   (let ((id (org-id-new)))
    (org-roam-capture- :node (org-roam-node-create :title title :id id)
                       :templates '(("c" "case" plain "* ${title} %?"
                                    :if-new (file+head "./roam/case/${slug}.org"
                                                       ":PROPERTIES:\n:ROAM_ALIASES: case://${title}\n:END:\n#+title: ${title}\n")
                                    ))
                       :props '(:immediate-finish t))
    id))

 (defun org-capture-case-notes (title)
   (let* ((id (org-roam-capture-case-notes title))
          (link (org-link-make-string (concat "id:" id) title))
          (tmpl (format "* TODO [[case://%s]] %%?\n%%U\n%s\n" title link)))
     tmpl))

 (defun org-capture-case-notes-prompt ()
   (interactive)
   (org-capture-case-notes (read-from-minibuffer "title: ")))

 (add-to-list 'org-capture-templates
              '("w" "case" entry
                (file+headline org-default-notes-file "Inbox")
                (function org-capture-case-notes-prompt))))

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

(load! "lisp/org-notification")
