;;; +pkm.el --- Personal Knowledge Management config -*- lexical-binding: t; -*-
(setq org-directory "~/org")
(add-to-list 'auto-mode-alist '("\\.org_archive\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.org.gpg\\'"     . org-mode))

(after! org
  (map! "C-x S" 'org-save-all-org-buffers) ;; NOTE: 'SPC h .' does the same
  (map!
   :map org-mode-map
   :localleader
        "TAB" #'org-insert-structure-template)
  (add-hook! 'org-mode-hook #'+word-wrap-mode)
  (remove-hook 'org-mode-hook #'auto-fill-mode)
  (setq org-agenda-files (list org-directory))
  (setq org-agenda-window-setup 'reorganize-frame)
  (setq org-default-notes-file (expand-file-name "01-inbox.org" org-directory))
  (setq org-columns-default-format "%25ITEM %3PRIORITY %TODO %SCHEDULED %DEADLINE %TAGS")
  (setq org-fontify-done-headline 't)
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
  (setq org-startup-folded 't)
  (setq org-tags-column -77)
  (setq org-cycle-open-archived-trees 't)
  (setq org-adapt-indentation nil)
  (setq org-startup-indented 't)
  (setq org-pretty-entities 't)
  (setq org-log-done 'time)
  (setq org-enforce-todo-dependencies 't)
  (setq org-latex-bib-compiler "biber")
  (setq org-latex-pdf-process
        '("%latex -interaction nonstopmode -output-directory %o %f"
          "%bib %b"
          "%latex -interaction nonstopmode -output-directory %o %f"
          "%latex -interaction nonstopmode -output-directory %o %f"))
  (setq org-export-date-timestamp-format "%B %e, %Y")
  (setq org-todo-keywords
  '((sequence "TODO(!)" "INPROGRESS(!)" "WAIT(w@/!)" "|" "DONE" "CANCELLED")))
  ;(setq org-log-into-drawer 't) ;;TODO: Check if this breaks clock logging
  (setq org-log-done 't)
  (setq org-todo-keywords-for-agenda org-todo-keywords)
  (setq org-table-duration-custom-format 'minutes)
  (setq org-clock-persist 't)
  (org-clock-persistence-insinuate)
  (setq org-clock-continuously nil) ;; TODO: Check shouldn't be 't
  (setq org-clock-persist-query-resume nil)
  (setq org-clock-out-when-done 't)
  (setq org-clock-report-include-clocking-task 't)
  (setq org-html-self-link-headlines 't)
  (setq org-use-tag-inheritance nil)
  (setq org-crypt-key "brettmilford@gmail.com"))

(after! deft
  (setq deft-directory org-directory)
  (setq deft-recursive t)
  (setq deft-default-extension "org")
  (setq deft-use-filter-string-for-filename t))

(setq org-roam-directory org-directory)
(after! org-roam
  ;; makes id links work, if org-mode hasn't cached them
  (org-id-update-id-locations (org-roam-list-files) 't)
  (setq org-roam-mode-sections
        '((org-roam-backlinks-section :unique -t)
           org-roam-reflinks-section))
  (setq org-roam-db-location "~/.emacs.d/.local/cache/org-roam.db")
  (setq org-roam-buffer-no-delete-other-windows 't)
  (setq org-roam-completion-system 'ivy)
  (setq org-roam-db-gc-threshold most-positive-fixnum)
  (setq org-roam-tag-sources '(prop all-directories))
  (setq org-roam-dailies-capture-templates
        '(("d" "Daily" entry "* %? \t:crypt:\n%U\n"
           :if-new (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%A the %e of %B %Y>\n#+filetags: %<:%Y:%B:>\n"))))
  (setq org-roam-capture-templates
        '(("d" "default" plain
          "%?"
          :target (file+head "./roam/${cxt}/%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n#+FILETAGS:\n\n- topics ::")
          :unnarrowed t))))

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

(setq reftex-default-bibliography (concat org-directory "/references.bib"))

(after! bibtex-completion
  ;(advice-add 'bibtex-completion-candidates
  ;            :filter-return 'reverse)
  (setq bibtex-completion-notes-path (concat org-directory "/annotations")
        bibtex-completion-library-path (concat org-directory "/fulltext")
        bibtex-completion-bibliography (concat org-directory "/references.bib"))
  (setq bibtex-completion-notes-template-multiple-files
        (concat
         ":PROPERTIES:\n"
         ":ROAM_ALIASES: ${=key=}\n"
         ":ROAM_REFS: @${=key=}\n"
         ":END:\n"
         "#+TITLE: ${title}\n"
         "#+FILETAGS: ${keywords}\n\n"
         "- topics :: \n"
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
  (setq org-noter-notes-search-path (concat org-directory "/annotations")))

(use-package! ox-reveal
  :after org-mode)

;(use-package! nov
;  :init
;  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

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
