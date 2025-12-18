(defun ktz--init-org-base ()

  ;; presentation
  ;;   1. visual-line-mode
  ;;      This wraps text at the window boundaries.
  ;;   2. visual-fill-column
  ;;      Extends window margins to make the space for
  ;;      text smaller. Can also center text.

  (use-package visual-fill-column)
  (use-package org-bullets)

  (use-package org
    :straight (:type built-in)
    :init
    (customize-set-variable 'visual-fill-column-width 100)

    :config
    (setq org-startup-folded 'show2levels
          org-hide-emphasis-markers t

          ;; refile configuration
          ;; see https://orgmode.org/manual/Refile-and-Copy.html
          org-refile-targets
          '((nil :maxlevel . 3)
            (org-agenda-files :maxlevel . 3)))

    (defun ktz--org-visual-hook ()
      (org-indent-mode)
      (visual-line-mode)
      (visual-fill-column-mode)

      (when (not (eq system-type 'windows-nt))
        (org-bullets-mode)))

    :hook
    (org-mode . ktz--org-visual-hook))

;;; end ktz--init-org-base
  )


(defun ktz--init-org-roam ()

  ;; top-level directories
  (defvar ktz--org-files-org    (f-join ktz-org-dir "org"))
  (defvar ktz--org-files-ref    (f-join ktz-org-dir "ref"))
  (defvar ktz--org-templates    (f-join ktz-org-dir "templates"))

  ;;
  ;; roam
  ;;
  ;; see also https://github.com/org-roam/org-roam#configuration
  (use-package org-roam
    :custom

    (org-log-done 'time)
    (org-roam-directory ktz--org-files-org)
    (org-roam-completion-everywhere t)
    (org-link-file-path-type 'relative) ;; all file links should be relative

    ;; `(1 ,(- 3 1)) -> (1 2) (the (concat ...) needs to be evaluated)
    ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Backquote.html
    ;; https://www.reddit.com/r/emacs/comments/quy7gd/setting_an_org_roam_capture_template/
    (org-roam-capture-templates
     `(;; roam templates
       ("r" "roam")
       ("rb" "blank" plain
        (file ,(concat ktz--org-templates "/roam-blank.org"))
        :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "")
        :unnarrowed t)
       ("rd" "default" plain
        (file ,(concat ktz--org-templates "/roam-default.org"))
        :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "")
        :unnarrowed t)
       ("rr" "research" plain
        (file ,(concat ktz--org-templates "/roam-research.org"))
        :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "")
        :unnarrowed t)
       ))

    :init
    (setq org-roam-v2-ack t
          ;; do not destroy current splits
          org-agenda-window-setup 'current-window
          ;; org-agenda-start-with-log-mode t
          org-agenda-files (list ktz--org-files-org))

    ;; (unless (vesion< emacs-version "29")
    ;;   (use-package emacsql-sqlite-builtin)
    ;;   (setq org-roam-database-connector 'sqlite-builtin))


    :bind (("C-c n l" . org-roam-buffer-toggle)
           ("C-c n f" . org-roam-node-find)
           ("C-c n i" . org-roam-node-insert)
           ("<f4>" . org-roam-node-find)
           ("C-<f4>" . org-roam-node-find)))

  ;;
  ;; agenda configuration
  ;;

  (use-package org-super-agenda
    :config
    ;; see https://github.com/alphapapa/org-super-agenda#examples
    (let ((org-super-agenda-groups
           '(;; Each group has an implicit boolean OR operator between its selectors.
             (:name "Today"  ; Optionally specify section name
                    :time-grid t  ; Items that appear on the time grid
                    :todo "NEXT")  ; Items that have this TODO keyword
             (:name "Important"
                    ;; Single arguments given alone
                    :priority "A")
             )))
      ;; show agenda on startup
      ;; (when (and ktz-org-dir (equal (length command-line-args) 1))
      ;;   (org-agenda nil "a")))
      )

    :bind (
           ("<f3>" . org-agenda)
           ("C-<f3>" . org-agenda)))

  (use-package consult-notes
    :straight (:type git :host github :repo "mclear-tools/consult-notes")
    :commands (consult-notes
               consult-notes-search-in-all-notes
               ;; if using org-roam
               consult-notes-org-roam-find-node
               consult-notes-org-roam-find-node-relation)
    :config
    (setq consult-notes-file-dir-sources
          `(("org"  ?o  ,ktz--org-files-org))) ;; Set notes dir(s), see below

    ;; Set org-roam integration, denote integration, or org-heading integration e.g.:
    ;; (setq consult-notes-org-headings-files '("~/path/to/file1.org"
    ;;                                          "~/path/to/file2.org"))
    ;; (consult-notes-org-headings-mode)
    )


;;; end of ktz--init-org
  )

(defun ktz--init-org()
  (ktz--init-org-base)
  (ktz--init-org-roam))


(defun ktz-init-org ()
  "Initialize org related config manually"
  (interactive)
  (ktz--init-org))


(provide 'ktz-init-org)
