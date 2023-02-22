(defun ktz--init-org-base ()

  ;; do not destroy current splits
  (setq org-agenda-window-setup 'current-window)

  ;; refile configuration
  ;; see https://orgmode.org/manual/Refile-and-Copy.html
  (setq org-refile-targets
        '((nil :maxlevel . 3)
          (org-agenda-files :maxlevel . 3)))

  ;; hooks
  (add-hook 'org-mode-hook 'org-indent-mode)
  (add-hook 'org-mode-hook 'visual-line-mode)

  t)


(defun ktz--init-org-roam ()

  ;; top-level directories
  (defvar ktz--org-files-org    (concat ktz-org-dir "/org/"))
  (defvar ktz--org-files-ref    (concat ktz-org-dir "/ref/"))
  (defvar ktz--org-templates    (concat ktz-org-dir "/templates/"))

  ;;
  ;; roam
  ;;
  (use-package visual-fill-column
    :config
    (defun ktz--org-visual-fill ()
      (visual-fill-column-mode 1))
    :hook (org-mode . ktz--org-visual-fill))

  ;; see also https://github.com/org-roam/org-roam#configuration
  (use-package org-roam
    :custom
    (org-log-done 'time)
    (org-roam-directory ktz--org-files-org)
    (org-agenda-files (list ktz--org-files-org))
    (org-agenda-start-with-log-mode t)
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
       ;; mailing
       ;; TODO load from ktz-mu4e.el
       ("m" "mail")
       ("mp" "paper heap" entry
        (file+olp ,(concat ktz--org-files-org "20211003154535-diss_paper_heap.org") "Scholar")
        (file ,(concat ktz--org-templates "/mail-paper_heap.org")))
       ("mq" "mail queries" entry
        (file+olp ,(concat ktz--org-files-org "20220307104213-mail.org") "Scholar")
        "TODO %:date ")
       ))

    :init
    (setq org-roam-v2-ack t)

    :bind (("C-c n l" . org-roam-buffer-toggle)
           ("C-c n f" . org-roam-node-find)
           ("C-c n i" . org-roam-node-insert))

    :config
    (org-roam-setup))

  ;;
  ;; scientific stuff
  ;;   see also roam:research.topics

  (defvar ktz--cite-pdfs
    (list (concat ktz--org-files-ref "pdfs/")))

  (defvar ktz--cite-notes
    (list (concat ktz--org-files-ref "notes/")))

  (defvar ktz--cite-bibfiles
    (list ;; the first element is used for automatic appends by org-ref/gscholar
     (concat ktz--org-files-ref "bibliography.bib")
     (concat ktz--org-files-ref "ramlimit.bib")))

  ;; citar
  ;;   frontend to access bibliography

  (use-package citar
    :after org-roam
    :bind
    (("C-c b" . citar-insert-citation)
     :map minibuffer-local-map
     ("M-b" . citar-insert-preset))

    :config
    (require 'citar-org)

    :custom
    (citar-bibliography ktz--cite-bibfiles)
    (citar-library-paths ktz--cite-pdfs)
    (citar-file-extensions '("pdf" "org" "md"))
    (citar-notes-paths ktz--cite-notes)
    ;; also using this place to configure org-cite
    (org-cite-global-bibliography ktz--cite-bibfiles))

  (use-package citar-embark
    :after citar embark
    :config (citar-embark-mode))

  (use-package citar-org-roam
    :after citar
    :no-require
    :config (citar-org-roam-mode))

  (use-package bibtex
    :config
    ;; format whole bibliography uniformly
    (defun ktz-reformat-bib ()
      (when (eq major-mode 'bibtex-mode)
        (bibtex-map-entries
         (lambda (key start end)
           (goto-char start)
           (ignore-errors
             (ktz-log "org" (format "cleaning %s" key))
             (org-ref-clean-bibtex-entry)))))))

    ;; automatically clean up the library file
    ;;(add-hook 'after-save-hook 'ktz--cite-reformat-bib))

  (use-package org-ref
    :after org bibtex

    :init
    (setq
     ;; org-cite compatibility
     org-ref-insert-cite-function  (lambda () (org-cite-insert nil))
     ;; completion
     bibtex-completion-bibliography  ktz--cite-bibfiles
     bibtex-completion-library-path  (list ktz--cite-pdfs)
     bibtex-completion-pdf-open-function  (lambda (fpath) (call-process "open" nil 0 nil fpath))
     ;; autokey
     bibtex-autokey-year-length 4
     bibtex-autokey-name-year-separator ""
	   bibtex-autokey-year-title-separator ""
	   bibtex-autokey-titleword-separator ""
	   bibtex-autokey-titlewords 1
	   bibtex-autokey-titlewords-stretch 1
	   bibtex-autokey-titleword-length 10)

    (require 'org-ref-arxiv))

  ;; org-ref integrates biblio to browse and retrieve bibtex entries
  (use-package biblio
    :config
    (defun ktz--biblio-ref-add (bibtex entry)
      "Add entry to bibtex file."
      (with-current-buffer (find-file-noselect (car ktz--cite-bibfiles))
        (goto-char (point-max))
        (insert (concat "\n\n" bibtex))
        (org-ref-clean-bibtex-entry)))

    (defun ktz--biblio-ref-select-and-add ()
      "Append current entry to bibtex file."
      (interactive)
      (biblio--selection-forward-bibtex #'ktz--biblio-ref-add))

    :bind
    (:map biblio-selection-mode-map
          ("A" . ktz--biblio-ref-select-and-add)))

  (use-package gscholar-bibtex
    :init
    (setq gscholar-bibtex-database-file (car ktz--cite-bibfiles)))

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
      (when (and ktz-org-dir (equal (length command-line-args) 1))
        (org-agenda nil "a"))))

  ;; misc
  (use-package org-bullets))

(defun ktz--init-org ()
  "Setup org related configuration"
  (when (or ktz-org-enable-headless (display-graphic-p))
    (ktz-log "org" "initializing configuration")
    (ktz--init-org-base)
    (when ktz-org-dir
      (ktz--init-org-roam)))

  t)

(defun ktz-org ()
  "Initialize org related config manually"
  (interactive)
  (ktz--init-org))


(provide 'ktz-init-org)
