;;; ktz-init-sci.el --- Configurations for scientific stuff
;;
;;; Commentary:
;;   - see also roam:research.topics
;;   - depends on ktz-init-org.el
;;
;;; Code:

(defun ktz--init-sci ()

  ;;;; Bibliography
  ;; ----------------------------------------

  (defvar ktz--cite-pdfs
    (list (concat ktz--org-files-ref "pdfs/")))

  (defvar ktz--cite-notes
    (list (concat ktz--org-files-ref "notes/")))

  (defvar ktz--cite-bibfiles
    ;; the first element is used for automatic appends by biblio
    (list
     (concat ktz--org-files-ref "bibliography.bib")
     (concat ktz--org-files-ref "bibliography-retained.bib")))

  (use-package bibtex
    :config
    ;; format whole bibliography uniformly
    (defun ktz-reformat-bib ()
      (interactive)
      (when (eq major-mode 'bibtex-mode)
        (bibtex-map-entries
         (lambda (key start end)
           (goto-char start)
           (ignore-errors
             (ktz-log "org" (format "cleaning %s" key))
             (org-ref-clean-bibtex-entry)))))))

  ;; Citar provides a highly-configurable completing-read front-end to
  ;; browse and act on BibTeX, BibLaTeX, and CSL JSON bibliographic
  ;; data, and LaTeX, markdown, and org-cite editing support.
  (use-package citar
    :hook
    ;; completion at point
    (LaTeX-mode . citar-capf-setup)
    (org-mode . citar-capf-setup)

    :custom
    (citar-bibliography ktz--cite-bibfiles)
    (citar-library-paths ktz--cite-pdfs)
    (citar-file-extensions '("pdf" "org" "md"))
    (citar-notes-paths ktz--cite-notes)
    ;; also using this place to configure org-cite
    (org-cite-global-bibliography ktz--cite-bibfiles)

    :bind
    (:map org-mode-map :package org ("C-c b" . #'citar-insert-citation)))

  ;; org-ref makes it easy to insert citations, cross-references,
  ;; indexes and glossaries as hyper-functional links into org files.
  (use-package org-ref
    :after org bibtex citar

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
	   bibtex-autokey-titleword-length 10))

  ;; biblio.el makes it easy to browse and gather bibliographic
  ;; references and publications from various sources, by keywords or
  ;; by DOI. References are automatically fetched from well-curated
  ;; sources, and formatted as BibTeX.
  (use-package biblio
    ;; org-ref integrates biblio to browse and retrieve bibtex entries
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

  ;; allows to search Google Scholar with biblio
  (straight-use-package
   '(biblio-gscholar.el
     :type git :host github :repo "seanfarley/biblio-gscholar.el"))

  ;;;; LaTeX and PDFs
  ;; ----------------------------------------

  (unless (eq system-type 'windows-nt)
    (use-package pdf-tools
      :init
      ;; Install pdf-tools on first use
      (pdf-tools-install)

      :config
      (global-auto-revert-mode t)
      (setq pdf-view-display-size 'fit-page)

      :hook
      (pdf-view-mode . auto-revert)))

  (use-package citar-embark
    :after citar embark
    :config (citar-embark-mode))

  ;; Out-of-box, Citar provides default support for file-per-note
  ;; bibliographic notes that are compatible with Org-Roam v2.
  (use-package citar-org-roam
    :after citar
    :no-require
    :config (citar-org-roam-mode))

  ;; Set a desired text body width to automatically resize window
  ;; margins to keep the text comfortably in the middle of the window.
  (use-package olivetti
    :hook (LaTeX-mode . olivetti-mode))

  ;; Flyspell mode is a minor mode that performs automatic
  ;; spell-checking of the text you type as you type it. When it finds
  ;; a word that it does not recognize, it highlights that word. You
  ;; can use the ispell-change-dictionary command if you want to
  ;; spell-check text in a different language
  (use-package flyspell
    :init
    (setq ispell-dictionary "en_GB")
    :hook (LaTeX-mode . flyspell-mode))

  ;;;; Standby

  ;; (use-package gscholar-bibtex
  ;;   :init
  ;;   (setq gscholar-bibtex-database-file (car ktz--cite-bibfiles)))
  ;; (use-package gscholar-bibtex
  ;;   :init
  ;;   (setq gscholar-bibtex-database-file
  ;;         (car ktz--cite-bibfiles)))

  ;; overleaf
  ;; (use-package git-auto-commit-mode)

  ;; automatically clean up the library file
  ;;(add-hook 'after-save-hook 'ktz--cite-reformat-bib))

  ) ;; end ktz--init-sci


(defun ktz-init-sci ()
  (interactive)
  (ktz--init-sci))

(provide 'ktz-init-sci)
;;; ktz-init-sci.el ends here
