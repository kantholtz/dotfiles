;;
;; scientific stuff
;;   see also roam:research.topics
;;   depends on ktz-init-org.el

(defun ktz--init-sci ()
  ;; overleaf
  (use-package git-auto-commit-mode)

  (defvar ktz--cite-pdfs
    (list (concat ktz--org-files-ref "pdfs/")))

  (defvar ktz--cite-notes
    (list (concat ktz--org-files-ref "notes/")))

  (defvar ktz--cite-bibfiles
    ;; the first element is used for automatic appends by org-ref/gscholar
    (list
     (concat ktz--org-files-ref "bibliography.bib")
     (concat ktz--org-files-ref "bibliography-retained.bib")))

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
        (org-ref-clean-bibtex-entry)
        (ktz-reformat-bib)))

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


  ;; tex and pdf

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


  (use-package olivetti
    :hook
    (LaTeX-mode . olivetti-mode))

  (use-package tex
    :defer t
    :straight auctex
    :mode ("\\.tex\\'" . LaTeX-mode)

    :hook
    (LaTeX-mode . visual-line-mode)
    (LaTeX-mode . flyspell-mode)
    (LaTeX-mode . reftex-mode))

  ;; citar
  ;;   frontend to access bibliography

  (use-package citar
    :after (org-roam auctex)
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
      (interactive)
      (when (eq major-mode 'bibtex-mode)
        (bibtex-map-entries
         (lambda (key start end)
           (goto-char start)
           (ignore-errors
             (ktz-log "org" (format "cleaning %s" key))
             (org-ref-clean-bibtex-entry)))))))

  ;; automatically clean up the library file
  ;;(add-hook 'after-save-hook 'ktz--cite-reformat-bib))
  ) ;; end ktz--init-sci

(defun ktz-init-sci ()
  (interactive)
  (ktz--init-sci))

(provide 'ktz-init-sci)
