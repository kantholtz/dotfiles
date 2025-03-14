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
	   bibtex-autokey-titleword-separator ""))

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

  (use-package gscholar-bibtex
    :init
    (setq gscholar-bibtex-database-file
          (car ktz--cite-bibfiles)))

  ;; automatically clean up the library file
  ;;(add-hook 'after-save-hook 'ktz--cite-reformat-bib))
  ) ;; end ktz--init-sci

(defun ktz-init-sci ()
  (interactive)
  (ktz--init-sci))

(provide 'ktz-init-sci)
