(defun ktz--init-org-base ()

  ;; do not destroy current splits
  (setq org-agenda-window-setup 'current-window)

  ;; hooks
  (add-hook 'org-mode-hook 'org-indent-mode)
  (add-hook 'org-mode-hook 'visual-line-mode)

  t)


(defun ktz--org-visual-fill ()
  (visual-fill-column-mode 1))


(defun ktz--init-org-roam ()

  ;; was not able to use (let ...) - need to understand scoping better
  ;; trailing slashes are required at least for ref-pdfs
  (defvar ktz--org-orgfiles      (concat ktz-org-dir "/org/"))
  (defvar ktz--org-templates     (concat ktz-org-dir "/templates/"))
  (defvar ktz--org-ref-pdfs      (concat ktz-org-dir "/ref/pdfs/"))
  (defvar ktz--org-ref-bibfiles
    (list ;; the first element is used for automatic appends by org-ref/gscholar
     (concat ktz-org-dir "/ref/bibliography.bib")
     (concat ktz-org-dir "/ref/ramlimit.bib")))

  ;;
  ;; roam
  ;;
  (use-package visual-fill-column
    :hook (org-mode . ktz--org-visual-fill))

  ;; see also https://github.com/org-roam/org-roam#configuration
  (use-package org-roam
    :custom
    (org-log-done 'time)
    (org-roam-directory ktz--org-orgfiles)
    (org-agenda-files (list ktz--org-orgfiles))
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
        (file+olp ,(concat ktz--org-orgfiles "20211003154535-diss_paper_heap.org") "Scholar")
        (file ,(concat ktz--org-templates "/mail-paper_heap.org")))
       ("mq" "mail queries" entry
        (file+olp ,(concat ktz--org-orgfiles "20220307104213-mail.org") "Scholar")
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
  ;; scientific org stuff
  ;;

  ;; in org-ref/melpa/init-helm.el there is some configuration
  ;; which is required as otherwise the doi async pdf download
  ;; thing fails for whatever reason...
  (setq package-user-dir "~/.emacs.d/package-user-dir")

  (use-package bibtex)
  (use-package helm-bibtex
    :init
    (setq
     ;; org-ref optiona
     org-ref-insert-link-function 'org-ref-insert-link-hydra/body
     org-ref-insert-cite-function 'org-ref-cite-insert-helm
     org-ref-insert-label-function 'org-ref-insert-label-link
     org-ref-insert-ref-function 'org-ref-insert-ref-link
     org-ref-cite-onclick-function (lambda (_) (org-ref-citation-hydra/body))
     )
    :config
    ;; https://github.com/tmalsburg/helm-bibtex/issues/280
    (setq warning-minimum-log-level :error))

  (use-package org-ref
    :init
    (setq bibtex-completion-library-path
          (list ktz--org-ref-pdfs)

          bibtex-completion-pdf-open-function
          (lambda (fpath) (call-process "open" nil 0 nil fpath))

          bibtex-completion-bibliography
          ktz--org-ref-bibfiles

          bibtex-autokey-year-length 4
          bibtex-autokey-name-year-separator ""
	        bibtex-autokey-year-title-separator ""
	        bibtex-autokey-titleword-separator ""
	        bibtex-autokey-titlewords 1
	        bibtex-autokey-titlewords-stretch 1
	        bibtex-autokey-titleword-length 10)

    (require 'org-ref-helm)
    (require 'org-ref-arxiv)

    :bind (("C-c r c" . org-ref-cite-insert-helm)
           ("C-c r r" . org-ref-insert-ref-link)
           ("C-c r l" . org-ref-insert-label-link)
           ("C-c r g" . gscholar-bibtex)
           ("C-c r b" . helm-bibtex)))

  (use-package company-bibtex
    :config
    (add-to-list 'company-backends 'company-bibtex))

  ;; org-ref integrates biblio to browse and retrieve bibtex entries
  (defun ktz--biblio-ref-add (bibtex entry)
    "Add entry to bibtex file."
    (with-current-buffer (find-file-noselect (car ktz--org-ref-bibfiles))
      (goto-char (point-max))
      (insert (concat "\n\n" bibtex))
      (org-ref-clean-bibtex-entry)))

  (defun ktz--biblio-ref-select-and-add ()
    "Append current entry to bibtex file."
    (interactive)
    (biblio--selection-forward-bibtex #'ktz--biblio-ref-add))

  (use-package biblio
    :bind
    (:map biblio-selection-mode-map
          ("a" . ktz--biblio-ref-select-and-add)))

  ;; use this to autoformat whole bibfiles
  (bibtex-map-entries
   (lambda (key start end)
     (goto-char start)
     (ignore-errors
       (message (format "cleaning %s" key))
       (org-ref-clean-bibtex-entry))))

  (use-package gscholar-bibtex
    :init
    (setq gscholar-bibtex-database-file (car ktz--org-ref-bibfiles)))


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
    (message "[ktz] initializing org configuration")
    (ktz--init-org-base)
    (when ktz-org-dir
      (ktz--init-org-roam)))

  t)

(defun ktz-org ()
  "Initialize org related config manually"
  (interactive)
  (ktz--init-org))


(provide 'ktz-init-org)
