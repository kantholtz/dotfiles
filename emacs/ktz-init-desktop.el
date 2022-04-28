;;; ktz-init-desktop.el --- Desktop initialization.

(defvar ktz--pkgs-desktop
  '(
    auctex
    pdf-tools
    flyspell-correct-helm

    '(nano-emacs :type git :host github :repo "rougier/nano-emacs")
    ))


;; --


(defun ktz--org-visual-fill ()
  (visual-fill-column-mode 1))


(defun ktz--init-desktop-roam ()

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
    :straight t
    :hook (org-mode . ktz--org-visual-fill))

  ;; see also https://github.com/org-roam/org-roam#configuration
  (use-package org-roam
    :straight t

    :custom
    (org-log-done 'time)
    (org-roam-directory ktz--org-orgfiles)
    (org-agenda-files (list ktz--org-orgfiles))
    (org-agenda-start-with-log-mode t)

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
    :straight t
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
    :straight t

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
    :straight t
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
    :straight t
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
    :straight t
    :init
    (setq gscholar-bibtex-database-file (car ktz--org-ref-bibfiles)))


  ;;
  ;; agenda configuration
  ;;

  (use-package org-super-agenda
    :straight t
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
  (use-package org-bullets :straight t))



(defun ktz--init-desktop-org ()

  ;; (require 'org-pretty-table)
  ;; (add-hook 'org-mode-hook (lambda () (org-pretty-table-mode)))

  ;; (when (display-graphic-p)
  ;;   (setq org-ellipsis " ▼")
  ;;   (require 'org-bullets)
  ;;   (setq org-bullets-bullet-list '("●" "●" "○" "○" "▫"))
  ;;   (add-hook 'org-mode-hook 'org-bullets-mode))

  ;; do not destroy current splits
  (setq org-agenda-window-setup 'current-window)

  ;; hooks
  (add-hook 'org-mode-hook 'org-indent-mode)
  (add-hook 'org-mode-hook 'visual-line-mode)

  t)


(defun ktz--init-desktop-theme-set-light ()

  ;; use M-x list-faces-display
  ;; or M-x describe-face (with cursor)

  "Overwriting nano-theme-light.el."
  (setq frame-background-mode    'light)
  (setq nano-color-foreground "#37474f") ;; blue grey 800
  (setq nano-color-background "#fafafa") ;; grey 50
  (setq nano-color-highlight  "#b0bec5") ;; blue grey 200
  (setq nano-color-critical   "#c2185b") ;; pink 700
  (setq nano-color-salient    "#4527a0") ;; deep purple 800
  (setq nano-color-strong     "#000000") ;; black
  (setq nano-color-popout     "#00695c") ;; green 800
  (setq nano-color-subtle     "#eceff1") ;; blue grey 100
  (setq nano-color-faded      "#90a4ae") ;; blue grey 300

  ;; adding some of my own
  ;; (later: use defcustom)
  (defvar nano-color-warning    "#bf360c")  ;; deep orange 900

  t)


(defun ktz--init-desktop-overwrite-faces ()
  (let ;; overwrite (mostly 'light) :weight face attributes
      ((weight-alist
        '((medium
           . (nano-face-default
              nano-face-faded
              nano-face-header-default))
          (semi-bold
           . (nano-face-salient
              nano-face-tag-default
              nano-face-tag-strong
              nano-face-tag-salient
              nano-face-tag-popout
              nano-face-tag-faded
              nano-face-tag-critical))
          (bold
           . (nano-face-strong)))))

    (dolist (pair weight-alist)
      (dolist (face (cdr pair))
        (set-face-attribute face nil :weight (car pair)))))

  (set-face-attribute
   'nano-face-critical nil
   :foreground nano-color-critical
   :background nano-color-background)

  (set-face-attribute
   'line-number nil :foreground "pink")

  t)


(defun ktz--init-desktop-theme ()

  ;; org

  (set-face-attribute
   'org-block nil
   :background nano-color-subtle)

  ;; helm

  (set-face-attribute
   'helm-ff-directory nil
   :weight 'bold
   :foreground nano-color-popout
   :background nano-color-background)

  (set-face-attribute
   'helm-ff-dotted-directory nil
   :foreground nano-color-faded
   :background nano-color-background)

  (set-face-attribute
   'helm-ff-executable nil
   :foreground nano-color-critical)

  (set-face-attribute
   'helm-ff-file-extension nil
   :foreground nano-color-salient)

  (with-eval-after-load 'helm-bookmark
    (set-face-attribute
     'helm-bookmark-file nil
     :foreground nano-color-popout)

    (set-face
     'helm-bookmark-file-not-found
     'nano-face-critical))

  ;; lsp

  (set-face-attribute
   'header-line nil :background nano-color-subtle)

  (add-hook
   'lsp-after-initialize-hook
   (lambda ()

     (set-face-attribute
      'lsp-headerline-breadcrumb-path-face nil
      :weight 'regular)

     (set-face-attribute
      'lsp-headerline-breadcrumb-path-error-face nil
      :underline nil
      :foreground nano-color-critical)

     (set-face-attribute
      'lsp-headerline-breadcrumb-path-warning-face nil
      :underline nil
      :foreground nano-color-warning)

     (set-face-attribute
      'lsp-headerline-breadcrumb-path-hint-face nil
      :underline nil
      :foreground nano-color-salient)

     (set-face-attribute
      'lsp-headerline-breadcrumb-path-info-face nil
      :underline nil
      :foreground nano-color-salient)

     (customize-set-variable
      'lsp-headerline-breadcrumb-segments
      '(path-up-to-project file))))

  ;; company

  ;; does not work (?)
  ;; (use-package company
  ;;   :custom-face
  ;;   `(company-tooltip ((t (:background ,nano-color-background)))))

  (add-hook
   'company-mode-hook
   (lambda ()

     (set-face-attribute
      'company-tooltip nil
      :background nano-color-background)

     (set-face-attribute
      'company-tooltip-selection nil
      :foreground nano-color-foreground
      :background nano-color-background)

     (set-face-attribute
      'company-tooltip-common-selection nil
      :foreground nano-color-salient
      :background nano-color-background)

     (set-face-attribute
      'company-tooltip-scrollbar-thumb nil
      :background nano-color-background)

     (set-face-attribute
      'company-tooltip-scrollbar-track nil
      :background nano-color-background)))


  ;; flycheck
  (require 'flycheck)

  (set-face-attribute
   'flycheck-warning nil
   :underline nil
   :foreground nano-color-warning)

  (set-face-attribute
   'flycheck-error nil
   :underline nil
   :foreground nano-color-critical)

  ;; other

  (set-face-attribute
   'show-paren-match nil
   :foreground nano-color-critical
   :weight 'bold)

  t)


(defun ktz--init-desktop-nano ()
  ;; gonna absorb and adapt nano one by one
  ;; the order of all these expressions is very important...

  ;; attributes must be set before requiring nano
  (setq nano-font-size ktz-font-size)
  (setq nano-font-family-monospaced ktz-font-monospace)
  (setq nano-font-family-proportional ktz-font-proportional)

  ;; basic layout customization (window divider)
  (require 'nano-layout)

  ;; lots of defcustom with nil initialization
  (require 'nano-faces)

  ;; overwrite colors
  (ktz--init-desktop-theme-set-light)

  ;; sets many attributes, e.g. font weights
  (nano-faces)
  (ktz--init-desktop-overwrite-faces)

  ;; assigns the nano-faces (set-face, set-face-attribute)
  (require 'nano-theme)
  (nano-theme)
  (ktz--init-desktop-theme)

  ;; (nano-theme) invokes (nano-theme--basics) which
  ;; overwrites :weight for 'default...
  (set-face-attribute 'default nil :weight 'regular)

  ;; setting up the modeline
  ;; (require 'nano-modeline)
  (require 'ktz-theme-modeline))


(defun ktz--init-desktop-graphic-p ()

  (ktz--init-desktop-nano)

  ;; spell checks
  (setq ispell-program-name (executable-find "hunspell") ispell-dictionary "en_GB")
  (defun flyspell-check-next-highlighted-word ()
    "Custom function to spell check next highlighted word"
    (interactive)
    (flyspell-goto-next-error)
    (ispell-word))

  t)


(defun ktz--init-desktop ()
  "Setup desktop configuration - includes Roam, LaTex etc."
  (dolist (pkg ktz--pkgs-desktop)
    (straight-use-package pkg))

  ;; lay your weary pinky to rest
  (require 'control-lock)
  (control-lock-keys)
  (global-set-key (kbd "C-`") 'control-lock-enable)

  (ktz--init-desktop-org)
  (when ktz-org-dir
    (ktz--init-desktop-roam))

  (when (display-graphic-p)

      ;; theming
      (ktz--init-desktop-graphic-p)

      ;; disable window clutter
      (menu-bar-mode -1)
      (tool-bar-mode -1)
      (scroll-bar-mode -1))
  t)


(provide 'ktz-init-desktop)
