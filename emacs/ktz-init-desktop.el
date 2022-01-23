;;; ktz-init-desktop.el --- Desktop initialization.


;; user interface


(defcustom ktz-org-dir nil
  "Directory where the .org files reside (e.g. path/to/Roam)"
  :type 'directory
  :group 'ktz)


;; initialization


;; must be initialized at import time
;; so that the splash screen is shown
;; (if (boundp 'ktz-org-dir)
;;   (setq org-agenda-start-with-log-mode t))


(defvar ktz--pkgs-desktop
  '(
    auctex
    pdf-tools
    visual-fill-column
    flyspell-correct-helm

    ;; for org-roam configuration
    use-package

    ;; doom-themes
    ;; doom-modeline

    org-roam
    org-bullets
    org-super-agenda

    '(nano-emacs :type git :host github :repo "rougier/nano-emacs")

    ))


;; --


(defun ktz--init-desktop-roam ()
  ;; see https://github.com/org-roam/org-roam#configuration
  (use-package org-roam

    :custom
    (org-roam-directory ktz-org-dir)
    (org-agenda-files (list ktz-org-dir))
    (org-agenda-start-with-log-mode t)

    ;; `(1 ,(- 3 1)) -> (1 2) (the (concat ...) needs to be evaluated)
    ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Backquote.html
    ;; https://www.reddit.com/r/emacs/comments/quy7gd/setting_an_org_roam_capture_template/
    (org-roam-capture-templates
     `(("d" "default" plain
        (file ,(concat ktz-org-dir "/_templates/default.org"))
        :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "")
        :unnarrowed t)))

    :init
    (setq org-roam-v2-ack t)

    :bind (("C-c n l" . org-roam-buffer-toggle)
           ("C-c n f" . org-roam-node-find)
           ("C-c n i" . org-roam-node-insert)
           ("M-." . org-open-at-point)
           ("M-," . org-mark-ring-goto)
           :map org-mode-map
           ("C-M-i"    . completion-at-point))

    :config
    (org-roam-setup)
    (setq org-log-done 'time))

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
    (org-agenda nil "a"))

  t)


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
  (setq nano-color-foreground "#37474F")
  (setq nano-color-background "#FFFFFF")
  (setq nano-color-highlight  "#FAFAFA")
  (setq nano-color-critical   "#c2185b") ;; pink 700
  (setq nano-color-salient    "#6a1b9a") ;; purple 700
  (setq nano-color-strong     "#000000")
  (setq nano-color-popout     "#00695c") ;; green 800
  (setq nano-color-subtle     "#ECEFF1")
  (setq nano-color-faded      "#90a4ae") ;; blue grey 300 B0BEC5

  t)


(defun ktz--init-desktop-overwrite-faces ()
  (let ;; overwrite (mostly 'light) :weight face attributes
      ((weight-alist
        '((medium
           . (nano-face-default
              nano-face-faded))
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

  t)


(defun ktz--init-desktop-theme ()

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

  (with-eval-after-load 'helm-bookmark
    (set-face-attribute
     'helm-bookmark-file nil
     :foreground nano-color-popout)

    (set-face
     'helm-bookmark-file-not-found
     'nano-face-critical))

  t)


(defun ktz--init-desktop-nano ()
  ;; the order of all these expressions is very important...

  ;; attributes must be set before requiring nano
  (setq nano-font-size 11)
  (setq nano-font-family-monospaced "Source Code Pro")
  (setq nano-font-family-proportional "Source Serif Pro")

  ;; basic layout customization (window divider)
  (require 'nano-layout)

  ;; lots of defcustom with nil initialization
  (require 'nano-faces)

  ;; custom theme
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
  (require 'nano-modeline)

  t)


;; currently unused in favor of nano
(defun ktz--init-desktop-ktz ()


  (dolist(face '(nano-face-strong nano-face-tag-default nano-face-tag-string))
    (set-face-attribute face nil :weight 'semi-bold))

  (set-face-attribute
   'nano-face-faded nil
   :weight 'bold)
  (set-face-attribute
   'nano-face-salient nil
   :weight 'bold)


  global doom settings
  (require 'doom-themes)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	doom-themes-enable-italic t) ; if nil, italics is universally disabled

  enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)

  show line numbers
  (global-linum-mode 1)

  add some space
  (fringe-mode 10)

  dynamically set theme based on environment vars
  (when (getenv "KTZ_LIGHT")
    (load-theme 'doom-kantholtz-light t))
  (when (getenv "KTZ_DARK")
    (load-theme 'doom-kantholtz-dark t))
)


(defun ktz--init-desktop-graphic-p ()

  (ktz--init-desktop-nano)
  ;; (ktz--init-desktop-ktz)

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
  (when (boundp 'ktz-org-dir)
    (ktz--init-desktop-roam))

  (when (display-graphic-p)
    (progn

      ;; theming
      (ktz--init-desktop-graphic-p)

      ;; disable window clutter
      (menu-bar-mode -1)
      (tool-bar-mode -1)
      (scroll-bar-mode -1)

      ;; show as splash screen
      (org-agenda-list)))

  t)


(provide 'ktz-init-desktop)
