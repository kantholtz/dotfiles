;; default (see C-h v for mode-line-format)
;; thanks https://www.youtube.com/watch?v=Qf_DLPIA9Cs

;; special keywords like :eval and :propertize are evaluated
;; when the mode-line is updated


(defcustom ktz-modeline-string-truncate-length 9
  "String length after which truncation should be done in small windows."
  :type 'natnum)

(setq mode-line-compact nil) ; Emacs 28


;; backported from Emacs 29.1
(defun mode-line-window-selected-p ()
  "Return non-nil if we're updating the mode line for the selected window.
This function is meant to be called in `:eval' mode line
constructs to allow altering the look of the mode line depending
on whether the mode line belongs to the currently selected window
or not."
  (let ((window (selected-window)))
    (or (eq window (old-selected-window))
	      (and (minibuffer-window-active-p (minibuffer-window))
	           (with-selected-window (minibuffer-window)
	             (eq window (minibuffer-selected-window)))))))


(defun ktz--modeline-string-truncate-p (str)
  "Return non-nil if STR should be truncated."
  (and (< (window-total-width) split-width-threshold)
       (> (length str) ktz-modeline-string-truncate-length)
       (not (one-window-p :no-minibuffer))))


(defun ktz-modeline-string-truncate (str)
  "Return truncated STR, if appropriate, else return STR.
Truncation is done up to `ktz-modeline-string-truncate-length'."
  (if (ktz--modeline-string-truncate-p str)
      (concat (substring str 0 ktz-modeline-string-truncate-length) "...")
    str))


;; face definitions

(defun ktz--modeline-pad-strbox (str &optional padding)
  "Pad a string symmetrically with the provided padding"
  (let ((d (if padding padding 2)))
    (format (format "%%%ds%%s%%%ds" d d) "" str "")))


(defface ktz-modeline-fg-subtle '()
  "least visible text in the modeline")
(defface ktz-modeline-fg-middle '()
  "better visible but not foreground")
(defface ktz-modeline-bg-subtle '()
  "bit more dark/bright than the modeline bg")


(defface ktz-modeline-indicator-base '((t :inherit 'fixed-pitch)) "")
;; (set-face-attribute 'ktz-modeline-indicator-base nil :box '(:line-width 1))

(defface ktz-modeline-indicator-mod
  '((t :inherit 'ktz-modeline-indicator-base))
  "whether the file has been modified")

(defface ktz-modeline-indicator-ro
  '((t :inherit 'ktz-modeline-indicator-base))
  "whether the file is read only")
(defface ktz-modeline-indicator-rw
  '((t :inherit 'ktz-modeline-indicator-base))
  "whether the file can be written to")
(defface ktz-modeline-indicator-god-active
  '((t :inherit 'ktz-modeline-indicator-base))
  "when god mode is active")
(defface ktz-modeline-indicator-god-inactive
  '((t :inherit 'ktz-modeline-indicator-base))
  "when god mode is inactive")


;; callback of modus-themes toggler to set faces
;; based on the active theme defintions
(defun ktz-modeline-set-faces ()
  (modus-themes-with-colors

    (set-face-attribute 'mode-line nil
                        :underline border-mode-line-active
                        :overline border-mode-line-active
                        :box `(:line-width 3 :color ,bg-mode-line-active))

    (set-face-attribute 'mode-line-inactive nil
                        :underline border-mode-line-inactive
                        :overline border-mode-line-inactive
                        :box `(:line-width 3 :color ,bg-mode-line-inactive))

    (set-face-attribute 'ktz-modeline-fg-subtle nil
                        :foreground fg-dim)

    (set-face-attribute 'ktz-modeline-fg-middle nil
                        :foreground fg-main)

    (set-face-attribute 'ktz-modeline-bg-subtle nil
                        :background cyan-faint)

    (set-face-attribute 'ktz-modeline-indicator-mod nil
                        :background (if (eq ktz-theme-current 'modus-operandi)
                                        red bg-red-nuanced)
                        :foreground (if (eq ktz-theme-current 'modus-operandi)
                                        bg-main red))

    (set-face-attribute 'ktz-modeline-indicator-rw nil
                        :foreground fg-alt)

    (set-face-attribute 'ktz-modeline-indicator-god-active nil
                        :background (if (eq ktz-theme-current 'modus-operandi)
                                        fg-alt bg-main)
                        :foreground (if (eq ktz-theme-current 'modus-operandi)
                                        bg-main fg-alt))

    (set-face-attribute 'ktz-modeline-indicator-god-inactive nil
                        :foreground fg-dim)
    ;; :foreground (when (eq ktz-theme-current 'dark)
    ;;               fg-dim))
    ))


;; getter functions: build and propertize strings

(defun ktz--modeline-get-god-indicator ()
  (let ((str (ktz--modeline-pad-strbox "GOD" 1)))
    (if (and (boundp 'god-local-mode) god-local-mode)
        (propertize str 'face 'ktz-modeline-indicator-god-active)
      (propertize str 'face 'ktz-modeline-indicator-god-inactive))))


(defun ktz--modeline-get-status-indicator ()
  (let ((pad 1))
    (cond (buffer-read-only
           (propertize (ktz--modeline-pad-strbox "◇" pad)
                       'face 'ktz-modeline-indicator-ro))
          ((buffer-modified-p)
           (propertize (ktz--modeline-pad-strbox "◆" pad)
                       'face 'ktz-modeline-indicator-mod))
          (t
           (propertize (ktz--modeline-pad-strbox "◆" pad)
                       'face 'ktz-modeline-indicator-rw)))))


(defun ktz--modeline-get-buffer-name ()
  "Returns the buffer name"
  (ktz--modeline-pad-strbox (buffer-name)))


(defun ktz--modeline-get-position ()
  "Returns line and char offset for the current point"
  (propertize (format-mode-line "%l:%c")
              'face 'ktz-modeline-fg-middle))


(defun ktz--modeline-get-major-mode ()
  "Returns the major mode name"
  (format "%s %s"
          (propertize "M:"
                      'face 'ktz-modeline-fg-subtle)
          (propertize (symbol-name major-mode)
                      'face 'ktz-modeline-fg-middle)))


(defun ktz--modeline-get-minor-modes ()
  "Returns a minor mode enumeration"
  (propertize
   (ktz--modeline-pad-strbox
    (format "(+%s)" (length local-minor-modes)))
   'face 'ktz-modeline-fg-subtle))


;; vc-* taken from prot (thanks!)

(defvar ktz--modeline-vc-faces
  '((added . vc-locally-added-state)
    (edited . vc-edited-state)
    (removed . vc-removed-state)
    (missing . vc-missing-state)
    (conflict . vc-conflict-state)
    (locked . vc-locked-state)
    (up-to-date . vc-up-to-date-state))
  "VC state faces.")

(declare-function vc-git--symbolic-ref "vc-git" (file))

(defun ktz--modeline-vc-branch-name (file backend)
  "Return capitalized VC branch name for FILE with BACKEND."
  (when-let ((rev (vc-working-revision file backend))
             (branch (or (vc-git--symbolic-ref file)
                         (substring rev 0 7))))
    (capitalize branch)))

(defun ktz--modeline-vc-details (file branch &optional face)
  "Return Git BRANCH details for FILE, truncating it if necessary.
The string is truncated if the width of the window is smaller
than `split-width-threshold'."
  (ktz-modeline-string-truncate
   (ktz--modeline-vc-text file branch face)))

(defun ktz--modeline-vc-text (file branch &optional face)
  "Prepare text for Git controlled FILE, given BRANCH.
With optional FACE, use it to propertize the BRANCH."
  (concat
   (propertize (char-to-string #xE0A0) 'face 'shadow)
   " "
   (propertize branch 'face face)
   " "
   ))

(defun ktz--modeline-vc-get-face (key)
  "Get face from KEY in `ktz--modeline-vc-faces'."
  (alist-get key ktz--modeline-vc-faces 'up-to-date))

(defun ktz--modeline-vc-face (file backend)
  "Return VC state face for FILE with BACKEND."
  (ktz--modeline-vc-get-face (vc-state file backend)))


;; register variables from getter functions

(defvar-local ktz--modeline-god-indicator
    '(:eval (if (mode-line-window-selected-p) (ktz--modeline-get-god-indicator) ""))
  "Whether god mode is active")
(defvar-local ktz--modeline-status-indicator
    '(:eval (ktz--modeline-get-status-indicator))
  "File acces rights and state")
(defvar-local ktz--modeline-buffer-name
    '(:eval (ktz--modeline-get-buffer-name))
  "Current file/buffer name")
(defvar-local ktz--modeline-position
    '(:eval (if (mode-line-window-selected-p) (ktz--modeline-get-position) ""))
  "Current cursor position")
(defvar-local ktz--modeline-major-mode
    '(:eval (if (mode-line-window-selected-p) (ktz--modeline-get-major-mode) ""))
  "Major mode name")
(defvar-local ktz--modeline-minor-modes
    '(:eval (if (mode-line-window-selected-p) (ktz--modeline-get-minor-modes) ""))
  "Minor mode enumeration")
(defvar-local ktz--modeline-vc-info
    '(:eval
      (when-let* (((mode-line-window-selected-p))
                  (file (buffer-file-name))
                  (backend (vc-backend file))
                  (branch (ktz--modeline-vc-branch-name file backend))
                  (face (ktz--modeline-vc-face file backend)))
        (ktz--modeline-vc-details file branch face)))
  "Mode line construct to return propertized VC branch.")


(dolist (construct '(ktz--modeline-god-indicator
                     ktz--modeline-status-indicator
                     ktz--modeline-buffer-name
                     ktz--modeline-position
                     ktz--modeline-major-mode
                     ktz--modeline-minor-modes
                     ktz--modeline-vc-info))
  (put construct 'risky-local-variable t))


(defun ktz--init-modeline ()
  ;; mode-line-format becomes a buffer-local variable

  ;; move :underline (set for the mode-line face)
  ;; to the very bottom instead of the font's baseline
  (setq x-underline-at-descent-line t)

  (setq-default mode-line-format
                '("%e"
                  ktz--modeline-god-indicator
                  ktz--modeline-status-indicator
                  ktz--modeline-buffer-name
                  ktz--modeline-major-mode
                  ktz--modeline-minor-modes
                  ktz--modeline-vc-info
                  ktz--modeline-position))

  ;; the file or project name:
  ;;  (:eval (breadcrumb-project-crumbs))
  (setq-default header-line-format
                '((:eval (breadcrumb-imenu-crumbs)))))



;;; HEADERLINE

(defun ktz-headerline-set-faces ()
  (modus-themes-with-colors
    (set-face-attribute
     'header-line nil
     :background bg-dim
     :underline border
     :overline border
     :box `(:line-width 5 :color ,bg-dim))))



(provide 'ktz-modeline)
