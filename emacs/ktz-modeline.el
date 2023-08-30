;; default (see C-h v for mode-line-format)
;; thanks https://www.youtube.com/watch?v=Qf_DLPIA9Cs

;; special keywords like :eval and :propertize are evaluated
;; when the mode-line is updated

;; TODO figure out how to use theme colors
;; (maybe it will work using the modus themes hook and custom-set-attributes)

(setq mode-line-compact nil) ; Emacs 28


;; face definitions

(defun ktz--modeline-pad-strbox (str)
  (format "%2s%s%2s" "" str ""))


(defface ktz-modeline-fg-subtle '() "")
(face-spec-set 'ktz-modeline-fg-subtle
               '((t :foreground "#BDBDBD")))

(defface ktz-modeline-fg-middle '() "")
(face-spec-set 'ktz-modeline-fg-middle
               '((t :foreground "#AAA")))

(defface ktz-modeline-indicator-base '((t :inherit 'fixed-pitch)) "")
(set-face-attribute 'ktz-modeline-indicator-base nil :box '(:line-width 1))

(defface ktz-modeline-indicator-mod '() "")
(face-spec-set 'ktz-modeline-indicator-mod
               '((t :inherit ktz-modeline-indicator-base
                    :background "#F48FB1" :foreground "#000")))

(defface ktz-modeline-indicator-ro '() "")
(face-spec-set 'ktz-modeline-indicator-ro
               '((t :inherit ktz-modeline-indicator-base
                    :background "#FAFAFA")))

(defface ktz-modeline-indicator-rw '() "")
(face-spec-set 'ktz-modeline-indicator-rw
               '((t :inherit ktz-modeline-indicator-base
                    :background "#CCFBF1")))

(defface ktz-modeline-indicator-god '() "")
(face-spec-set 'ktz-modeline-indicator-god
               '((t :inherit ktz-modeline-indicator-base
                    :background "#666" :foreground "#fff")))


;; getter functions: build and propertize strings

(defun ktz--modeline-get-god-indicator ()
  (let ((str (ktz--modeline-pad-strbox "GOD")))
    (if god-local-mode
        (propertize str 'face 'ktz-modeline-indicator-god)
      (propertize str 'face 'ktz-modeline-indicator-base))))


(defun ktz--modeline-get-status-indicator ()
  (cond (buffer-read-only
         (propertize (ktz--modeline-pad-strbox "RO")
                     'face 'ktz-modeline-indicator-ro))
        ((buffer-modified-p)
         (propertize (ktz--modeline-pad-strbox "ツ") ;; †⸸
                     'face 'ktz-modeline-indicator-mod))
        (t
         (propertize (ktz--modeline-pad-strbox "RW")
                     'face 'ktz-modeline-indicator-rw))))


(defun ktz--modeline-get-buffer-name ()
  "Returns the buffer name"
  (format "%s %s"
          (propertize "B:" 'face 'ktz-modeline-fg-subtle)
          (buffer-name)))


(defun ktz--modeline-get-major-mode ()
  "Returns the major mode name"
  (format "%s %s"
          (propertize "M:"
                      'face 'ktz-modeline-fg-subtle)
          (propertize (symbol-name major-mode)
                      'face 'ktz-modeline-fg-middle)))


(defun ktz--modeline-get-minor-modes ()
  "Returns a minor mode enumeration"
  (propertize (format "(+%s)" (length local-minor-modes))
              'face 'ktz-modeline-fg-subtle))


;; register variables from getter functions

(defvar-local ktz--modeline-god-indicator
    '(:eval (ktz--modeline-get-god-indicator) "Whether god mode is active"))
(defvar-local ktz--modeline-status-indicator
    '(:eval (ktz--modeline-get-status-indicator) "File acces rights and state"))
(defvar-local ktz--modeline-buffer-name
    '(:eval (ktz--modeline-get-buffer-name) "Current file/buffer name"))
(defvar-local ktz--modeline-major-mode
    '(:eval (ktz--modeline-get-major-mode) "Major mode name"))
(defvar-local ktz--modeline-minor-modes
    '(:eval (ktz--modeline-get-minor-modes) "Minor mode enumeration"))

(dolist (construct '(ktz--modeline-god-indicator
                     ktz--modeline-status-indicator
                     ktz--modeline-buffer-name
                     ktz--modeline-major-mode
                     ktz--modeline-minor-modes))
  (put construct 'risky-local-variable t))


;; add a face to a string
;; (propertize STRING 'face 'error)

(defun ktz--init-modeline ()
  ;; mode-line-format becomes a buffer-local variable
  (setq-default mode-line-format
                '("%e"
                  ktz--modeline-god-indicator
                  ktz--modeline-status-indicator
                  "  "
                  ktz--modeline-buffer-name
                  "  "
                  ktz--modeline-major-mode
                  " "
                  ktz--modeline-minor-modes
                  )))


;; to reset
(defun ktz-modeline-reset ()
  (interactive)
  (setq-default mode-line-format
                '("%e" mode-line-front-space
                  (:propertize
                   ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote)
                   display
                   (min-width
                    (5.0)))
                  mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position
                  (vc-mode vc-mode)
                  "  " mode-line-modes mode-line-misc-info mode-line-end-spaces)))


(provide 'ktz-modeline)
