;; append
;; (goto-char (point-max))
;; save current point
;; (save-excursion ...)
;; get things at point:
;; (thing-at-point SYM) where
;;   SYM in { 'word, 'sentence, 'sexp, 'url, ... }
;; (thing-at-point SYM t) returns only the raw text
;; (search-forward ...) (search-backward ...)


(defun ktz--splash-quote (&rest file-paths)
  (let ((contents ""))
    (dolist (file-path file-paths)
      (setq contents (concat contents
                             (with-temp-buffer
                               (insert-file-contents file-path)
                               (buffer-string)))))
    (let ((quotes (split-string contents "---" t)))
      (when quotes (nth (random (length quotes)) quotes)))))

(defun ktz-splash ()
  ;; (ktz-log "splash" (format
  ;;                    "length=%d args=%s"
  ;;                    (length command-line-args)
  ;;                    command-line-args))

  (when (= 1 (length command-line-args))

    (let ((splash-buffer-name "*ktz*")
          (assets-dir (concat ktz-root-dir "/../assets")))

      (with-output-to-temp-buffer splash-buffer-name
        ;; select and style
        (switch-to-buffer splash-buffer-name)
        (delete-other-windows)
        (set-window-margins nil 10)

        ;; content
        (newline 2)
        (insert (ktz--splash-quote
                 (concat assets-dir "/verses/kantholtz")
                 (concat assets-dir "/verses/eldenring")))
        (newline 2)
        (insert-file-contents
         (concat ktz-root-dir "ktz-splash.md"))

        ;; style and interaction
        (if (require 'markdown-mode nil 'noerror)
            (markdown-mode))

        ;; doesn't work atm; maybe because god-mode
        ;; hooks overwrite the cursor-type
        (setq cursor-type nil)
        (setq buffer-read-only t)))

    ))


;; No startup screens or messages
(customize-set-variable 'inhibit-splash-screen t)
(customize-set-variable 'inhibit-startup-screen t)
(customize-set-variable 'inhibit-startup-message t)
(customize-set-variable 'inhibit-startup-echo-area-message t)

(defun ktz-show-splash ()
  (interactive)
  (ktz-splash))

(provide 'ktz-splash)

