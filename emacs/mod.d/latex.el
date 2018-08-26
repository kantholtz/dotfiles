
;; only auto-fill outside of equations

(defvar nvrn-LaTeX-no-autofill-environments
  '("equation" "equation*")
  "A list of LaTeX environment names in which `auto-fill-mode' should be inhibited.")

(defun nvrn-LaTeX-auto-fill-function ()
  "This function checks whether point is currently inside one of
the LaTeX environments listed in
`nvrn-LaTeX-no-autofill-environments'. If so, it inhibits automatic
filling of the current paragraph."
  (let ((do-auto-fill t)
        (current-environment "")
        (level 0))
    (while (and do-auto-fill (not (string= current-environment "document")))
      (setq level (1+ level)
            current-environment (LaTeX-current-environment level)
            do-auto-fill (not (member current-environment nvrn-LaTeX-no-autofill-environments))))
    (when do-auto-fill
      (do-auto-fill))))

(defun nvrn-LaTeX-setup-auto-fill ()
  "This function turns on auto-fill-mode and sets the function
used to fill a paragraph to `nvrn-LaTeX-auto-fill-function'."
  (auto-fill-mode)
  (setq auto-fill-function 'nvrn-LaTeX-auto-fill-function))


;; spell check related

(setq ispell-program-name (executable-find "hunspell")
      ispell-dictionary "en_GB")

;; I highly suggest setting ‘flyspell-issue-message-flag’ to nil, as
;; printing messages for every word (when checking the entire
;; buffer) causes an enormous slowdown. – nschum

;; interactive stuff

(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word))

(global-set-key (kbd "C-c J") 'flyspell-buffer)
(global-set-key (kbd "C-c j") 'flyspell-check-next-highlighted-word)

;; hooks

(add-hook 'LaTeX-mode-hook 'nvrn-LaTeX-setup-auto-fill)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(message "nvrn: registered latex hooks")
