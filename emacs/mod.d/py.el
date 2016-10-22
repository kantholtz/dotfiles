;;
;;   python programming
;;
(setq python-basic-offset 2)
(put 'dired-find-alternate-file 'disabled nil)

;; use ipython as python shell
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i")

(elpy-enable)


;; ;; enable pep8 syntax checks
;; (when (load "flymake" t)
;;   (defun flymake-pylint-init ()
;;     (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                        'flymake-create-temp-inplace))
;;            (local-file (file-relative-name
;;                         temp-file
;;                         (file-name-directory buffer-file-name))))
;;       (list "pep8" (list "--repeat" local-file))))

;;   (add-to-list 'flymake-allowed-file-name-masks
;;                '("\\.py\\'" flymake-pylint-init)))

;; (defun my-flymake-show-help ()
;;   (when (get-char-property (point) 'flymake-overlay)
;;     (let ((help (get-char-property (point) 'help-echo)))
;;       (if help (message "%s" help)))))

;; (add-hook 'post-command-hook 'my-flymake-show-help)
