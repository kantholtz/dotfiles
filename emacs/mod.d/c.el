;;
;;   c programming
;;
(add-to-list 'load-path "~/.emacs.d/cc-mode/")
(require 'cc-mode)

(setq c-basic-offset 4
      c-default-style "linux")

(define-key c-mode-base-map
  (kbd "RET") 'newline-and-indent)

;;(add-hook 'find-file-hook 'flymake-find-file-hook)

(defun make-run ()
  (interactive)
  (switch-to-buffer-other-window "the executor")
  (erase-buffer)
  (start-process-shell-command
   "executor" "the executor" "make run"))

  ;; (shell-command "make run") "the executor")

(add-hook 'compilation-mode-hook 'visual-line-mode)


(global-set-key (kbd "C-x [") 'compile)
(setq compilation-scroll-output t)

(global-set-key (kbd "C-x ]") 'make-run)
(global-set-key (kbd "C-x C-]") 'gdb)
