;;
;;   python programming
;;
(setq python-basic-offset 2)
(put 'dired-find-alternate-file 'disabled nil)

;; use conda for pyvenv
;; -- this is not working, as conda overwrites its own CONDA_PREFIX
;; -- as soon as an environment is activated. It seems like there
;; -- is no canonical way to access the conda base directory... (wtf)
;; (setenv "WORKON_HOME" (concat (getenv "CONDA_PREFIX") "/envs"))
;; (pyvenv-mode 1)

(require 'python)
(elpy-enable)

;; enable sphinx-doc
(add-hook
 'python-mode-hook
 (lambda ()
   (require 'sphinx-doc)
   (sphinx-doc-mode t)))

;; use ipython as python shell
(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args "--simple-prompt -i"
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")


(message
 (concat
  "nvrn: configured python - venvs: " (getenv "WORKON_HOME")))
