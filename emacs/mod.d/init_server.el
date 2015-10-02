;;
;;  load the emacs config modules
;;

(setq nvrn-mod-list
      '(pack server))

(defun nvrn-load-config (mod-dir mod-list)
  (dolist (mod nvrn-mod-list)
    (load (concat mod-dir "/" (symbol-name mod)))))

;; load all files in emacs.d/mod.d
(nvrn-load-config "~/.emacs.d/mod.d" nvrn-mod-list)
