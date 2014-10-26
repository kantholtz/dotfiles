
;; include package repos
(require 'package)
(add-to-list 'package-archives 
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
	'("melpa" .
	  "http://melpa.milkbox.net/packages/") t)
(package-initialize)


;; for the iTerm2 theme "mona lisa"
(load-theme 'dreadworks t)


;; generally desired modes and configurations
(show-paren-mode t)
(ido-mode t)
(column-marker-1 80)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)


;; for faster reactions
(fset 'yes-or-no-p 'y-or-n-p)

;; no annoying beeps
(setq visible-bell t)

;; linum-mode config
(setq linum-format "%4d \u2502 ")
(global-linum-mode t)

;; magit config
(global-set-key (kbd "C-x g") 'magit-status)

;; aggregate all backups in one place
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(put 'dired-find-alternate-file 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("b2b7cfba98b4701231aaf6e7557b1a00a19d801508504378859b4debc49f70bf" "c7426aa09640f5d5df05f94bfbec9890df8be4f3c4910d571d5872b11eb2ccaa" "7fe588d938b5dfbe096b72fdc3f2b93770782f2f118a0053fb44a9874f4a35a6" "b9ca9dec91058ca664637837f9b82465a21de915f0c14be80120e95e9f8bf094" "72f53e3ae4ad4aa94ea78e38b3c2ffcd47952a06a0051c662848671a431a54d2" "5d6c78a27cdfcf6a62d2e6ad4f0430ba83916faad912955c83cb132af8d5cb3a" "278c3017111682d5354d3c5e5b78de73859d0d380651153f3e0ada7a493b82d5" "1eaf6386dde80a6cd8b74052b26c9be3693a8b052c20061ecc0f4b845526f5d2" "2193ba7060ddc24c43421612c33099cfb2256682f0a11b60319e9dad8a0bd004" "2dbd3d8aa7985074f61ed6ea3677032c36ad3da3b54e582fc387ffef68d4249d" "082c85e8e91f21ff1a1a73c2590a3642896e3aff292975b38efaccb7ac17e662" "a40781d7582c3d567b5bfb6fe282e54701d107fca02db4a190511a94367b885c" "725a6fcb02bdab98e438a4da5081d7737ca4620cf8da3a0a5aa326b93af109aa" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
