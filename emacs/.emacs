
;; include package repos
(require 'package)
;; (add-to-list 'package-archives 
;;     '("marmalade" .
;;       "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
	'("melpa" .
	  "http://melpa.milkbox.net/packages/") t)
(package-initialize)


;; for the iTerm2 theme "mona lisa"
(load-theme 'dreadworks t)

;; generally desired modes and configurations
(column-number-mode t)
(show-paren-mode t)
(ido-mode t)

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

;; multiple cursors config
(require 'multiple-cursors)
(global-set-key (kbd "C-x n") 'mc/mark-next-like-this)

;; aggregate all backups in one place
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))


;; c programming
(add-to-list 'load-path "~/.emacs.d/cc-mode/")

(setq c-basic-offset 4
      c-default-style "linux")

(defun my-make-CR-do-indent ()
  (define-key c-mode-base-map "\C-m" 'c-context-line-break))
(add-hook 'c-initialization-hook 'my-make-CR-do-indent)



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("5993aedd5c0b0c8be7a8aa1f5bace8fc3837121f65a60416e9d65ff7d5ad91c5" "7bf7c7b72fe6ea09e69d9f65d08157e588665e5573d143132ce5f100e1a1cfc7" "7c0a4df265ab084374e2d66763cc01c22314ea3b89cb26fceb4c7e3acd8ded2d" "be0efb9d405e6db4d6f62cda1f59eafcc96f3ec5ec532af3aac45680b60afc5e" "1c3152c834b5a678094d6a29898c44ef837e5c16986a2d6b166e5be8530d0048" "081038524d81a3dddb7824b2083281f04bb6e6361099bec7289071b12a6db886" "3f92caac9887be7c3a1d4cbef98776e0c1a753ec6d5a7461290a4e343933de49" "c52ed6314c9b532c7214fa563009002ba9628afc561d583ab89a36c35ca44023" "0e53b97c98df8fff29bd02c0e6b44bf797b6405437028427c55c4ad44d3f8012" "522c9def4d43ddd787f0c0f955e9eb5fbbc927ab589b88f03d3af4db7615af6c" "0e466e1f1de7e09fe5deba1e52c9251bb71d23f7e109096d1aa4405f1e7765d0" "c55b581e9d6210f53c584fc0e7071408a253c42a6bb4f4e4d1baca128954f194" "3a9fdc3e98cee072455e10168528e01d68057918ca530d693a9a53010bf6f200" "78ac84a67e5106b940081ee85abce96850d3fb2a8b7d7ccb1590f6cec5ed78c6" "b2b7cfba98b4701231aaf6e7557b1a00a19d801508504378859b4debc49f70bf" "c7426aa09640f5d5df05f94bfbec9890df8be4f3c4910d571d5872b11eb2ccaa" "7fe588d938b5dfbe096b72fdc3f2b93770782f2f118a0053fb44a9874f4a35a6" "b9ca9dec91058ca664637837f9b82465a21de915f0c14be80120e95e9f8bf094" "72f53e3ae4ad4aa94ea78e38b3c2ffcd47952a06a0051c662848671a431a54d2" "5d6c78a27cdfcf6a62d2e6ad4f0430ba83916faad912955c83cb132af8d5cb3a" "278c3017111682d5354d3c5e5b78de73859d0d380651153f3e0ada7a493b82d5" "1eaf6386dde80a6cd8b74052b26c9be3693a8b052c20061ecc0f4b845526f5d2" "2193ba7060ddc24c43421612c33099cfb2256682f0a11b60319e9dad8a0bd004" "2dbd3d8aa7985074f61ed6ea3677032c36ad3da3b54e582fc387ffef68d4249d" "082c85e8e91f21ff1a1a73c2590a3642896e3aff292975b38efaccb7ac17e662" "a40781d7582c3d567b5bfb6fe282e54701d107fca02db4a190511a94367b885c" "725a6fcb02bdab98e438a4da5081d7737ca4620cf8da3a0a5aa326b93af109aa" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)
