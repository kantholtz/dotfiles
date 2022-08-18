;; this modeline was adapted from
;; https://github.com/rougier/nano-emacs/blob/master/nano-modeline.el
;; on Feb. 19 2022 - thanks N Λ N O developers

;; [ status | name (primary) lsp breadcrumbs    secondary | item1 | item2 ]
(require 'subr-x)


;; ---


(defun vc-branch ()
  (if vc-mode
      (let ((backend (vc-backend buffer-file-name)))
        (concat "#" (substring-no-properties vc-mode
                                 (+ (if (eq backend 'Hg) 2 3) 2))))  nil))

(defun ktz-theme--mode-name ()
  (let ((name (if (listp mode-name) (car mode-name) mode-name)))
    (if (and (derived-mode-p 'python-mode) conda-env-current-name)
        (concat name " conda: " conda-env-current-name)
      name)))


;; From https://amitp.blogspot.com/2011/08/emacs-custom-mode-line.html
;; ----


(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat "…/" output)))
    output))

;; ---

(defun ktz-theme--modeline-compose (status name primary secondary)
  "Compose a string with provided information"

  ;; overwrite box layout until i figure out how to include lsp breadcrumbs
  (set-face-attribute
   'nano-face-header-faded nil
   :background nil
   :foreground nano-color-faded
   :weight 'regular)

  (set-face-attribute
   'nano-face-header-popout nil
   :background nil
   :foreground nano-color-popout
   :weight 'regular)

  (set-face-attribute
   'nano-face-header-salient nil
   :background nil
   :foreground nano-color-salient
   :weight 'regular)

  (set-face-attribute
   'nano-face-header-critical nil
   :background nano-color-critical
   :foreground "white"
   :weight 'bold)

  (let*
      ((char-width    (window-font-width nil 'header-line))
       (window        (get-buffer-window (current-buffer)))
       (space-up       +0.15)
       (space-down     -0.20)

       ;; left status box
	     (prefix
        (cond

         ((string= status "**")
			    (propertize (if (window-dedicated-p) " -- " " ** ")
                      'face 'nano-face-header-critical))

         ((string= status "RO")
			    (propertize (if (window-dedicated-p) " -- " "[RO]")
                      'face 'nano-face-header-popout))

         ((string= status "RW")
			    (propertize (if (window-dedicated-p) " -- " "[RW]")
                      'face 'nano-face-header-faded))

         (t (propertize status 'face 'nano-face-header-popout))))

       (prefix (concat " " prefix))

       ;; buffer name
       (left
        (concat
         (propertize " "  'face 'nano-face-header-default
                     'display `(raise ,space-up))
         (propertize name 'face 'nano-face-header-strong)
         (propertize " "  'face 'nano-face-header-default
                     'display `(raise ,space-down))
		     (propertize primary 'face 'nano-face-header-default)))

       ;; optional information
       (right (concat " " secondary " ")))

    ;; assemble (cannot align info right because lsp
    ;; breadcrumbs are not included atm)
    (concat
     prefix left
	   (propertize
      right 'face
      `(:inherit nano-face-header-default
                 :foreground ,nano-color-faded)))))

;; ----

(defun ktz-theme--modeline-mu4e-dashboard-mode-p ()
  (bound-and-true-p mu4e-dashboard-mode))

(defun ktz-theme--modeline-mu4e-dashboard-mode ()
  (ktz-theme--modeline-compose (ktz-theme--modeline-status)
                         "Mail"
                         (ktz-theme--modeline-mu4e-context)
                         (format "%d messages" (plist-get mu4e~server-props :doccount))
                         ))

;; ----


;; since the EIN library itself is constantly re-rendering the notebook, and thus
;; re-setting the header-line-format, we cannot use the ktz-theme--modeline function to set
;; the header format in a notebook buffer.  Fortunately, EIN exposes the
;; ein:header-line-format variable for just this purpose.

(with-eval-after-load 'ein
  (defun ktz-theme--modeline-ein-notebook-mode ()
    (let ((buffer-name (format-mode-line "%b")))
      (ktz-theme--modeline-compose (if (ein:notebook-modified-p) "**" "RW")
                             buffer-name
                             ""
                             (ein:header-line))))
  (setq ein:header-line-format '((:eval (ktz-theme--modeline-ein-notebook-mode)))))

;; ----

(defun ktz-theme--modeline-elfeed-search-mode-p ()
  (derived-mode-p 'elfeed-search-mode))

(defun ktz-theme--modeline-elfeed-search-mode ()
  (ktz-theme--modeline-compose (ktz-theme--modeline-status)
                         "Elfeed"
                         (concat "(" (elfeed-search--header)  ")")
                         ""))

;; Elfeed (regular header)
(with-eval-after-load 'elfeed
  (defun elfeed-setup-header ()
    (setq header-line-format (default-value 'header-line-format)))
  (setq elfeed-search-header-function #'elfeed-setup-header))

;; ----

(defun ktz-theme--modeline-elfeed-show-mode-p ()
  (derived-mode-p 'elfeed-show-mode))

(defun ktz-theme--modeline-elfeed-show-mode ()
  (let* ((title        (elfeed-entry-title elfeed-show-entry))
         (tags         (elfeed-entry-tags elfeed-show-entry))
         (tags-str     (mapconcat #'symbol-name tags ", "))
         (date         (seconds-to-time (elfeed-entry-date elfeed-show-entry)))
         (feed         (elfeed-entry-feed elfeed-show-entry))
         (feed-title   (plist-get (elfeed-feed-meta feed) :title))
         (entry-author (elfeed-meta elfeed-show-entry :author)))
    (ktz-theme--modeline-compose (ktz-theme--modeline-status)
                           (s-truncate 40 title "…")
                           (concat "(" tags-str ")")
                           feed-title)))

;; ----

(defun ktz-theme--modeline-calendar-mode-p ()
  (derived-mode-p 'calendar-mode))

(defun ktz-theme--modeline-calendar-mode () "")

;; Calendar (no header, only overline)
(with-eval-after-load 'calendar
  (defun calendar-setup-header ()
    (setq header-line-format "")
    (face-remap-add-relative
     'header-line `(:overline ,(face-foreground 'default)
                    :height 0.5
                    :background ,(face-background 'default))))
  (add-hook 'calendar-initial-window-hook #'calendar-setup-header)

  ;; From https://emacs.stackexchange.com/questions/45650
  (add-to-list 'display-buffer-alist
               `(,(rx string-start "*Calendar*" string-end)
                 (display-buffer-below-selected))))

;; ----

(defun ktz-theme--modeline-org-capture-mode-p ()
  (bound-and-true-p org-capture-mode))

(defun ktz-theme--modeline-org-capture-mode ()
  (ktz-theme--modeline-compose (ktz-theme--modeline-status)
                         "Capture"
                         "(org)"
                         ""))

(with-eval-after-load 'org-capture
  (defun org-capture-turn-off-header-line ()
    (setq-local header-line-format (default-value 'header-line-format))
    ;; (fit-window-to-buffer nil nil 8)
    ;; (face-remap-add-relative 'header-line '(:background "#ffffff"))
    (message nil))
  (add-hook 'org-capture-mode-hook
            #'org-capture-turn-off-header-line))

;; ----

(setq Info-use-header-line nil)
(defun ktz-theme--modeline-info-breadcrumbs ()
  (let ((nodes (Info-toc-nodes Info-current-file))
        (cnode Info-current-node)
	(node Info-current-node)
        (crumbs ())
        (depth Info-breadcrumbs-depth)
	line)
    (while  (> depth 0)
      (setq node (nth 1 (assoc node nodes)))
      (if node (push node crumbs))
      (setq depth (1- depth)))
    (setq crumbs (cons "Top" (if (member (pop crumbs) '(nil "Top"))
			         crumbs (cons nil crumbs))))
    (forward-line 1)
    (dolist (node crumbs)
      (let ((text
	     (if (not (equal node "Top")) node
	       (format "%s"
		       (if (stringp Info-current-file)
			   (file-name-sans-extension
			    (file-name-nondirectory Info-current-file))
			 Info-current-file)))))
	(setq line (concat line (if (null line) "" " > ")
                                (if (null node) "..." text)))))
    (if (and cnode (not (equal cnode "Top")))
        (setq line (concat line (if (null line) "" " > ") cnode)))
    line))

(defun ktz-theme--modeline-info-mode-p ()
  (derived-mode-p 'Info-mode))

(defun ktz-theme--modeline-info-mode ()
  (ktz-theme--modeline-compose (ktz-theme--modeline-status)
                         "Info"
                         (concat "("
                                 (ktz-theme--modeline-info-breadcrumbs)
                                 ")")
                         ""))

;; ----

(defun ktz-theme--modeline-org-agenda-mode-p ()
  (derived-mode-p 'org-agenda-mode))

(defun ktz-theme--modeline-org-agenda-mode ()
  (ktz-theme--modeline-compose (ktz-theme--modeline-status)
                         "Agenda"
                         ""
                         (format-time-string "%A %-e %B %Y")))

;; ----

(defun ktz-theme--modeline-term-mode-p ()
  (derived-mode-p 'term-mode))

(defun ktz-theme--modeline-vterm-mode-p ()
  (derived-mode-p 'vterm-mode))

(defun ktz-theme--modeline-term-mode ()
  (ktz-theme--modeline-compose " >_ "
                         "Terminal"
                         (concat "(" shell-file-name ")")
                         (shorten-directory default-directory 32)))

;; ----

(defun ktz-theme--modeline-mu4e-main-mode-p ()
  (derived-mode-p 'mu4e-main-mode))

(defun ktz-theme--modeline-mu4e-main-mode ()
  (ktz-theme--modeline-compose (ktz-theme--modeline-status)
                         "Mail"
                         (ktz-theme--modeline-mu4e-context)
                         (format-time-string "%A %d %B %Y, %H:%M")))

;; ----

(defun ktz-theme--modeline-mu4e-headers-mode-p ()
  (derived-mode-p 'mu4e-headers-mode))

(defun ktz-theme--modeline-mu4e-headers-mode ()
  (ktz-theme--modeline-compose (ktz-theme--modeline-status)
                         (mu4e~quote-for-modeline mu4e~headers-last-query)
                         ""
                         ""))

(with-eval-after-load 'mu4e
  (defun mu4e~header-line-format () (ktz-theme-modeline)))

;; ----

(setq mu4e-modeline-max-width 72)

(defun ktz-theme--modeline-mu4e-view-mode-p ()
  (derived-mode-p 'mu4e-view-mode))

(defun ktz-theme--modeline-mu4e-view-mode ()
  (let* ((msg     (mu4e-message-at-point))
         (subject (mu4e-message-field msg :subject))
         (from    (mu4e~headers-contact-str (mu4e-message-field msg :from)))
         (date     (mu4e-message-field msg :date)))
    (ktz-theme--modeline-compose (ktz-theme--modeline-status)
                           (s-truncate 40 subject "…")
                           ""
                           from)))

(defun ktz-theme--modeline-mu4e-view-hook ()
  (setq header-line-format "%-")
  (face-remap-add-relative 'header-line
                           '(:background "#ffffff"
                                         :underline nil
                                         :box nil
                                         :height 1.0)))
(add-hook 'mu4e-view-mode-hook #'ktz-theme--modeline-mu4e-view-hook)


;; ----

(defun ktz-theme--modeline-ktz-theme--help-mode-p ()
  (derived-mode-p 'ktz-theme--help-mode))

(defun ktz-theme--modeline-ktz-theme--help-mode ()
  (ktz-theme--modeline-compose (ktz-theme--modeline-status)
                         "GNU Emacs / N Λ N O"
                         "(help)"
                         ""))

;; ----

(defun ktz-theme--modeline-message-mode-p ()
  (derived-mode-p 'message-mode))

(defun ktz-theme--modeline-message-mode ()
  (ktz-theme--modeline-compose (ktz-theme--modeline-status)
                         "Message" "(draft)" ""))


;; ----

(setq org-mode-line-string nil)
(with-eval-after-load 'org-clock
  (add-hook 'org-clock-out-hook
            #'(lambda () (setq org-mode-line-string nil)
                (force-mode-line-update))))

(defun ktz-theme--modeline-org-clock-mode-p ()
  org-mode-line-string)

(defun ktz-theme--modeline-org-clock-mode ()
    (let ((buffer-name (format-mode-line "%b"))
          (mode-name   (ktz-theme--mode-name))
          (branch      (vc-branch))
          (position    (format-mode-line "%l:%c")))
      (ktz-theme--modeline-compose (ktz-theme--modeline-status)
                             buffer-name
                             (concat "(" mode-name
                                     (if branch (concat ", "
                                             (propertize branch 'face 'italic)))
                                     ")" )
                             org-mode-line-string)))

;; ----

(defun ktz-theme--modeline-docview-mode-p ()
  (derived-mode-p 'doc-view-mode))

(defun ktz-theme--modeline-docview-mode ()
  (let ((buffer-name (format-mode-line "%b"))
	(mode-name   (ktz-theme--mode-name))
	(branch      (vc-branch))
	(page-number (concat
		      (number-to-string (doc-view-current-page)) "/"
		      (or (ignore-errors
			    (number-to-string (doc-view-last-page-number)))
			  "???"))))
    (ktz-theme--modeline-compose
     (ktz-theme--modeline-status)
     buffer-name
     (concat "(" mode-name
	     (if branch (concat ", "
				(propertize branch 'face 'italic)))
	     ")" )
     page-number)))

;; ----

(defun ktz-theme--modeline-pdf-view-mode-p ()
  (derived-mode-p 'pdf-view-mode))

(defun ktz-theme--modeline-pdf-view-mode ()
  (let ((buffer-name (format-mode-line "%b"))
	(mode-name   (ktz-theme--mode-name))
	(branch      (vc-branch))
	(page-number (concat
		      (number-to-string (pdf-view-current-page)) "/"
		      (or (ignore-errors
			    (number-to-string (pdf-cache-number-of-pages)))
			  "???"))))
    (ktz-theme--modeline-compose
     "RW"
     buffer-name
     (concat "(" mode-name
	     (if branch (concat ", "
				(propertize branch 'face 'italic)))
	     ")" )
     page-number)))

;; ----

(defun buffer-menu-mode-header-line ()
  (face-remap-add-relative
   'header-line `(:background ,(face-background 'nano-face-subtle))))
(add-hook 'Buffer-menu-mode-hook
          #'buffer-menu-mode-header-line)

;; ----

(defun ktz-theme--modeline-completion-list-mode-p ()
  (derived-mode-p 'completion-list-mode))

(defun ktz-theme--modeline-completion-list-mode ()
    (let ((buffer-name (format-mode-line "%b"))
          (mode-name   (ktz-theme--mode-name))
          (position    (format-mode-line "%l:%c")))

      (ktz-theme--modeline-compose (ktz-theme--modeline-status)
                             buffer-name "" position)))
;; ----

(with-eval-after-load 'deft
  (defun deft-print-header ()
    (force-mode-line-update)
    (widget-insert "\n")))

(defun ktz-theme--modeline-deft-mode-p ()
  (derived-mode-p 'deft-mode))

(defun ktz-theme--modeline-deft-mode ()
  (let ((prefix " DEFT ")
        (primary "Notes")
        (filter  (if deft-filter-regexp
                     (deft-whole-filter-regexp) "<filter>"))
        (matches (if deft-filter-regexp
                     (format "%d matches" (length deft-current-files))
                   (format "%d notes" (length deft-all-files)))))
    (ktz-theme--modeline-compose " DEFT "
                           primary filter matches)))


;; ----

(defun ktz-theme--modeline-prog-mode-p ()
  (derived-mode-p 'prog-mode))

(defun ktz-theme--modeline-text-mode-p ()
  (derived-mode-p 'text-mode))

(defun ktz-theme--modeline-default-mode ()
    (let ((buffer-name (format-mode-line "%b"))
          (mode-name   (ktz-theme--mode-name))
          (branch      (vc-branch))
          (position    (format-mode-line "%l:%c")))
      (ktz-theme--modeline-compose (ktz-theme--modeline-status)
                             buffer-name
                             (concat "(" mode-name
                                     (if branch (concat ", "
                                            (propertize branch 'face 'italic)))
                                     ")" )
                             position)))

;; ----

(defun ktz-theme--modeline-status ()
  "Return buffer status: read-only (RO), modified (**) or read-write (RW)"

  (let ((read-only   buffer-read-only)
        (modified    (and buffer-file-name (buffer-modified-p))))
    (cond (modified  "**") (read-only "RO") (t "RW"))))

;; ----

(defun ktz-theme--modeline-mu4e-context ()
  "Return the current mu4e context as a non propertized string."

  (if (> (length (mu4e-context-label)) 0)
      (concat "(" (substring-no-properties (mu4e-context-label) 1 -1) ")")
    "(none)"))


;; ----

(defun ktz-theme-modeline ()
  "Install a header line whose content is dependend on the major mode"
  (interactive)
  (setq-default header-line-format
  '((:eval
     (cond ((ktz-theme--modeline-prog-mode-p)            (ktz-theme--modeline-default-mode))
           ((ktz-theme--modeline-message-mode-p)         (ktz-theme--modeline-message-mode))
           ((ktz-theme--modeline-elfeed-search-mode-p)   (ktz-theme--modeline-elfeed-search-mode))
           ((ktz-theme--modeline-elfeed-show-mode-p)     (ktz-theme--modeline-elfeed-show-mode))
           ((ktz-theme--modeline-deft-mode-p)            (ktz-theme--modeline-deft-mode))
           ((ktz-theme--modeline-info-mode-p)            (ktz-theme--modeline-info-mode))
           ((ktz-theme--modeline-calendar-mode-p)        (ktz-theme--modeline-calendar-mode))
           ((ktz-theme--modeline-org-capture-mode-p)     (ktz-theme--modeline-org-capture-mode))
           ((ktz-theme--modeline-org-agenda-mode-p)      (ktz-theme--modeline-org-agenda-mode))
           ((ktz-theme--modeline-org-clock-mode-p)       (ktz-theme--modeline-org-clock-mode))
           ((ktz-theme--modeline-term-mode-p)            (ktz-theme--modeline-term-mode))
           ((ktz-theme--modeline-vterm-mode-p)           (ktz-theme--modeline-term-mode))
           ((ktz-theme--modeline-mu4e-dashboard-mode-p)  (ktz-theme--modeline-mu4e-dashboard-mode))
           ((ktz-theme--modeline-mu4e-main-mode-p)       (ktz-theme--modeline-mu4e-main-mode))
           ((ktz-theme--modeline-mu4e-headers-mode-p)    (ktz-theme--modeline-mu4e-headers-mode))
;;         ((ktz-theme--modeline-mu4e-view-mode-p)       (ktz-theme--modeline-mu4e-view-mode))
           ((ktz-theme--modeline-text-mode-p)            (ktz-theme--modeline-default-mode))
           ((ktz-theme--modeline-pdf-view-mode-p)        (ktz-theme--modeline-pdf-view-mode))
	   ((ktz-theme--modeline-docview-mode-p)         (ktz-theme--modeline-docview-mode))
	   ((ktz-theme--modeline-completion-list-mode-p) (ktz-theme--modeline-completion-list-mode))
           ((ktz-theme--modeline-ktz-theme--help-mode-p)       (ktz-theme--modeline-ktz-theme--help-mode))
           (t                                      (ktz-theme--modeline-default-mode)))))))

;; ----

(defun ktz-theme--modeline-update-windows ()
  "Modify the mode line depending on the presence of a window
below or a buffer local variable 'no-mode-line'."
  (dolist (window (window-list))
    (with-selected-window window
	  (with-current-buffer (window-buffer window)
        (if (or (not (boundp 'no-mode-line)) (not no-mode-line))
            (set-window-parameter window 'mode-line-format
                                  (cond ((not mode-line-format) 'none)
                                        ((one-window-p t 'visible) (list ""))
                                        ((eq (window-in-direction 'below) (minibuffer-window)) (list ""))
                                        ((not (window-in-direction 'below)) (list ""))
                                        (t 'none))))))))

(add-hook 'window-configuration-change-hook 'ktz-theme--modeline-update-windows)

(setq eshell-status-in-modeline nil)
;; (setq-default mode-line-format (list "%-"))
(setq-default mode-line-format "")
(ktz-theme-modeline)

(provide 'ktz-theme-modeline)
