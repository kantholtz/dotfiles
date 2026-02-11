;;; ktz-menu.el --- Transient menu for ktz's convenience -*-

;;; Code:

(require 'transient)
;; tbd: this changes the transient behaviour globally...
(transient-bind-q-to-quit)

(transient-define-prefix ktz-menu--bookmarks ()
  "Bookmarking Operations"
  ["Bookmarks"
   ("s" "set" bookmark-set)
   ("j" "jump" bookmark-jump)
   ("l" "list" bookmark-bmenu-list)])


(defconst ktz-menu--indent "      ")

(defun ktz-menu--header ()
  "The banner"
  (propertize
   (concat ktz-menu--indent "KANTHOLTZ' EMACS\n")
   'face
   '(:weight bold (:box (:line-width (0 . 2) :color "red")))))


(defun ktz-menu--suffixes ()
  "Creates the main menu entries"
  ;; a list of group vectors where each suffix is a list
  `([,(ktz-menu--header)
     ("b" "bookmarks" ktz-menu--bookmarks)
     ("c" "configuration" ktz-configuration)
     ("r" "ripgrep" rg)
     ,@(when (member 'ide ktz-modules)
         '(("i" "ide" ktz-menu--ide)))
     ,@(when (member 'llm ktz-modules)
         '(("l" "llm" ktz-menu--llm)))]))


(transient-define-prefix ktz-menu ()
  "Interactively assembled main menu."
  [""
   ;; --
   :class transient-columns
   :setup-children
   (lambda (_)
     ;; the result of parsing here will be a group
     (transient-parse-suffixes
      'ktz-menu
      (ktz-menu--suffixes)))])


(global-set-key (kbd "<f2>") #'ktz-menu)
(global-set-key (kbd "C-<f2>") #'ktz-menu)
(global-set-key (kbd "C-`") #'ktz-menu)


;; ,(when ktz-menu--test ("t" "test" ktz-configuration))

(transient-define-prefix ktz-menu ()
  "KTZ Main Menu"
  [:class transient-columns
          :setup-children
          (lambda (_)
            (transient-parse-suffixes
             'ktz-menu
             (ktz-menu--suffixes)))])


;; ["Commands"
;;  ("b" "bookmarks" ktz-menu--bookmarks)
;;  ("c" "configuration" ktz-configuration)])


(provide 'ktz-menu)
;;; ktz-menu.el ends here
