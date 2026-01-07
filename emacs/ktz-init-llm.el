;;; ktz-llm.el --- LLM integration in Emacs -*-

;;; Code:

(require 'auth-source) ;; gptel auth
(require 'transient)


;; this heavily relies on the (internal) gptel--known-backends
;; variable and may break if this structure is changed in the
;; future, see therefore:
;;   - C-h v gptel--known-backends
;;   - C-h f gptel-backend-models
(defun ktz--init-llm ()

  ;; GPTEL: LLM integration
  (use-package gptel
    :config
    ;; disable default OpenAI backend
    ;; https://github.com/karthink/gptel/issues/649
    (setq gptel--known-backends nil)

    ;; configure desired backends and models manually
    (setq gptel-expert-commands t
          gptel-model   'anthropic/claude-opus-4.5
          gptel-backend  (gptel-make-openai "OpenRouter" ; any name
                           :host "openrouter.ai"
                           :endpoint "/api/v1/chat/completions"
                           :stream t
                           :key #'ktz-openrouter-api-key
                           :models '(openai/gpt-4.1
                                     anthropic/claude-haiku-4.5  ;; small/fast
                                     anthropic/claude-sonnet-4.5 ;; normal/medium
                                     anthropic/claude-opus-4.5 ;; premium/medium
                                     google/gemini-3-pro-preview))
          gptel-org-branching-context t))  ;; only org lineage as context


  (defun ktz-llm--gptel-backends-to-alist (backends)
    "Transforms gptel--knwon-backends into an alist of (name . models)."
    (mapcar
     (lambda (backend)
       (let* ((name (car backend))
              (struct (cdr backend))
              (models (gptel-backend-models struct)))
         (cons name models)))
     backends))


  ;; tbd: at some point, hard-code most used model keys?
  (defun ktz-llm--generate-transient-keys ()
    "Generate a list of transient key bindings."
    (append (mapcar #'number-to-string (number-sequence 1 9))
            (mapcar #'char-to-string (number-sequence ?a ?z))
            (mapcar #'char-to-string (number-sequence ?A ?Z))))


  (defun ktz-llm--switch-model (backend model)
    "Stuff"
    (message "switching to %s %s" backend model))

  (transient-define-prefix ktz-menu--llm-models ()
    "LLM backend and model selector."
    [;;
     :class transient-columns
     :setup-children
     (lambda (_)
       (transient-parse-suffixes
        'ktz-llm--menu
        (let ((keys (ktz-llm--generate-transient-keys)))
          ;; transform all backends to columns
          (mapcar
           (lambda (backend-def)
             (let* ((backend (car backend-def))
                    (models (cdr backend-def)))
               ;; create a vector (which represents a column)
               (vconcat
                ;; with the backend's name as heading
                `[,backend]
                ;; and map all model definitions to a list
                (mapcar
                 ;; where each model
                 (lambda (model)
                   ;; is assigned a list: (key name function)
                   (list (pop keys) ;; key
                         (symbol-name model) ;; name
                         `(lambda () (interactive) ;; function
                            (ktz-llm--switch-model ',backend ',model))))
                 models))))
           ;; transform all backends to alist
           (ktz-llm--gptel-backends-to-alist gptel--known-backends)))))])


  ;; main entry point
  (transient-define-prefix ktz-menu--llm ()
    ["LLMs and GPTEL"
     ("m" "main menu" gptel-menu)
     ("s" "switch model" ktz-menu--llm-models)]))


(defun ktz-init-llm ()
  "Initialize llm related config manually"
  (interactive)
  (ktz--init-llm))

(provide 'ktz-init-llm)
