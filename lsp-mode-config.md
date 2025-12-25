# LSP-Mode Configuration

## Current Issues in Your Config

Your current LSP configuration has several suboptimal settings compared to the top configurations:

1. **Conflicting completion providers**: You set `lsp-completion-provider :none` but still use LSP completion
2. **Missing orderless integration**: Basic setup but not optimized for LSP
3. **Performance issues**: Some settings could be better optimized
4. **LSP-UI conflicts**: Some settings might interfere with Corfu

## Optimized LSP-Mode Configuration

Based on analysis of `materusPL/nixos-config` and `stsquad/my-emacs-stuff`:

```elisp
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-completion-mode . my/lsp-mode-setup-completion)
  :custom
  ;; === COMPLETION SETTINGS ===
  ;; Use CAPF (completion-at-point-functions) instead of LSP's built-in completion
  (lsp-completion-provider :none)  ; Let Corfu handle completion via CAPF
  (lsp-completion-show-detail t)    ; Show completion item details
  (lsp-completion-show-kind t)      ; Show completion item kinds
  
  ;; === PERFORMANCE OPTIMIZATIONS ===
  (lsp-idle-delay 0.5)              ; Wait 0.5s before updating
  (lsp-log-io nil)                  ; Disable IO logging for performance
  (lsp-file-watch-threshold 2000)   ; Don't watch too many files
  (lsp-enable-file-watchers nil)    ; Disable file watchers (can be resource heavy)
  (lsp-response-timeout 30)         ; Reasonable timeout
  (lsp-keep-workspace-alive nil)    ; Don't keep workspace alive when no buffers
  
  ;; === LSP-BOOSTER INTEGRATION ===
  ;; Enable emacs-lsp-booster if available (significant performance boost)
  (lsp-use-plists t)                ; Required for lsp-booster (use plists instead of hash tables)
  
  ;; === UI SETTINGS ===
  (lsp-keymap-prefix "C-c l")       ; Prefix for LSP commands
  (lsp-enable-which-key-integration t)  ; Show keybindings in which-key
  (lsp-enable-snippet t)            ; Enable snippet support
  (lsp-headerline-breadcrumb-enable nil)  ; Disable breadcrumb (can be distracting)
  (lsp-signature-auto-activate nil) ; Don't show signature automatically (use manual)
  (lsp-signature-render-documentation nil)  ; Don't render doc in signature
  
  :config
  ;; === LSP-BOOSTER SETUP (from materusPL config) ===
  ;; This provides significant performance improvements for LSP
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)
       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))

  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)

  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)
               (not (file-remote-p default-directory))
               lsp-use-plists
               (not (functionp 'json-rpc-connection))
               (executable-find "emacs-lsp-booster"))
          (progn
            (when-let* ((command-from-exec-path (executable-find (car orig-result))))
              (setcar orig-result command-from-exec-path))
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))
        
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))

;; === COMPLETION INTEGRATION FUNCTION ===
(defun my/lsp-mode-setup-completion ()
  "Setup completion for LSP mode with orderless."
  ;; Configure orderless for LSP completion
  (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
        '(orderless))
  
  ;; Optional: Add flex matching for first word (from stsquad config)
  (add-hook 'orderless-style-dispatchers #'my/orderless-dispatch-flex-first nil 'local)
  
  ;; Enhance completion-at-point with Cape capf-buster (prevents hanging)
  (setq-local completion-at-point-functions
              (list (cape-capf-buster #'lsp-completion-at-point))))

(defun my/orderless-dispatch-flex-first (_pattern index _total)
  "Use flex matching for the first completion component."
  (and (eq index 0) 'orderless-flex))
```

## Key Improvements

1. **LSP-Booster Integration**: Massive performance boost using bytecode
2. **Proper CAPF Setup**: Clean integration with Corfu via completion-at-point
3. **Optimized Performance**: Better timeout and resource management
4. **Cape Integration**: Prevents completion hangs with capf-buster
5. **Orderless Integration**: Proper flex matching for LSP completions

## Language-Specific LSP Setup Example (Go)

```elisp
(use-package go-mode
  :mode "\\.go\\'"
  :hook ((go-mode . lsp-deferred)
         (before-save . lsp-format-buffer)
         (before-save . lsp-organize-imports))
  :config
  ;; Go-specific LSP settings
  (with-eval-after-load 'lsp-mode
    (lsp-register-custom-settings
     '(("gopls.gofumpt" t t)
       ("gopls.staticcheck" t t)))))
```

This configuration is based on the most performant setups from the top GitHub configurations and will work seamlessly with your Corfu setup.