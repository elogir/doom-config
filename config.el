;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq doom-font (font-spec :family "FiraMono Nerd Font" :size 18 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "FiraMono Nerd Font" :size 19))

(setq display-line-numbers-type 'relative)
(setq org-directory "~/org/")
(setq auth-sources '("~/.authinfo"))

(load-theme 'doom-one t)
;; (setq doom-theme nil)

(winner-mode t)
(global-visual-line-mode t)
(global-auto-revert-mode t)

(use-package! ctrlf
  :config (ctrlf-mode +1))

(defun my-projectile-run-project (&optional prompt)
  (interactive "P")
  (let ((compilation-read-command
         (or (not (projectile-run-command (projectile-compilation-dir)))
             prompt)))
    (projectile-run-project prompt)))

(defun my-projectile-compile-project (&optional prompt)
  (interactive "P")
  (let ((compilation-read-command
         (or (not (projectile-compilation-command (projectile-compilation-dir)))
             prompt)))
    (projectile-compile-project prompt)))

;;; Keybindings

(map! :map c-mode-map
      "C-c C-c" #'my-projectile-compile-project
      "C-c C-v" #'my-projectile-run-project)

(map! :map c++-mode-map
      "C-c C-c" #'my-projectile-compile-project
      "C-c C-v" #'my-projectile-run-project)

(map! :map rjsx-mode-map
      "C-c C-c" #'my-projectile-compile-project
      "C-c C-v" #'my-projectile-run-project)

(map! :map v-mode-map
      "C-c C-c" #'my-projectile-compile-project
      "C-c C-v" #'my-projectile-run-project)

(map! :map dart-mode-map
      "C-c C-c" #'my-projectile-compile-project
      "C-c C-v" #'my-projectile-run-project)

(map! :map global-map
      :leader
      "<left>" #'winner-undo
      "<right>" #'winner-redo)

(map! :leader "SPC" #'yas-expand)
(map! "C-x O" #'other-frame)

(map! "C-|" (lambda () (interactive)
              (duplicate-line)
              (forward-line)
              (doom/forward-to-last-non-comment-or-eol)))

;;; Packages

(use-package ansi-color
  :hook (compilation-filter . ansi-color-compilation-filter))

(use-package! crux
  :bind (:map global-map
              ("M-o" . #'crux-smart-open-line-above)
	      ("C-o" . #'crux-smart-open-line)))

(use-package! goto-line-preview
  :bind (:map global-map
              ([remap goto-line] . #'goto-line-preview)))

(use-package! vundo
  :bind (:map global-map
              ("C-x u" . #'vundo)))

(use-package! mhtml-mode
  :bind (:map mhtml-mode-map
              ("M-o" . nil)))

(use-package! projectile
  :custom
  (projectile-track-known-projects-automatically nil)
  (projectile-auto-discover nil)
  :bind
  ;; (:map projectile-mode-map
  ;;       ("C-c C-c" . #'my-projectile-compile-project)
  ;;       ("C-c C-v" . #'my-projectile-run-project))
  :config
  (push ".class" projectile-globally-ignored-file-suffixes))

(use-package! company
  :custom
  (company-idle-delay nil)
  :bind (:map global-map
              (([remap complete-symbol] . #'company-complete)
               ("M-`" . #'company-complete))))


(use-package! lsp-mode
  :hook ((rjsx-mode . lsp-mode)
         (js-ts-mode . lsp-mode)
         (v-mode . lsp-mode)
         (mhtml-mode . lsp-mode)
         (lsp-mode . lsp-enable-which-key-integration))
  :bind (:map lsp-mode-map
              ("M-RET" . #'lsp-execute-code-action))
  :custom
  ;; (lsp-signature-auto-activate t)
  (lsp-headerline-breadcrumb-enable t)
  (lsp-javascript-display-enum-member-value-hints t)
  (lsp-javascript-display-parameter-name-hints t)
  (lsp-javascript-display-parameter-type-hints t)
  (lsp-javascript-display-property-declaration-type-hints t)
  (lsp-javascript-display-return-type-hints t)
  (lsp-javascript-display-parameter-name-hints "literals")
  (lsp-javascript-display-variable-type-hints t))

(use-package! projectile
  :config
  (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")
  (add-to-list 'projectile-project-root-files-bottom-up "BUILD"))

(use-package! yasnippet
  :bind (:map global-map
	 ("C-c SPC" . #'yas-expand)
         ("C-c C-SPC" . #'yas-insert-snippet)
         :map yas-keymap
         ("TAB" . #'yas-next-field))
  :config
  (yas-reload-all)
  (yas-global-mode t))


(use-package! rjsx-mode
  :hook (rjsx-mode . subword-mode))

(use-package! js-ts-mode
  :hook (js-ts-mode . subword-mode))


(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '(v-mode . "vlang"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "v-analyzer")
                    :activation-fn (lsp-activate-on "vlang")
                    :server-id 'v-analyzer)))

(use-package! jsdoc
  :bind (:map js-ts-mode-map
              ("C-c C-d" . jsdoc)))

(use-package! js-ts-mode
  :mode ("\\.js$"))

(use-package! dart-mode
  :bind (:map dart-mode-map
              ("C-c C-c" . #'flutter-run-or-hot-reload)
              ("C-c C-v" . #'flutter-hot-restart))
  :custom
  (lsp-dart-flutter-widget-guides t)
  (subword-mode t))


(global-so-long-mode nil)
(global-org-modern-mode t)

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
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

;; (use-package! flycheck-grammalecte
;;   :init
;;   (setq flycheck-grammalecte-report-apos nil
;;         flycheck-grammalecte-report-esp nil
;;         flycheck-grammalecte-report-nbsp nil
;;         flycheck-grammalecte-report-grammar t
;;         flycheck-grammalecte-report-spellcheck nil
;;         flycheck-grammalecte-report-typo nil)
;;   :config
;;   (add-to-list 'flycheck-grammalecte-enabled-modes 'org-mode)
;;   (grammalecte-download-grammalecte)
;;   (flycheck-grammalecte-setup))

(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("org-plain-latex"
                 "\\documentclass{article}
           [NO-DEFAULT-PACKAGES]
           [PACKAGES]
           [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(require 'xterm-color)
(setq compilation-environment '("TERM=xterm-256color"))
(defun my/advice-compilation-filter (f proc string)
  (funcall f proc (xterm-color-filter string)))
(advice-add 'compilation-filter :around #'my/advice-compilation-filter)

(use-package! dart-mode
  :config
  (subword-mode))

(use-package! move-text
  :bind (:map global-map
              ("M-p" . #'move-text-up)
              ("M-n" . #'move-text-down)))

(use-package! dape
  :custom
  (dape-buffer-window-arrangement 'right)
  (dape-inlay-hints t)
  (dape-cwd-fn 'projectile-project-root)
  :config
  (dape-breakpoint-global-mode)
  (add-hook 'dape-display-source-hook 'pulse-momentary-highlight-one-line))

(use-package! org
  :custom
  (org-return-follows-link t))

(use-package! yasnippet-snippets
  :after yasnippet
  :ensure t)

(setq tramp-default-method "ssh")

(use-package! vterm
  :custom
  (vterm-tramp-shells '(("ssh" "/bin/bash") ("docker" "/bin/sh"))))

(use-package! gptel
  :bind (:map global-map
              ("C-c g r" . #'gptel-rewrite)
              ("C-c g c" . #'gptel-add)
              ("C-c g g" . #'gptel-menu))
  :config
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response)
  (setq
   gptel-model   'claude-3-sonnet
   gptel-backend (gptel-make-ollama "Kagi Assistant"
                   :host "localhost:8089"
                   :protocol "http"
                   :endpoint "/completions"
                   :stream nil
                   :models '(claude-3-sonnet claude-3-opus claude-3-haiku gpt-4o-mini gpt-4o mistral-nemo mistral-large gemini-pro llama-3-70b llama-3-405b qwen-qwq-32b nova-lite nova-pro deepseek deepseek-r1))))

(use-package! gptel-quick
  :bind (:map global-map
              ("C-c g q" . #'gptel-quick))
  :custom
  (gptel-quick-timeout 30)
  (gptel-quick-backend gptel-backend)
  (gptel-quick-model 'gpt-4o-mini))

(use-package! markdown-ts-mode
  :mode ("\\.md\\'" . markdown-ts-mode)
  :defer 't
  :config
  (add-to-list 'treesit-language-source-alist '(markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown/src"))
  (add-to-list 'treesit-language-source-alist '(markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown-inline/src")))

(use-package! ultra-scroll
  :config
  (ultra-scroll-mode 1))

(use-package! reddigg
  :custom
  (reddigg-subs '("emacs" "NoStupidQuestions" "FitnessFrance")))
