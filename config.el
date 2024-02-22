;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq doom-font (font-spec :family "FiraMono Nerd Font" :size 18 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "FiraMono Nerd Font" :size 19))

(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

(setq auth-sources '("~/.authinfo"))

(winner-mode t)

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

(map! :map global-map
      :leader
      "<left>" #'winner-undo
      "<right>" #'winner-redo)

(map! :leader "SPC" #'yas-expand)

(map! "C-|" (lambda () (interactive)
              (duplicate-line)
              (forward-line)
              (doom/forward-to-last-non-comment-or-eol)))

(map! "M-0" #'treemacs-select-window)

(map! :leader "g" #'gptel-menu)

;;; Packages

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

(use-package! projectile
  :custom
  (projectile-track-known-projects-automatically nil)
  (projectile-auto-discover nil)
  :config
  (push ".class" projectile-globally-ignored-file-suffixes))

(use-package! org-modern
  :delight
  :hook ((org-mode . org-modern-mode)
	 (org-agenda-finalize . org-modern-agenda)))

(use-package! company
  :bind (:map global-map
              ([remap complete-symbol] . #'company-complete))
  :custom
  (company-idle-delay nil)
  (company-frontends '(company-pseudo-tooltip-frontend))
  (company-tooltip-align-annotations t)
  (company-tooltip-limit 6)
  (company-tooltip-minimum 6))

(use-package! lsp-java
  :init
  (setq lombok-library-path (concat doom-data-dir "lombok.jar"))
  :bind
  (:map lsp-mode-map
        (("M-RET" . #'lsp-execute-code-action)
         ("C-h h" . #'lsp-ui-doc-glance)
         ("C-c C-c" . #'my-projectile-compile-project)
         ("C-c C-v" . #'my-projectile-run-project)))
  :custom
  (lsp-java-vmargs '("-XX:+UseParallelGC" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Dsun.zip.disableMemoryMapping=true" "-Xmx4G" "-Xms100m"))
  (lsp-java-save-actions-organize-imports t)
  :config
  (unless (file-exists-p lombok-library-path)
    (url-copy-file "https://projectlombok.org/downloads/lombok.jar" lombok-library-path))
  (push (concat "-javaagent:" (expand-file-name lombok-library-path)) lsp-java-vmargs))

(use-package! treemacs
  :custom
  (treemacs-collapse-dirs 4)
  (treemacs-indentation 1))

(use-package! gptel
  :custom
  (gptel-api-key "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")
  (gptel-default-mode 'org-mode)
  :config
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response))

(add-to-list 'load-path "/home/rigole/Documents/Git-Repos/gptel-extensions.el")
(require 'gptel-extensions)

(use-package! v-mode
  :config
  :bind
  (:map v-mode-map
        ("M-z" . v-menu))
  :mode ("\\(\\.v?v\\|\\.vsh\\)$" . 'v-mode))

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration
               '(v-mode . "v"))

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "v-analyzer")
                    :activation-fn (lsp-activate-on "v")
                    :server-id 'v-analyzer)))
