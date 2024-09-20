;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq doom-font (font-spec :family "FiraMono Nerd Font" :size 18 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "FiraMono Nerd Font" :size 19))

(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

(setq auth-sources '("~/.authinfo"))

(load-theme 'doom-solarized-light t)

(winner-mode t)
(global-visual-line-mode t)

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
  :bind (:map global-map
              (([remap complete-symbol] . #'company-complete)
               ("M-`" . #'company-complete))))


(use-package! lsp-mode
  :hook ((rjsx-mode . lsp-mode)
         (mhtml-mode . lsp-mode)
         (lsp-mode . lsp-enable-which-key-integration))
  :bind (:map lsp-mode-map
              ("M-RET" . #'lsp-execute-code-action))
  :custom
  (lsp-headerline-breadcrumb-enable t)
  (lsp-inlay-hint-enable t)
  (lsp-javascript-display-enum-member-value-hints t)
  (lsp-javascript-display-parameter-name-hints t)
  (lsp-javascript-display-parameter-type-hints t)
  (lsp-javascript-display-property-declaration-type-hints t)
  (lsp-javascript-display-return-type-hints t)
  (lsp-javascript-display-parameter-name-hints "literals")
  (lsp-javascript-display-variable-type-hints t)
  :commands lsp)

(use-package! projectile)

(use-package! yasnippet
  :hook ((lsp-mode . yas-minor-mode) (org-mode . yas-minor-mode))
  :bind (:map yas-keymap
	      ("C-c SPC" . #'yas-expand)
              ("TAB" . #'yas-next-field))
  :config
  (yas-reload-all))

(use-package rjsx-mode
  :hook (rjsx-mode . subword-mode))
