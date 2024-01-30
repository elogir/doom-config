;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq doom-font (font-spec :family "FiraMono Nerd Font" :size 18 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "FiraMono Nerd Font" :size 19))

(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

(gud-tooltip-mode t)

(winner-mode t)
(map! :map winner-mode-map
      :leader
      "<left>" #'winner-undo
      "<right>" #'winner-redo)

(use-package! crux
  :bind (("M-o" . #'crux-smart-open-line-above)
	 ("C-o" . #'crux-smart-open-line)))

(use-package! goto-line-preview
  :bind ([remap goto-line] . #'goto-line-preview))

(map! :leader "SPC" #'yas-expand)

(use-package! vundo
  :bind (("C-x u" . #'vundo)))

(use-package! projectile
  :custom
  (projectile-track-known-projects-automatically nil)
  (projectile-auto-discover nil))

(use-package! org-modern
  :delight
  :hook ((org-mode . org-modern-mode)
	 (org-agenda-finalize . org-modern-agenda)))

(use-package! company
  :bind ([remap complete-symbol] . #'company-complete))

(map! "C-|" (lambda () (interactive)
              (duplicate-line)
              (forward-line)
              (doom/forward-to-last-non-comment-or-eol)))

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


(map! :map c-mode-map
      "C-c C-c" #'my-projectile-compile-project
      "C-c C-v" #'my-projectile-run-project)

(setq auth-sources '("~/.authinfo"))

(use-package! forge
  :after magit)
