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

(use-package! realgud)
(use-package! realgud-lldb)

(add-to-list 'load-path "~/.config/emacs/site-lisp")
;; (load-library "doxygen-highlight.el")
(load-library "gendoxy.el")
(autoload 'tiger-mode "tiger" "Load tiger-mode" t)
(add-to-list 'auto-mode-alist '("\.ti[gh]$" . tiger-mode))

(defface doxygen-verbatim-face
  '((default :inherit default))
  "Face used to show Doxygen block regions"
  :group 'font-lock-faces)

(defface doxygen-match-face
  '((default :inherit default)
    (t :underline t))
  "Face used to show Doxygen region start end commands"
  :group 'font-lock-faces)

(defconst custom-font-lock-doc-comments
  `(
    (,(concat
       "\\(?:"
       "[\\@][a-z]+"
       "\\|"
       "[\\@]\\(?:\\\\\\|@\\|&\\|#\\|<\\|>\\|%\\|\"\\|\\.\\|::\\||\\|---?\\|~[a-z]*\\)"
       "\\)")
     0 ,c-doc-markup-face-name prepend nil)
    (,(concat
       "\\(?:"
       "[A-Za-z_0-9]+(\\([A-Za-z_0-9:&*, ]*)\\)?"
       "\\|"
       "\\(?:[A-Za-z_0-9]+\\|\\s-\\)\\(?:::\\|#\\)~?[A-Za-z_0-9]+(?\\(?:[A-Za-z_0-9:&*, \t]*)\\)?"
       "\\|"
       "[A-Za-z_0-9/]+\\.\\(?:cpp\\|cxx\\|cc\\|c\\|hpp\\|hxx\\|hh\\|h\\)"
       "\\)")
     0 font-lock-function-name-face prepend nil)
    ("https?://[^[:space:][:cntrl:]]+"
     0 font-lock-keyword-face prepend nil)
    (,(concat "</?\\sw"
              "\\("
              (concat "\\sw\\|\\s \\|[=\n\r*.:]\\|"
                      "\"[^\"]*\"\\|'[^']*'")
              "\\)*>")
     0 ,c-doc-markup-face-name prepend nil)
    ("[A-Za-z0-9.]+@[A-Za-z0-9_]+\\.[A-Za-z0-9_.]+"
     0 font-lock-keyword-face prepend nil)
    ("\"[^\"[:cntrl:]]+\""
     0 ,c-doc-face-name prepend nil)

    ("[^\\@]\\([\\@]f.+?[\\@]f\\$\\)"
     1 'doxygen-verbatim-face prepend nil)
    (,(concat
       "[^\\@]"
       "\\([\\@]\\(?:verbatim\\|endverbatim\\|code\\|endcode\\|f{\\|f\\[\\|f}\\|f]\\)\\)")
     1 'doxygen-match-face prepend nil)
    ))

(defconst custom-font-lock-keywords
  `((,(lambda (limit)
        (c-font-lock-doc-comments "/\\(//\\|\\*[\\*!][^\\*!]\\)"
            limit custom-font-lock-doc-comments)))))

(setq-default c-doc-comment-style (quote (custom)))

(setq apheleia-mode-alist '(("\\.ll\\'" . nil) ("\\.yy\\'" . nil)))
