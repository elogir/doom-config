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
  :bind
  ;; (:map projectile-mode-map
  ;;       ("C-c C-c" . #'my-projectile-compile-project)
  ;;       ("C-c C-v" . #'my-projectile-run-project))
  :config
  (push ".class" projectile-globally-ignored-file-suffixes))

(use-package! org-modern
  :delight
  :hook ((org-mode . org-modern-mode)
	 (org-agenda-finalize . org-modern-agenda)))

(use-package! company
  :bind (:map global-map
              (([remap complete-symbol] . #'company-complete)
               ("M-`" . #'company-complete))))
:custom
(company-idle-delay nil)
(company-frontends '(company-pseudo-tooltip-frontend))
(company-tooltip-align-annotations t)
(company-tooltip-limit 6)
(company-tooltip-minimum 6))

(use-package! lsp-java
  :init
  (setq lombok-library-path (concat doom-data-dir "lombok.jar"))
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

;; (use-package! gptel
;;   :custom
;;   (gptel-api-key "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")
;;   (gptel-default-mode 'org-mode)
;;   :config
;;   (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
;;   (add-hook 'gptel-post-response-functions 'gptel-end-of-response))

;; (add-to-list 'load-path "/home/rigole/Documents/Git-Repos/gptel-extensions.el")
;; (require 'gptel-extensions)

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

;; (setq apheleia-mode-alist '(("\\.ll\\'" . nil) ("\\.yy\\'" . nil)))

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
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

(use-package! devdocs
  :hook ((c++-ts-mode . (lambda () (setq-local devdocs-current-docs '("cpp")))))
  :bind (:map global-map
              (("C-h h" . #'devdocs-lookup))))

(use-package! lsp-mode
  :hook (
         (dockerfile-mode . lsp)
         (c-ts-mode . lsp)
         (c++-ts-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :bind (:map lsp-mode-map
              ("M-RET" . #'lsp-execute-code-action))
  :custom
  (lsp-headerline-breadcrumb-enable t)
  :commands lsp)

(use-package! projectile)

(use-package! org-latex-preview
  :config
  (plist-put org-latex-preview-appearance-options
             :page-width 0.8)
  (setq org-latex-preview-process-default 'dvisvgm)
  (add-hook 'org-mode-hook 'org-latex-preview-auto-mode)
  (add-hook 'org-latex-preview-auto-ignored-commands 'next-line)
  (add-hook 'org-latex-preview-auto-ignored-commands 'previous-line)
  (setq org-latex-preview-numbered t)
  (setq org-latex-preview-live t)
  (setq org-latex-preview-live-debounce 0.25))

(use-package! aas
  :hook (org-mode . aas-activate-for-major-mode)
  :config
  (aas-set-snippets 'org-mode
                    :cond #'texmathp
                    "/ens" '(yas "\\mathbb{$1} ")
                    "/fr" '(yas "\\frac{$1}{$2} ")
                    "_" '(yas "_{$1}")
                    "^" '(yas "^{$1}")
                    "dans" "\\in"
                    "Sigma" "\\Sigma"
                    "/pt" "\\forall "
                    "/ex" "\\exists "
                    "RR" "\\mathbb{R} "
                    "NN" "\\mathbb{N} "
                    "PP" "\\mathbb{P} "
                    "<=" "\\leq"
                    ">=" "\\geq"
                    "=>" "\\implies"
                    "->" "\\mapsto"
                    "iif" "\\Longleftrightarrow"
                    "/abs" '(yas "\\displaystyle\\left\\lvert $1 \\right\\rvert $0")
                    "/norme" '(yas "\\lVert $1 \\rVert")
                    "/inf" "\\infty"
                    "/lim" '(yas "\\underset{$1 \\to $2}{lim} $3 = $0")
                    "/dp" '(yas "\\frac{\\partial{$1}}{\\partial{$2}}")
                    "/dd" '(yas "\\frac{d$1}{d$2}")
                    "/t1" '(yas "\\text{$1} ")
                    "/t2" '(yas "\\quad\\text{$1}\\quad ")
                    "/t3" '(yas "\\qquad\\text{$1}\\qquad ")
                    "/pp" '(yas "\\left( $1 \\right)")
                    "~" "\\sim"
                    "/V" '(yas "\\sqrt{$1}")
                    "/int" '(yas "\\int_{$1}^{$2}$3d$4"))
  (aas-set-snippets 'org-mode
                    ";i" '(yas "\\\\( $1 \\\\) $0")
                    ";e" '(yas "\\\\[ $1 \\\\] $0")))

(use-package! yasnippet
  :hook ((lsp-mode . yas-minor-mode) (org-mode . yas-minor-mode))
  :bind (:map yas-keymap
	      ("C-c SPC" . #'yas-expand)
              ("TAB" . #'yas-next-field))
  :config
  (yas-reload-all))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))

(use-package! org
  :bind (:map org-mode-map
              ("C-c C-c" . #'org-babel-execute-src-block)
              ("<tab>" . nil)))

(use-package toc-org
  :hook (org-mode . toc-org-mode))

(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("org-plain-latex"
                 "\\documentclass{article}
           [NO-DEFAULT-PACKAGES]
           \\usepackage{amsmath}
           \\usepackage{graphicx}
           \\usepackage{amssymb}
           \\usepackage{hyperref}
           [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(use-package! engrave-faces-latex
  :after ox-latex)

(setq org-latex-listings 'engraved
      org-latex-engraved-theme 'doom-one-light)

(org-export-update-features 'latex
  (no-protrusion-in-code
   :condition t
   :when (microtype engraved-code)
   :snippet "\\ifcsname Code\\endcsname\n  \\let\\oldcode\\Code\\renewcommand{\\Code}{\\microtypesetup{protrusion=false}\\oldcode}\n\\fi"
   :after (engraved-code microtype)))
