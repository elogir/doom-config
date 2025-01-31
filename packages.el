;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! crux)
(package! goto-line-preview)
(package! v-mode
  :recipe (:host github :repo "elogir/v-mode"))
(package! jsdoc)
;; (package! treesit-auto)
;; (package! ultimate-js-mode
;;   :recipe (:host github :repo "guillaumebrunerie/ultimate-js-mode"))

(unpin! apheleia)
(package! apheleia
  :recipe (:host github :repo "elogir/apheleia"))

(package! org-modern)
(package! toc-org)

(unpin! lsp-mode)

(package! janet-mode
  :recipe (:host github
           :repo "ALSchwalm/janet-mode"))


;; (package! janet-ts-mode
;;   :recipe (:host github
;;            :repo "sogaiu/janet-ts-mode"))

(package! persistent-scratch)
;; (package! flycheck-grammalecte)
(package! xterm-color)
(package! move-text)
(package! dape)
(package! ctrlf)
(package! yasnippet-snippets)
(package! eat
  :recipe (:host codeberg
           :repo "akib/emacs-eat"
           :files ("*.el" ("term" "term/*.el") "*.texi"
                   "*.ti" ("terminfo/e" "terminfo/e/*")
                   ("terminfo/65" "terminfo/65/*")
                   ("integration" "integration/*")
                   (:exclude ".dir-locals.el" "*-tests.el"))))
(package! gptel)
(package! gptel-quick :recipe (:host github :repo "karthink/gptel-quick"))
(package! ement)
(package! ultra-scroll :recipe (:host github :repo "jdtsmith/ultra-scroll"))
(package! elfeed)
(package! reddigg)
;; (package! elysium :recipe (:host github :repo "lanceberge/elysium"))
;; (package! gptel :recipe (:host github :repo "elogir/gptel" :no-byte-compile t))
