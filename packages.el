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
(package! flycheck-grammalecte)
(package! xterm-color)
(package! move-text)
(package! dape)
