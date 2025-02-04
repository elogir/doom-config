;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; Packages

(package! crux)
(package! goto-line-preview)
(package! v-mode :recipe (:host github :repo "elogir/v-mode"))
(package! org-modern)
(package! toc-org)
(package! xterm-color)
(package! move-text)
(package! dape)
(package! ctrlf)
(package! yasnippet-snippets)
(package! gptel :recipe (:host github :repo "karthink/gptel" :branch "master"))
(package! gptel-quick :recipe (:host github :repo "karthink/gptel-quick"))
(package! ement)
(package! ultra-scroll :recipe (:host github :repo "jdtsmith/ultra-scroll"))
(package! reddigg)

;;; Unpins

(unpin! lsp-mode)
