(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 4)
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(org-latex-preview-appearance-options
   '(:foreground auto :background "Transparent" :scale 1.5 :zoom 1.5 :page-width 0.8 :matchers
     ("begin" "$1" "$" "$$" "\\(" "\\[")))
 '(org-latex-preview-preamble
   "\\documentclass{article}\12[DEFAULT-PACKAGES]\12[PACKAGES]\12\\usepackage{xcolor}\12\\usepackage{amssymb}\12\\usepackage{amsmath}")
 '(safe-local-variable-values
   '((etags-regen-ignores "test/manual/etags/")
     (etags-regen-regexp-alist
      (("c" "objc")
       "/[ \11]*DEFVAR_[A-Z_ \11(]+\"\\([^\"]+\\)\"/\\1/" "/[ \11]*DEFVAR_[A-Z_ \11(]+\"[^\"]+\",[ \11]\\([A-Za-z0-9_]+\\)/\\1/")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'customize-group 'disabled nil)
(put 'erase-buffer 'disabled nil)
