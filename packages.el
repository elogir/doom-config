;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! crux)
(package! goto-line-preview)
(package! org-modern)
                                        ; (package! forge)
                                        ;(package! dap-mode
                                        ;  :recipe
                                        ;  (:host github :repo "elogir/dap-mode"))
                                        ;(package! gptel)
(package! v-mode
  :recipe (:host github :repo "elogir/v-mode"))

;; (package! v-mode)
;; (package! realgud)
;; (package! realgud-lldb)
;; (package! devdocs)
;; (package! treesit-auto)
;; (package! d-mode)

;; (package! org :recipe
;;   (:host nil :repo "https://git.tecosaur.net/mirrors/org-mode.git" :remote "mirror" :fork
;;    (:host nil :repo "https://git.tecosaur.net/tec/org-mode.git" :branch "dev" :remote "tecosaur")
;;    :files
;;    (:defaults "etc")
;;    :build t :pre-build
;;    (with-temp-file "org-version.el"
;;      (require 'lisp-mnt)
;;      (let
;;          ((version
;;            (with-temp-buffer
;;              (insert-file-contents "lisp/org.el")
;;              (lm-header "version")))
;;           (git-version
;;            (string-trim
;;             (with-temp-buffer
;;               (call-process "git" nil t nil "rev-parse" "--short" "HEAD")
;;               (buffer-string)))))
;;        (insert
;;         (format "(defun org-release () \"The release version of Org.\" %S)\n" version)
;;         (format "(defun org-git-version () \"The truncate git commit hash of Org mode.\" %S)\n" git-version)
;;         "(provide 'org-version)\n"))))
;;   :pin nil)

;; (unpin! org)

;; (package! auctex)
;; (package! aas)
;; (package! toc-org)
;; (package! engrave-faces)
;; (package! nov)
