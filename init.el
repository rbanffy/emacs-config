;;; init --- Initializes the editor
;; -*- coding:utf-8 -*-

;;; Commentary:

;; This is the file Emacs wants to be able to modify on its own.

;;; Code:

;; Emacs likes to update this file when options change, so we'll keep
;; the portions we actually care about elsewhere.

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Load our local customizations.
(load-file (concat (file-name-directory user-init-file) "local-init.el"))

;; From here to the end of the file, Emacs will make its own updates.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes nil)
 '(exec-path
   (quote
    ("/usr/bin" "/bin" "/usr/sbin" "/sbin" "~/.pyenv/shims/" "/Applications/Emacs.app/Contents/MacOS/bin-x86_64-10_9" "/Applications/Emacs.app/Contents/MacOS/libexec-x86_64-10_9" "/Applications/Emacs.app/Contents/MacOS/libexec" "/Applications/Emacs.app/Contents/MacOS/bin" "/usr/local/bin")))
 '(indent-tabs-mode nil)
 '(package-selected-packages
   (quote
    (ansible ansible-doc terraform-mode django-mode django-snippets docker-compose-mode flymake-go go-mode kubernetes rust-mode dot-mode git-gutter string-inflection ac-html auto-complete dockerfile-mode electric-spacing elfeed elfeed-goodies fill-column-indicator flycheck flymake-jshint lua-mode markdown-mode+ popup-complete selectric-mode web-completion-data web-mode yaml-mode yasnippet zencoding-mode)))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :family "IBM 3270 Narrow")))))

(provide 'init)
;;; init.el ends here
