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
 '(elfeed-feeds
   (quote
    ("http://www.tomshardware.com/feeds/rss2/all.xml" "http://hothardware.com/rss/news.aspx" "http://cacm.acm.org/news.rss" "https://www.nextplatform.com/feed/" "http://gizmodo.com/index.xml" "http://rss.cnn.com/rss/edition.rss")))
 '(exec-path
   (quote
    ("/usr/bin" "/bin" "/usr/sbin" "/sbin" "~/.pyenv/shims/" "/Applications/Emacs.app/Contents/MacOS/bin-x86_64-10_9" "/Applications/Emacs.app/Contents/MacOS/libexec-x86_64-10_9" "/Applications/Emacs.app/Contents/MacOS/libexec" "/Applications/Emacs.app/Contents/MacOS/bin" "/usr/local/bin")))
 '(indent-tabs-mode nil)
 '(package-selected-packages
   (quote
    (dockerfile-mode web-mode web-completion-data ac-html electric-spacing elfeed elfeed-goodies zencoding-mode zenburn-theme yasnippet yaml-mode selectric-mode popup-complete markdown-mode+ flymake-jshint flycheck fill-column-indicator auto-complete)))
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
