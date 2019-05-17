;;; init --- Initializes the editor
;; -*- coding:utf-8 -*-

;;; Commentary:

;; This is the file Emacs wants to be able to modify on its own.

;;; Code:

;; Emacs likes to update this file when options change, so we'll keep
;; the portions we actually care about elsewhere.

;; Add MELPA. Will be needed for the next step.
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Load our local customizations.
(load-file (concat (file-name-directory
                    user-init-file) "local-init.el"))

;; From here to the end of the file, Emacs will make its own updates.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7"
    "#8cc4ff" "#eeeeec"])
 '(column-number-mode t)
 '(custom-enabled-themes (quote (deeper-blue)))
 '(desktop-save-mode t)
 '(font-use-system-font t)
 '(global-hl-line-mode t)
 '(global-linum-mode t)
 '(ido-mode (quote both) nil (ido))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (zenburn-theme yasnippet yaml-mode web-mode
     web-completion-data terraform-mode selectric-mode
     rainbow-mode popup-complete oceanic-theme obsidian-theme
     multiple-cursors minimap markdown-mode+ lua-mode
     less-css-mode green-screen-theme flymake-jshint flymake-easy
     flycheck fill-column-indicator elfeed-goodies elfeed
     electric-spacing dockerfile-mode ac-html tabbar session
     pod-mode markdown-mode initsplit htmlize graphviz-dot-mode
     eproject dpkg-dev-el diminish devscripts csv-mode
     browse-kill-ring boxquote bar-cursor apache-mode)))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Inconsolata" :foundry "PfEd" :slant
 normal :weight normal :height 128 :width normal)))))


(provide 'init)
;;; init.el ends here
