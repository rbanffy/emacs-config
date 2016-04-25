; -*- coding:utf-8 -*-


;; Add MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Avoid the startup screen
(setq inhibit-startup-message t)

;; Highlight the fill column
(require 'fill-column-indicator)
(fci-mode t)

;; Use a better way to open files
(ido-mode)

;; Clean up file on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Show matching parentheses
(show-paren-mode 1)

;; Restore previous desktop on startup
(desktop-save-mode 1)

;; Tabs are 4 columns and use spaces
(setq tab-width 4)
(setq indent-tabs-mode nil)

;; Show line numbers on the left
(global-linum-mode)

;; Enable autocomplete (requires package 'auto-complete', which is not listed by package-list)
(require 'auto-complete-config)
(ac-config-default)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
   (quote
    ("c4465c56ee0cac519dd6ab6249c7fd5bb2c7f7f78ba2875d28a50d3c20a59473" "1db337246ebc9c083be0d728f8d20913a0f46edc0a00277746ba411c149d7fe5" default)))
 '(custom-theme-load-path
   (quote
    ("/Users/ricardobanffy/.emacs.d/elpa/ample-zen-theme-20150119.1354/" "/Users/ricardobanffy/.emacs.d/elpa/zenburn-theme-20160416.1011/" custom-theme-directory t)))
 '(exec-path
   (quote
    ("/usr/bin" "/bin" "/usr/sbin" "/sbin" "/Applications/Emacs.app/Contents/MacOS/bin-x86_64-10_9" "/Applications/Emacs.app/Contents/MacOS/libexec-x86_64-10_9" "/Applications/Emacs.app/Contents/MacOS/libexec" "/Applications/Emacs.app/Contents/MacOS/bin" "/usr/local/bin")))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 180 :width normal :family "IBM 3270 Narrow"))))
 '(linum ((t (:inherit default :foreground "#777" :background "#333" :box nil :height 100)))))
