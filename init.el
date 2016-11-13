;;; init --- Initializes the editor
;; -*- coding:utf-8 -*-

;;; Commentary:

;; This is the file Emacs wants to be able to modify on its own.

;;; Code:

;; Emacs likes to update this file when options change, so we'll keep
;; the portions we actually care about elsewhere.
(load-file (concat (file-name-directory user-init-file) "local-init.el"))

;; From here to the end of the file, Emacs will make its own updates.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes nil)
 '(custom-safe-themes
   (quote
    ("c4465c56ee0cac519dd6ab6249c7fd5bb2c7f7f78ba2875d28a50d3c20a59473" "1db337246ebc9c083be0d728f8d20913a0f46edc0a00277746ba411c149d7fe5" default)))
 '(custom-theme-load-path
   (quote
    ("~/.emacs.d/elpa/ample-zen-theme-20150119.1354/" "~/.emacs.d/elpa/zenburn-theme-20160416.1011/" custom-theme-directory t)))
 '(exec-path
   (quote
    ("/usr/bin" "/bin" "/usr/sbin" "/sbin" "~/.pyenv/shims/" "/Applications/Emacs.app/Contents/MacOS/bin-x86_64-10_9" "/Applications/Emacs.app/Contents/MacOS/libexec-x86_64-10_9" "/Applications/Emacs.app/Contents/MacOS/libexec" "/Applications/Emacs.app/Contents/MacOS/bin" "/usr/local/bin")))
 '(indent-tabs-mode nil)
 '(package-selected-packages
   (quote
    (zencoding-mode zenburn-theme yasnippet yaml-mode selectric-mode popup-complete markdown-mode+ flymake-jshint flycheck fill-column-indicator auto-complete)))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 200 :width normal :family "IBM 3270 Narrow"))))
 '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face :slant italic))))
 '(linum ((t (:inherit default :foreground "#777" :background "#333" :box nil :height 100))))
 '(minimap-active-region-background ((t (:background "gray37")))))

(provide 'init)
;;; init.el ends here
