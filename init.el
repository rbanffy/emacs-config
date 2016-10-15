;;; init --- Initializes the editor
;; -*- coding:utf-8 -*-

;;; Commentary:

;; Packages to be instaled

;; auto-complete
;; fill-column-indicator
;; flycheck
;; flycheck-typescript
;; flymake-easy
;; flymake-jshint
;; less-css-mode
;; markdown-mode
;; markdown-mode+
;; pkg-info
;; popup
;; popup-complete
;; selectric-mode
;; yaml-mode
;; yasnippet
;; zenburn-theme

;; Note: This file assumes the IBM 3270 fonts
;; (https://github.com/rbanffy/3270font) are installed (it won't fail,
;; it just won't make your Emacs look better)

;;; Code:

;; Utility functions
(defun perfect-font-size (pixels)
  "Find out the 'perfect' font size based on screen width (from PIXELS)."
  (cond
   ;; For X-based systems
   ((eq 'x window-system) (cond ((<= pixels 1024) 100)
                                ((<= pixels 1366) 110)
                                ((> pixels 1366) 130)))
   ;; For Macs (and NeXT boxes, or course)
   ((eq 'ns window-system) (cond ((<= pixels 1024) 120)
                                 ((<= pixels 1280) 130)
                                 ((> pixels 1280) 135)))))

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

;; Display column number
(column-number-mode)

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

;; Enable YASnippet
(require 'yasnippet)
(yas-global-mode 1)

;; Highlight the current line and set sane defaults
(global-hl-line-mode t)
(set-face-background 'hl-line "#777")
(set-face-foreground 'hl-line "#fff")

;; Because your computer must sound nice, even when your keyboard is not
(require 'selectric-mode)

;; Keyboard mappings
(global-set-key [M-f3] 'grep)
(global-set-key [s-f3] 'grep-find)
(global-set-key (kbd "C-$") '(lambda () (interactive) (eshell t)))
(global-set-key (kbd "C-c SPC") 'whitespace-mode)

;; Make keyboard defaults sensible on Mac
(if (eq 'darwin system-type)
    (progn
      (global-set-key (kbd "C-<home>") 'beginning-of-buffer)
      (global-set-key (kbd "C-<end>") 'end-of-buffer)
      (global-set-key (kbd "<home>") 'move-beginning-of-line)
      (global-set-key (kbd "<end>") 'move-end-of-line)))

;; Find out the right fill-column based on the hostname
(defvar local-python-fill-column)
(if (string= system-name "rbmbp.local")
    (setq local-python-fill-column 100)
    (setq local-python-fill-column 79))

;; Set up custom modes
(add-hook 'python-mode-hook '(lambda () (progn (set-fill-column local-python-fill-column) (fci-mode) (flycheck-mode))))
(add-hook 'markdown-mode-hook '(lambda () (progn (set-fill-column 72) (fci-mode))))
(add-hook 'javascript-mode-hook '(lambda () (progn (set-fill-column 79) (fci-mode) (flycheck-mode))))
(add-hook 'emacs-lisp-mode-hook '(lambda () (flycheck-mode)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (tango-dark)))
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
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :family "IBM 3270 Narrow"))))
 '(linum ((t (:inherit default :foreground "#777" :background "#333" :box nil :height 100)))))

(provide 'init)
;;; init.el ends here
