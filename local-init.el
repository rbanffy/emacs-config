;;; init-local --- The part of init we actually care about.
;; -*- coding:utf-8 -*-

;;; Commentary:

;; Packages to be instaled

(defconst required-packages
  '(
    ac-html
    auto-complete
    dockerfile-mode
    electric-spacing
    elfeed
    elfeed-goodies
    fill-column-indicator
    flycheck
    flymake-easy
    flymake-jshint
    graphviz-dot-mode
    green-screen-theme
    less-css-mode
    lua-mode
    markdown-mode
    markdown-mode+
    minimap
    multiple-cursors
    obsidian-theme
    oceanic-theme
    pkg-info
    popup
    popup-complete
    rainbow-mode
    selectric-mode
    web-completion-data
    web-mode
    yaml-mode
    yasnippet
    zenburn-theme
    )
  "The packages we need installed for this to work.")

;;; Code:

;; Add MELPA. Will be needed for the next step.
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(package-refresh-contents) ;; Update the package list so nothing we need is missing.

;; Install anything that's missing.
(mapc
 (lambda (p)
   (unless (package-installed-p p)
     (package-install p)))
 required-packages)

(set-variable 'flycheck-python-flake8-executable "/opt/local/bin/flake8")
(set-variable
 'package-selected-packages '(
                              ac-html
                              auto-complete
                              dockerfile-mode
                              electric-spacing elfeed
                              elfeed-goodies
                              fill-column-indicator
                              flycheck
                              flymake-jshint
                              lua-mode
                              markdown-mode+
                              popup-complete
                              selectric-mode
                              web-completion-data
                              web-mode
                              yaml-mode
                              yasnippet
                              zencoding-mode
                              )
 )
(set-variable
 'elfeed-feeds '(
                 "http://www.tomshardware.com/feeds/rss2/all.xml"
                 "http://hothardware.com/rss/news.aspx"
                 "http://cacm.acm.org/news.rss"
                 "https://www.nextplatform.com/feed/"
                 "http://gizmodo.com/index.xml"
                 "http://rss.cnn.com/rss/edition.rss"
                 )
 )

;; Note: This file assumes the IBM 3270 fonts
;; (https://github.com/rbanffy/3270font) are installed (it won't fail,
;; it just won't make your Emacs look better)

;; Utility functions
(defun perfect-font-size ()
  "Find out the 'perfect' font size based on screen width."
  (/ (* (window-width) (face-attribute 'default :height)) 100)
  )

;; Set "perfect" font size
(defun set-perfect-font-size ()
  "Set the font to a size perfect to this screen or window."
  (interactive)
  (set-face-attribute 'default nil :height (perfect-font-size)))
(set-perfect-font-size) ; We'll bind this to a key later.

;; Insert file name at point
(defun insert-file-name ()
  "Insert the name of the buffer at point."
  (interactive)
  (insert (buffer-name)))

;; Avoid the startup screen
(setq inhibit-startup-message t)

;; Highlight the fill column
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

;; Because your computer must sound nice, even when your keyboard is not
(require 'selectric-mode)

;; Keyboard mappings
(global-set-key [M-f3] 'grep)
(global-set-key [s-f3] 'grep-find)
(global-set-key (kbd "C-c SPC") 'whitespace-mode)

;; Adjust the screen text size to the perfect size
(global-set-key [C-f12] 'set-perfect-font-size)

;; Toggle hiding blocks
(global-set-key [C-tab] 'hs-toggle-hiding)

;; Make keyboard defaults sensible on Mac
(if (eq 'darwin system-type)
    (progn
      (global-set-key (kbd "C-<home>") 'beginning-of-buffer)
      (global-set-key (kbd "C-<end>") 'end-of-buffer)
      (global-set-key (kbd "<home>") 'move-beginning-of-line)
      (global-set-key (kbd "<end>") 'move-end-of-line)))


;; Find out the right fill-column based on the hostname
(defvar local-python-fill-column)
(if (string= (system-name) "rbmbp.local")
    (setq local-python-fill-column 100)
    (setq local-python-fill-column 79))

;; Set up custom modes
(add-hook 'python-mode-hook
          '(lambda ()
             (progn
               (set-fill-column local-python-fill-column)
               (fci-mode)
               (flycheck-mode)
               (hs-minor-mode t)
               (local-set-key (kbd "s->") 'python-indent-shift-right)
               (local-set-key (kbd "s-<") 'python-indent-shift-left)
               )))
(add-hook 'markdown-mode-hook '(lambda () (progn (set-fill-column 72) (fci-mode))))
(add-hook 'javascript-mode-hook '(lambda () (progn (set-fill-column 79) (fci-mode) (flycheck-mode))))
(add-hook 'emacs-lisp-mode-hook '(lambda () (progn (hs-minor-mode t) (flycheck-mode))))

;; Set up the fringe indicators
(fringe-mode nil)
(setq indicate-buffer-boundaries t)

;; Get us a more appropriate grep
(setq grep-find-command "find .. -type f -exec fgrep -rnH -e  {} +")

;; Set up the default theme
(require 'oceanic-theme)

;; Display color specs in color
(require 'rainbow-mode)
(rainbow-mode t)

;; This is really awesome
(require 'multiple-cursors)

;; Set "perfect" font size
(let ((size (perfect-font-size)))
     (message (format "Setting font size to %s" size))
     (set-face-attribute 'default nil :height size))

(provide 'local-init)
;;; local-init.el ends here
