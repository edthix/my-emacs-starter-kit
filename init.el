
;; Switch caps-lock and right ctrl
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; indent the buffer
;; http://stackoverflow.com/questions/4090793/emacs-reindenting-entire-c-buffer
(defun indent-buffer ()
  "Indents an entire buffer using the default intenting scheme."
  interactive)
(save-excursion
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))
(global-set-key (kbd "C-c TAB") 'indent-buffer)

;; quit stuffs
(global-set-key (kbd "C-M-g") 'keyboard-quit)
(global-set-key (kbd "C-x C-g") 'keyboard-quit)

;; set ~/public_html as default director
(setq default-directory "C:/xampp/htdocs")

;; Put some kungfu for emacs
;;(recentf-mode t)
;;(transient-mark-mode t) ;; turn transient
(global-linum-mode t) ;; turn line numbers on
(setq make-backup-files nil)
(setq query-replace-highlight t)
(setq search-highlight t)
(setq font-lock-maximum-decoration t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq require-final-newline t)
(setq major-mode 'text-mode)
;; turn on paren matching
(show-paren-mode t)
(setq show-paren-style 'mixed)
;; Get rid of the startup screen
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
;; Use inconsolota font
;; (setq default-frame-alist '((font . "inconsolata")))
;; Get back font antialiasing
(push '(font-backend xft x) default-frame-alist)
(setq font-lock-maximum-decoration t)

;; ;; Set some defaults
;; (when (not package-archive-contents)
;;   (package-refresh-contents))

;; Packages go here
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; Add in your own as you wish:
(defvar my-packages '(starter-kit 
                      ;; Ruby
                      starter-kit-ruby inf-ruby rinari rspec-mode
                      ruby-compilation ruby-end
                      ruby-mode ruby-test-mode flymake-ruby
                      
                      ;; PHP
                      php+-mode flymake-php

                      ;; CSS
                      flymake-css

                      ;; YAML
                      yaml-mode

                      ;; JS
                      starter-kit-js flymake-jslint

                      ;; Clojure
                      ac-nrepl align-cljlet cljsbuild-mode clojure-mode
                      clojure-test-mode nrepl
                      slamhound starter-kit-lisp
                      ;; clojurescript-mode
                      )
  
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; TODO
;; Fix issue https://github.com/technomancy/emacs-starter-kit/pull/145
(defalias 'inf-ruby-keys 'inf-ruby-setup-keybindings)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(custom-enabled-themes (quote (cyberpunk)))
 '(custom-safe-themes (quote ("ea0c5df0f067d2e3c0f048c1f8795af7b873f5014837feb0a7c8317f34417b04" "6bc195f4f8f9cf1a4b1946a12e33de2b156ce44e384fbc54f1bae5b81e3f5496" "f38dd27d6462c0dac285aa95ae28aeb7df7e545f8930688c18960aeaf4e807ed" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
