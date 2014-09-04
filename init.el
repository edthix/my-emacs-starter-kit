;; Switch caps-lock and right ctrl
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; indent the buffer
;; http://stackoverflow.com/questions/4090793/emacs-reindenting-entire-c-buffer
(defun indent-buffer ()
  "Indents an entire buffer using the default intenting scheme."
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max))))

(global-set-key (kbd "C-c TAB") 'indent-buffer)

;; quit stuffs
(global-set-key (kbd "C-M-g") 'keyboard-quit)
(global-set-key (kbd "C-x C-g") 'keyboard-quit)

;; set ~/public_html as default director
;; (setq default-directory "C:/xampp/htdocs") ;; Windows
(setq default-directory "~/public_html") ;; Linux

;; Put some kungfu for emacs
(recentf-mode t)
(transient-mark-mode t) ;; turn transient

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
;; (defvar my-packages
;;   '(starter-kit
;;     ;; Ruby
;;     starter-kit-ruby inf-ruby rinari rspec-mode
;;     ruby-compilation ruby-end
;;     ruby-mode ruby-test-mode flymake-ruby

;;     ;; PHP
;;     php+-mode flymake-php

;;     ;; CSS
;;     flymake-css

;;     ;; YAML
;;     yaml-mode

;;     ;; JS
;;     starter-kit-js flymake-jslint

;;     ;; Clojure
;;     ac-nrepl align-cljlet cljsbuild-mode clojure-mode
;;     clojure-test-mode nrepl
;;     slamhound starter-kit-lisp
;;     ;; clojurescript-mode
;;     )
;;   "A list of packages to ensure are installed at launch.")

;; (dolist (p my-packages)
;;   (when (not (package-installed-p p))
;;     (package-install p)))

;; Turn ido mode
(ido-mode t)

;; Turn of tool-bar
(tool-bar-mode -1)

;; TODO
;; Fix issue https://github.com/technomancy/emacs-starter-kit/pull/145
;; (defalias 'inf-ruby-keys 'inf-ruby-setup-keybindings)

;; Ruby settings
;; http://lorefnon.me/2014/02/02/configuring-emacs-for-rails.html
(add-hook 'ruby-mode-hook 'flymake-ruby-load)

;; Ruby repl inside buffer
(global-set-key (kbd "C-c r r") 'inf-ruby)

;; projectile
(projectile-global-mode)
(add-hook 'ruby-mode-hook 'projectile-on)
(add-hook 'projectile-mode-hook 'projectile-rails-on)

;; flx-ido decorations
(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
(defun ido-disable-line-truncation () (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)
(defun ido-define-keys () ;; C-n/p is more intuitive in vertical layout
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
(add-hook 'ido-setup-hook 'ido-define-keys)

