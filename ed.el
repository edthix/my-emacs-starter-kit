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
(setq default-directory "~/public_html")

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
(setq default-frame-alist '((font . "inconsolata")))
;; Get back font antialiasing
(push '(font-backend xft x) default-frame-alist)
(setq font-lock-maximum-decoration t)
;; Set the background/foreground color
(set-background-color "#2b2b2b")
(set-foreground-color "white")
(set-face-background 'modeline "DarkRed")
(set-face-foreground 'modeline "white")

;;---------------------------------
;; Plugins
;;---------------------------------
;; php-mode - work with php
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.ctp$" . html-mode))
;; set php to have better indent than standard php-mode
(defun pear/php-mode-init()
  "Set some buffer-local variables."
  (setq case-fold-search t)
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-close '0))

(add-hook 'php-mode-hook 'pear/php-mode-init)

;; emacs-cake - work with cakephp framework
;; (add-to-list 'load-path "~/.emacs.d/ed/emacs-historyf")
;; (add-to-list 'load-path "~/.emacs.d/ed/emacs-cake")
;; (require 'cake)
;; (global-cake t)
;; (cake-set-default-keymap)

;; emacs-cake - work with cakephp2 framework
;; (add-to-list 'load-path "~/.emacs.d/ed/emacs-cake2")
;; (require 'cake2)
;; (global-cake2 t)
;; (cake2-set-default-keymap)

;; easy toggle between php and html modes
;; (defun toggle-php-html-mode ()
;;   (interactive)
;;   "Toggle mode between PHP & HTML Helper modes"
;;   (cond ((string= mode-name (sgml-xml-mode "XHTML" "HTML"))
;;          (php-mode))
;;         ((string= mode-name "PHP")
;;          (html-mode))))
;; (global-set-key [f5] 'toggle-php-html-mode)

;; http://stackoverflow.com/questions/3545458/disable-hl-line-in-emacs-when-using-emacs-starter-kit
(remove-hook 'coding-hook 'turn-on-hl-line-mode)

;; git-emacs
(add-to-list 'load-path "~/.emacs.d/ed/git-emacs/")
(require 'git-emacs)

;; Adds lein to PATH so that we can use clojure-jack-in properly
(setenv "PATH" (concat "~/opt/leiningen:" (getenv "PATH")))
(setenv "PATH" (concat "/home/ed/opt/leiningen:" (getenv "PATH")))

;; rhtml-mode
(add-to-list 'load-path "~/.emacs.d/ed/rhtml")
(require 'rhtml-mode)
(add-hook 'rhtml-mode-hook
          (lambda () (rinari-launch)))

;; yasnippets-rails
(load "~/.emacs.d/ed/yasnippets-rails/setup.el")
(add-hook 'html-mode-hook
          (yas/load-directory "~/.emacs.d/ed/yasnippets-rails/rails-snippets"))

;; string interpolation in ruby
;; http://blog.senny.ch/blog/2012/10/06/emacs-tidbits-for-ruby-developers/
(defun senny-ruby-interpolate ()
  "In a double quoted string, interpolate."
  (interactive)
  (insert "#")
  (when (and
         (looking-back "\".*")
         (looking-at ".*\""))
    (insert "{}")
    (backward-char 1)))

(eval-after-load 'ruby-mode
  '(progn
     (define-key ruby-mode-map (kbd "#") 'senny-ruby-interpolate)))

;; Auto Complete stuffs
;;(require 'ac-nrepl)
;; auto-complete for clojure-mode
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'nrepl-mode))
(eval-after-load "nrepl"
  '(add-hook 'clojure-mode-hook 'auto-complete-mode))
(setq ac-auto-start 3) ;; start completion when entered 3 characters
