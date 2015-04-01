;; ;; Set some defaults
;; (when (not package-archive-contents)
;;   (package-refresh-contents))

;; Packages go here
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; Switch caps-lock and right ctrl
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; indent the buffer
;; http://stackoverflow.com/questions/4090793/emacs-reindenting-entire-c-buffer
(defun indent-buffer-fn ()
  "Indents an entire buffer using the default intenting scheme."
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max))))

(global-set-key (kbd "C-c TAB") 'indent-buffer-fn)

;; Open init file for re-eval indent function
(defun fix-indent-eval ()
  ;;(interactive "P")
  (find-file "~/.emacs.d/init.el")
  (goto-char 800)
  (end-of-line)
  ;;(eval-last-sexp)
  ;;(kill-buffer "init.el")
  )
(fix-indent-eval)


;; Keyboard quit shortcut key
(global-set-key (kbd "C-M-g") 'keyboard-quit)
(global-set-key (kbd "C-x C-g") 'keyboard-quit)

;; set ~/public_html as default director
;; (setq default-directory "C:/xampp/htdocs") ;; Windows
(setq default-directory "~/public_html") ;; Linux

;; Put some kungfu for emacs
(recentf-mode t)
;; turn transient
(transient-mark-mode t)
;; turn line numbers on
(global-linum-mode t)
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
;; Get back font antialiasing
(push '(font-backend xft x) default-frame-alist)
(setq font-lock-maximum-decoration t)

;; http://mixandgo.com/blog/how-i-ve-convinced-emacs-to-dance-with-ruby
;; Sets a 80 character line width
(setq-default fill-column 80)
;; Enable copy/past-ing from clipboard
(setq x-select-enable-clipboard t)
;; Always reload the file if it changed on disk
(global-auto-revert-mode 1)
;; A nice line height
(setq-default line-spacing 1)
;; Treat the CMD key like meta on OSX
(setq mac-command-modifier 'meta)
;; 4 character and a space for line numbers
(setq linum-format "%4d ")
;; Always use two spaces to indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; anzu mode
(global-anzu-mode +1)
(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)

;; Turn ido mode
(ido-mode t)

;; Turn off tool-bar
(tool-bar-mode 0)

;; TODO
;; Fix issue https://github.com/technomancy/emacs-starter-kit/pull/145
;; (defalias 'inf-ruby-keys 'inf-ruby-setup-keybindings)

;;********************************************************************************
;; Ruby settings
;; http://lorefnon.me/2014/02/02/configuring-emacs-for-rails.html
;; http://mixandgo.com/blog/how-i-ve-convinced-emacs-to-dance-with-ruby
;;********************************************************************************
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))

;; flymake check
(require 'flymake-ruby)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)

;; refactor
(require 'ruby-refactor)
(add-hook 'ruby-mode-hook 'ruby-refactor-mode-launch)

;; Ruby repl inside buffer
(require 'inf-ruby)
(global-set-key (kbd "C-c r r") 'inf-ruby)

;; When folding, take these delimiters into consideration
(add-to-list 'hs-special-modes-alist
             '(ruby-mode
               "\\(class\\|def\\|do\\|if\\)" "\\(end\\)" "#"
               (lambda (arg) (ruby-end-of-block)) nil))

;; Turn on eldoc in ruby files to display info about the
;; method or variable at point
(add-hook 'ruby-mode-hook 'eldoc-mode)

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

;; Robe - ide like feature (find class/modules)
(require 'robe)
(add-hook 'ruby-mode-hook 'robe-mode)

;; Only on mac - get path
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Rspec mode
(require 'rspec-mode)
(add-hook 'ruby-mode-hook 'rspec-mode)
(eval-after-load 'rspec-mode
  '(rspec-install-snippets))

;; Delete trailing whitespace when saving file
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; We wanna see the menu
(menu-bar-mode t)

;; Add some projectile custom finders
(defun projectile-rails-find-uploaders ()
  (interactive)
  (projectile-rails-find-resource
   "uploaders: "
   '(("app/uploaders/" "/uploaders/\\(.+\\)_uploader\\.rb$"))
   "app/uploaders/${filename}_uploader.rb"))

;; goto-last-change
(require 'goto-chg)

;; whitespace hacks
(require 'whitespace)
;; limit line length
(setq whitespace-line-column 80)
(setq whitespace-style '(spaces tabs newline space-mark tab-mark newline-mark face lines-tail))
(setq whitespace-display-mappings
      ;; all numbers are Unicode codepoint in decimal. e.g. (insert-char 182 1)
      '(
        ;;(space-mark nil) ; 32 SPACE, 183 MIDDLE DOT
        ;;(newline-mark 10 [172 10]) ; 10 LINE FEED
        (tab-mark 9 [183 9] [92 9]) ; 9 TAB, MIDDLE DOT
        ))
(setq whitespace-global-modes '(not org-mode web-mode "Web" emacs-lisp-mode))
(global-whitespace-mode)

;; Learn more about emacs with discover
(require 'discover)
(global-discover-mode 1)

;;******************************************************************************
;; Optional for Mac
;;******************************************************************************
