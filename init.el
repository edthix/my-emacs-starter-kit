;;********************************************************************************
;; Start - Global configs
;;********************************************************************************
;; Switch caps-lock and right ctrl
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)
(global-set-key (kbd "C-c TAB") 'indent-buffer-fn)q

;; indent the buffer
;; http://stackoverflow.com/questions/4090793/emacs-reindenting-entire-c-buffer
(defun indent-buffer-fn ()
  "Indents an entire buffer using the default intenting scheme."
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max))))

;; Open init file for re-eval indent function.
;; Call at the end of init
(defun fix-indent-eval ()
  ;;(interactive "P")
  (find-file "~/.emacs.d/init.el")
  (goto-char 378)
  (end-of-line))

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

;; Turn off tool-bar
(tool-bar-mode 0)

;; Enable company mode in all buffer
(add-hook 'after-init-hook 'global-company-mode)

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

;; after everything loads we call this function to fix indent
(fix-indent-eval)

;; enable narrow-to-region mode all the time
(put 'narrow-to-region 'disabled nil)

;;********************************************************************************
;; End - Global configs
;;********************************************************************************


;;********************************************************************************
;; Packages config here
;; Set some defaults
;; (when (not package-archive-contents)
;;   (package-refresh-contents))
;;********************************************************************************
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
;; (add-to-list 'package-archives
;;              '("org" . "http://orgmode.org/elpa/") t)

(package-initialize)

;; Projectile
(global-set-key (kbd "C-c f") 'projectile-find-file)

;; anzu mode
(global-anzu-mode +1)
(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)

;; Turn ido mode
(ido-mode t)

;; Turn on which-key mode
(which-key-mode t)
(which-key-setup-side-window-right-bottom)

;;********************************************************************************
;; Start - Ruby configs
;; http://lorefnon.me/2014/02/02/configuring-emacs-for-rails.html
;; http://mixandgo.com/blog/how-i-ve-convinced-emacs-to-dance-with-ruby
;;********************************************************************************
;; (add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
;; (add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
;; (add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
;; (add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
;; (add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
;; (add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
;; (add-to-list 'auto-mode-alist '("\\.rabl$" . ruby-mode))

;; ;; flymake check
;; (require 'flymake-ruby)
;; (add-hook 'ruby-mode-hook 'flymake-ruby-load)

;; ;; refactor
;; ;; (require 'ruby-refactor)
;; ;; (add-hook 'ruby-mode-hook 'ruby-refactor-mode-launch)

;; ;; Ruby repl inside buffer
;; (require 'inf-ruby)
;; (global-set-key (kbd "C-c r r") 'inf-ruby)

;; ;; When folding, take these delimiters into consideration
;; (add-to-list 'hs-special-modes-alist
;;              '(ruby-mode
;;                "\\(class\\|def\\|do\\|if\\)" "\\(end\\)" "#"
;;                (lambda (arg) (ruby-end-of-block)) nil))

;; ;; Turn on eldoc in ruby files to display info about the
;; ;; method or variable at point
;; (add-hook 'ruby-mode-hook 'eldoc-mode)

;; ;; projectile
;; (add-hook 'dired-mode-hook 'projectile-mode)
;; (add-hook 'dired-mode-hook 'projectile-rails-mode)
;; (add-hook 'ruby-mode-hook 'projectile-mode)
;; (add-hook 'ruby-mode-hook 'projectile-rails-on)
;; (add-hook 'projectile-mode-hook 'projectile-rails-on)

;; ;; flx-ido decorations
;; (setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
;; (defun ido-disable-line-truncation () (set (make-local-variable 'truncate-lines) nil))
;; (add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)
;; (defun ido-define-keys () ;; C-n/p is more intuitive in vertical layout
;;   (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
;;   (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
;; (add-hook 'ido-setup-hook 'ido-define-keys)

;; ;; Robe - ide like feature (find class/modules)
;; (require 'robe)
;; (add-hook 'ruby-mode-hook 'robe-mode)

;; ;; Only on mac - get path
;; (when (memq window-system '(mac ns))
;;   (exec-path-from-shell-initialize))

;; ;; Rspec mode
;; (require 'rspec-mode)
;; (add-hook 'ruby-mode-hook 'rspec-mode)
;; (eval-after-load 'rspec-mode
;;   '(rspec-install-snippets))

;; ;; Delete trailing whitespace when saving file
;; (add-hook 'before-save-hook 'delete-trailing-whitespace)

;; ;; Auto complete mode
;; (add-hook 'ruby-mode-hook 'auto-complete-mode)

;; ;; We wanna see the menu
;; (menu-bar-mode t)

;; ;; Add some projectile custom finders
;; (defun projectile-rails-find-uploaders ()
;;   (interactive)
;;   (projectile-rails-find-resource
;;    "uploaders: "
;;    '(("app/uploaders/" "/uploaders/\\(.+\\)_uploader\\.rb$"))
;;    "app/uploaders/${filename}_uploader.rb"))

;; (defun projectile-rails-find-services ()
;;   (interactive)
;;   (projectile-rails-find-resource
;;    "services: "
;;    '(("app/services/" "/services/\\(.+\\)\\.rb$"))
;;    "app/services/${filename}.rb"))

;; (defun projectile-rails-find-workers ()
;;   (interactive)
;;   (projectile-rails-find-resource
;;    "workers: "
;;    '(("app/workers/" "/workers/\\(.+\\)_worker\\.rb$"))
;;    "app/workers/${filename}_worker.rb"))

;; (defun projectile-rails-find-listeners ()
;;   (interactive)
;;   (projectile-rails-find-resource
;;    "listeners: "
;;    '(("app/listeners/" "/listeners/\\(.+\\)_listener\\.rb$"))
;;    "app/listeners/${filename}_listener.rb"))

;; (defun projectile-rails-find-models-concerns ()
;;   (interactive)
;;   (projectile-rails-find-resource
;;    "models concerns: "
;;    '(("app/models/concerns/" "/concerns/\\(.+\\)\\.rb$"))
;;    "app/models/concerns/${filename}.rb"))

;; (defun projectile-rails-find-controllers-concerns ()
;;   (interactive)
;;   (projectile-rails-find-resource
;;    "controllers concerns: "
;;    '(("app/controllers/concerns/" "/concerns/\\(.+\\)\\.rb$"))
;;    "app/controllers/concerns/${filename}.rb"))

;; (defun projectile-rails-find-active-admin ()
;;   (interactive)
;;   (projectile-rails-find-resource
;;    "active admin: "
;;    '(("app/admin/" "/\\(.+\\)\\.rb$"))
;;    "app/admin/${filename}.rb"))

;; (defun projectile-rails-find-config ()
;;   (interactive)
;;   (projectile-rails-find-resource
;;    "config: "
;;    '(("config/" "\\(.+\\)$"))
;;    "config/${filename}.rb"))

;; TODO
;; Fix issue https://github.com/technomancy/emacs-starter-kit/pull/145
;; (defalias 'inf-ruby-keys 'inf-ruby-setup-keybindings)

;;********************************************************************************
;; End - Ruby configs
;;********************************************************************************


;;********************************************************************************
;; Start - Clojure configs
;;********************************************************************************
;; (add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))

;; ;; Enable paredit-mode
;; (add-hook 'clojure-mode-hook #'paredit-mode)
;; (add-hook 'cider-repl-mode-hook #'paredit-mode)

;; ;; Enable rainbow-delimiters-mode
;; (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
;; (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)

;; ;; Auto complete mode
;; (add-hook 'clojure-mode-hook 'auto-complete-mode)

;; ;; Enable el-doc-mode
;; (add-hook 'clojure-mode-hook #'eldoc-mode)
;; (add-hook 'cider-repl-mode-hook #'eldoc-mode)

;;********************************************************************************
;; End - Clojure configs
;;********************************************************************************


;;********************************************************************************
;; Start - Elixir configs
;;********************************************************************************
(require 'elixir-mode)
(add-to-list 'auto-mode-alist '("\\.elixir2\\'" . elixir-mode))
(add-to-list 'auto-mode-alist '("\\.exs\\'" . elixir-mode))
(add-to-list 'elixir-mode-hook 'alchemist-mode)
(add-to-list 'elixir-mode-hook 'yas-minor-mode)
(add-to-list 'elixir-mode-hook 'auto-complete-mode)
(add-to-list 'elixir-mode-hook 'company-mode)
(add-to-list 'auto-mode-alist '("\\.html.eex\\'" . web-mode))

;;********************************************************************************
;; End - Elixir configs
;;********************************************************************************


;;********************************************************************************
;; Start - Web configs
;; http://www.cyrusinnovation.com/initial-emacs-setup-for-reactreactnative/
;;********************************************************************************
;; web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.rhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))

(setq web-mode-markup-indent-offset 2
      web-mode-css-indent-offset 2
      web-mode-code-indent-offset 2)
(setq js-indent-level 2)


;;********************************************************************************
;; End - Web configs
;;********************************************************************************
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "cdb4ffdecc682978da78700a461cdc77456c3a6df1c1803ae2dd55c59fa703e3" default)))
 '(package-selected-packages
   (quote
    (auto-virtualenv csv-mode company-web undo-tree goto-chg zenburn-theme helm yaml-imenu xkcd which-key web-mode virtualenv show-css projectile ob-elixir material-theme magithub lsp-elixir\.el lfe-mode json-navigator json-mode js2-mode flyparens flymd flymake-yaml flymake-shell flymake-python-pyflakes flymake-json flymake-css flycheck-rebar3 flycheck-mix flycheck-elixir flycheck-dialyzer flycheck-dialyxir flycheck-credo exec-path-from-shell evalator erlstack-mode enlive emamux elpy elixir-yasnippets edts discover direx csv company-erlang company-distel commander auto-complete-distel anzu ansi ac-html-csswatcher ac-html-bootstrap ac-html ac-alchemist))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
