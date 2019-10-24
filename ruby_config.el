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
