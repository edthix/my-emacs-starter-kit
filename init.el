;;********************************************************************************
;; Start - Global configs
;;********************************************************************************
;; Switch caps-lock and right ctrl
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)
(global-set-key (kbd "C-c TAB") 'indent-buffer-fn)

;; Keyboard quit shortcut key
(global-set-key (kbd "C-M-g") 'keyboard-quit)
(global-set-key (kbd "C-x C-g") 'keyboard-quit)

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

;; Always use 4 spaces to indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; projectile key
(global-set-key (kbd "C-c f") 'projectile-find-file)

;; anzu mode
;; (global-anzu-mode +1)
;; (set-face-attribute 'anzu-mode-line nil
;;                     :foreground "yellow" :weight 'bold)

;; Turn ido mode
(ido-mode t)

;; Turn on which-key mode
;; (which-key-mode t)
;; (which-key-setup-side-window-right-bottom)

;; Turn off tool-bar
(tool-bar-mode 0)

;; Enable company mode in all buffer
(add-hook 'after-init-hook 'global-company-mode)

;; whitespace hacks
;; limit line length
(require 'whitespace)
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

;; enable narrow-to-region mode all the time
(put 'narrow-to-region 'disabled nil)

;; remove trailing whitespaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; verb-mode
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map))


;;********************************************************************************
;; End - Global configs
;;********************************************************************************


;;********************************************************************************
;; Start - Packages configs
;;********************************************************************************

;; Set some defaults
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;;********************************************************************************
;; End - Packages configs
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
(add-to-list 'auto-mode-alist '("\\.elixir\\'" . elixir-mode))
(add-to-list 'auto-mode-alist '("\\.exs\\'" . elixir-mode))
(add-to-list 'auto-mode-alist '("\\.ex\\'" . elixir-mode))
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

(fix-indent-eval)
(print "Emacs initialized")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (material)))
 '(custom-safe-themes
   (quote
    ("d4f8fcc20d4b44bf5796196dbeabec42078c2ddb16dcb6ec145a1c610e0842f3" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" default)))
 '(package-selected-packages
   (quote
<<<<<<< HEAD
    (verb pylint n4js flycheck-dogma lsp-elixir tide zenburn-theme yaml-imenu xkcd which-key web-mode virtualenv spacemacs-theme show-css projectile ob-elixir material-theme magithub lsp-elixir\.el lfe-mode json-navigator json-mode js2-mode flyparens flymd flymake-yaml flymake-shell flymake-python-pyflakes flymake-json flymake-elixir flymake-css flycheck-rebar3 flycheck-mix flycheck-elixir flycheck-dialyzer flycheck-dialyxir flycheck-credo find-file-in-project exec-path-from-shell evalator erlstack-mode enlive emamux elpy elixir-yasnippets edts discover direx csv company-web company-erlang company-distel auto-virtualenv auto-complete-distel anzu ansi ac-html-csswatcher ac-html-bootstrap ac-html ac-alchemist 0blayout))))
=======
    (typescript-mode csv-mode zenburn-theme yaml-imenu xkcd which-key web-mode virtualenv spacemacs-theme show-css projectile ob-elixir material-theme magithub lsp-elixir\.el lfe-mode json-navigator json-mode js2-mode helm flyparens flymd flymake-yaml flymake-shell flymake-python-pyflakes flymake-json flymake-elixir flymake-css flycheck-rebar3 flycheck-mix flycheck-elixir flycheck-dialyzer flycheck-dialyxir flycheck-credo find-file-in-project exec-path-from-shell evalator erlstack-mode enlive emamux elpy elixir-yasnippets edts discover direx csv company-web company-erlang company-distel auto-virtualenv auto-complete-distel anzu ansi ac-html-csswatcher ac-html-bootstrap ac-html ac-alchemist 0blayout))))
>>>>>>> fd5c3807feb4fc6e316653a70afd6b1f6348fac6
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
<<<<<<< HEAD
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
=======
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
>>>>>>> fd5c3807feb4fc6e316653a70afd6b1f6348fac6
