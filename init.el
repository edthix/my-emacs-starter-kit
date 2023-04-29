;; Switch caps-lock and right ctrl
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)
(global-set-key (kbd "C-c TAB") 'indent-buffer-fn)

;; Some quit shortcut keys
(global-set-key (kbd "C-M-g") 'keyboard-quit)
(global-set-key (kbd "C-x C-g") 'keyboard-quit)

;; Indent the buffer
;; http://stackoverflow.com/questions/4090793/emacs-reindenting-entire-c-buffer
(defun indent-buffer-fn ()
  "Indents an entire buffer using the default intenting scheme."
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max))))

;; Hack
;; Open init file for re-eval indent function.
;; Call at the end of init
(defun fix-indent-eval ()
  ;;(interactive "P")
  (find-file "~/.emacs.d/init.el")
  (goto-char 194)
  (end-of-line))

;; set ~/public_html as default director
;; (setq default-directory "C:/xampp/htdocs") ;; Windows
;; (setq default-directory "~/public_html") ;; Mac
(setq default-directory "~/Projects") ;; Linux

;; Recentf
;; https://www.emacswiki.org/emacs/RecentFiles
;; I want to keep only latest only 25 items
(recentf-mode t)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)

;; Transient Mark Mode
;; https://www.emacswiki.org/emacs/TransientMarkMode
(transient-mark-mode t)

;; Line Numbers and Indentations
;; Turn line numbers on
(global-linum-mode t)
;; 4 character and a space for line numbers
(setq linum-format "%4d ")
;; Always use 4 spaces to indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)


;; Life is boring with backup files
(setq make-backup-files nil)

;; Search for strings
(setq search-highlight t)
(setq query-replace-highlight t)

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Font-Lock.html
(setq font-lock-maximum-decoration t)

;;https://www.gnu.org/software/emacs/manual/html_node/emacs/Customize-Save.html
(setq require-final-newline t)

;; https://www.gnu.org/software/emacs/manual/html_node/eintr/Text-and-Auto_002dfill.html
;; I want to have text-mode as default
(setq major-mode 'text-mode)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Turn on paren matching
;; https://www.emacswiki.org/emacs/ShowParenMode
;; https://emacs-fu.blogspot.com/2009/01/balancing-your-parentheses.html
(show-paren-mode t)
(setq show-paren-style 'expression)

;; Prompts and startups
;; https://www.masteringemacs.org/article/disabling-prompts-emacs
;; Get rid of the startup screen
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(setq initial-scratch-message nil)
;; https://www.emacswiki.org/emacs/YesOrNoP
(fset 'yes-or-no-p 'y-or-n-p)

;; Get back font antialiasing
(setq font-lock-maximum-decoration t)

;; Sets a 80 character line width
;; http://mixandgo.com/blog/how-i-ve-convinced-emacs-to-dance-with-ruby
(setq-default fill-column 80)

;; Enable copy/past-ing from clipboard
(setq x-select-enable-clipboard t)

;; Always reload the file if it changed on disk
(global-auto-revert-mode 1)

;; A nice line height
(setq-default line-spacing 1)

;; Treat the CMD key like meta on OSX
(setq mac-command-modifier 'meta)

;; Turn off tool-bar
(tool-bar-mode 0)

;; Set some defaults
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; Projectile
;; https://projectile.mx
(projectile-mode 1)
(global-set-key (kbd "C-c a") 'projectile-command-map)
(global-set-key (kbd "C-c f") 'projectile-find-file)
(global-set-key (kbd "C-c g") 'projectile-grep)

;; Ido Mode
;; https://www.masteringemacs.org/article/introduction-to-ido-mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Company Mode
;; https://company-mode.github.io/
(add-hook 'after-init-hook 'global-company-mode)
;; Make company-mode fast to react
(setq company-minimum-prefix-length 2)
(setq company-idle-delay
      (lambda () (if (company-in-string-or-comment) nil 0.3)))

;; Whitespace
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
;; remove trailing whitespaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Narrow To Region Mode
(put 'narrow-to-region 'disabled nil)

;; Dashboard
;; https://github.com/emacs-dashboard/emacs-dashboard
(dashboard-setup-startup-hook)
(setq dashboard-banner-logo-title "It's coding time")
(setq dashboard-startup-banner 'logo)
(setq dashboard-center-content t)

;; start emacs and go to init tab function above
(fix-indent-eval)
(load-theme 'material t)

;; Enable upcase and downcase
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(print "Emacs initialized")
