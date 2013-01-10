(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; Set some defaults
(when (not package-archive-contents)
  (package-refresh-contents))

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
 '(custom-safe-themes (quote ("6bc195f4f8f9cf1a4b1946a12e33de2b156ce44e384fbc54f1bae5b81e3f5496" "f38dd27d6462c0dac285aa95ae28aeb7df7e545f8930688c18960aeaf4e807ed" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
