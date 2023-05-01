;; Default packages to install after a fresh install of Emacs

(defvar default-packages
  '(
    ;; 1. Useful
    anzu
    beacon
    company
    dashboard
    direx
    exec-path-from-shell
    linum-relative
    magit
    markdown-mode
    material-theme
    projectile
    yasnippet
    yasnippet-snippets

    ;; 2. Python
    blacken
    elpy
    py-autopep8
    virtualenvwrapper

    ;; 3. Web
    web-mode

    ;; 4. JavaScript
    js2-mode
    json-mode
    json-reformat

    ;; 5. TypeScript
    tide

    ;; 6. Docker
    docker-compose-mode
    dockerfile-mode
    ))

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(defun packages-installed-p ()
  (dolist (p default-packages)
    (when (not (package-installed-p p))
      t)))

(require 'package)

;; if not all packages are installed, check one by one and install the missing ones from 'default-packages
(unless (packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p default-packages)
    (when (not (package-installed-p p))
      (package-install p))))
