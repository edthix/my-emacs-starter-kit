
(defvar required-packages
  '(
    ;; 1. Useful
    company
    dashboard
    direx
    exec-path-from-shell
    git-commit
    linum-relative
    magit
    magit-popup
    markdown-mode
    material-theme
    projectile
    zenburn-theme
    ))

;; Set some defaults
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(defun packages-installed-p ()
  (dolist (p required-packages)
    (when (not (package-installed-p p))
      t)))

;; if not all packages are installed, check one by one and install the missing ones.
(unless (packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p required-packages)
    (when (not (package-installed-p p))
      (package-install p))))
