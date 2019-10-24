
(defvar required-packages
  '(
    ;; 1. Useful
    ansi
    anzu
    auto-complete
    codesearch
    commander
    company
    direx
    discover
    emamux
    evalator
    exec-path-from-shell
    flycheck
    git-commit
    helm
    magit
    magit-popup
    markdown-mode
    material-theme
    projectile
    xkcd
    yasnippet
    zenburn-theme
    flymd
    flyparens

    ;; 2. webdev packages
    ac-html
    ac-html-bootstrap
    ac-html-csswatcher
    company-web
    enlive
    flymake-css
    flymake-json
    flymake-yaml
    js2-mode
    json-mode
    json-reformat
    json-snatcher
    show-css
    web-beautify
    web-mode
    yaml-mode

    ;; 8. python packages
    auto-virtualenv
    elpy
    flymake-python-pyflakes
    virtualenv

    ;; 7. Erlang
    alchemist
    auto-complete-distel
    company-distel
    edts
    elixir-mode
    elixir-yasnippets
    erlang
    erlstack-mode
    flycheck-credo
    flycheck-dialyxir
    flycheck-dialyzer
    flycheck-elixir
    flycheck-mix
    flymake-elixir
    lfe-mode
    ob-elixir
    ))

(defun packages-installed-p ()
  (dolist (p required-packages)
    (when (not (package-installed-p p))
      t)))

;; ;; if not all packages are installed, check one by one and install the missing ones.
;; (unless (packages-installed-p)
;;   ;; check for new packages (package versions)
;;   (message "%s" "Emacs is now refreshing its package database...")
;;   (package-refresh-contents)
;;   (message "%s" " done.")
;;   ;; install the missing packages
;;   (dolist (p required-packages)
;;     (when (not (package-installed-p p))
;;       (package-install p))))
