
(defvar required-packages
  '(
    ;; 1. Useful
    ansi
    auto-complete
    better-defaults
    company
    dashboard
    direx
    discover
    emamux
    evalator
    exec-path-from-shell
    flycheck
    flymd
    flyparens
    git-commit
    helm
    magit
    magit-popup
    markdown-mode
    material-theme
    projectile
    tide
    xkcd
    yasnippet
    zenburn-theme

    ;; 2. webdev packages
    ac-html
    ac-html-bootstrap
    ac-html-csswatcher
    company-web
    enlive
    flymake-css
    flymake-json
    flymake-yaml
    import-js
    js2-mode
    json-mode
    json-reformat
    json-snatcher
    prettier-js
    show-css
    typescript-mode
    web-beautify
    web-mode
    yaml-mode

    ;; 3. python packages
    auto-virtualenv
    elpy
    flymake-python-pyflakes
    py-autopep8
    pyenv-mode
    virtualenv

    ;; 4. Erlang
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
    flymake-elixir
    lfe-mode
    ob-elixir

    ;; 5. docker
    docker
    docker-api
    docker-cli
    docker-compose-mode
    dockerfile-mode

    ;; 6. kubernets
    k8s-mode
    kubel
    kubernetes
    kubernetes-helm
    kubernetes-tramp

    ;; 7. lsp
    company-box
    lsp-treemacs
    lsp-ui
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
