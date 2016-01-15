(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

;; List of packages to use
(defvar my-packages '(better-defaults
                      magit
                      idle-highlight-mode
                      markdown-mode
                      projectile
                      smartparens
                      exec-path-from-shell
                      etags-table
                      etags-select
                      go-mode
                      php-mode
                      company
                      company-go
                      ws-butler
                      flycheck
                      helm
                      el-get
                      go-eldoc
                      gotest
                      multiple-cursors
                      helm-projectile
                      helm-ag
                      helm-open-github
                      vagrant
                      js2-mode
                      js-doc
                      coffee-mode
                      monochrome-theme
                      powerline
                      evil
                      evil-surround
                      evil-leader
                      org
                      yasnippet
                      fill-column-indicator
                      web-mode
                      key-chord
                      ag
                      python-mode
                      ipython
                      ein
                      pydoc-info
		      rich-minority
                      smart-mode-line
                      smart-mode-line-powerline-theme
                      minimal-theme)
  "A list of packages that should be installed at launch")

;; Make sure the packages are up to date
(when (not package-archive-contents)
  (package-refresh-contents))
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Load My Custom Config
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(setq user-specific-config (concat dotfiles-dir "mcos.el"))

(if (file-exists-p user-specific-config) (load user-specific-config))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "gray7" :foreground "light gray" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "nil" :family "Menlo")))))
