(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

;; List of packages to use
(defvar my-packages '(better-defaults
                      use-package
                      magit
                      idle-highlight-mode
                      markdown-mode
                      projectile
                      smartparens
                      exec-path-from-shell
                      etags-table
                      etags-select
                      go-mode
                      go-rename
                      go-projectile
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
                      js2-refactor
                      json-mode
                      js-doc
                      coffee-mode
                      evil
                      evil-surround
                      evil-leader
                      org
                      yasnippet
                      fill-column-indicator
                      web-mode
                      key-chord
                      ag
                      rich-minority
                      minimal-theme
                      elixir-mode
                      elixir-yasnippets
                      alchemist
                      nav
                      ox-gfm ;; Export org as github flavored markdown
                      org-pandoc
                      yaml-mode
                      protobuf-mode
                      gotests
                      terraform-mode
                      go-impl
                      godoctor
                      helm-gtags
                      gorepl-mode
                      evil-visualstar
                      )

  "A list of packages that should be installed at launch")

;; Make sure the packages are up to date
(when (not package-archive-contents)
  (package-refresh-contents))
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; CONFIRM EXITING!
(setq confirm-kill-emacs 'y-or-n-p)

;; Don't show the menu bar
(menu-bar-mode -1)

;; Set up our .emacs.d as a variable
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

(add-to-list 'load-path (concat dotfiles-dir "custom"))

;; This function replaces modes in some alist with another mode
;;
;; Some modes just insist on putting themselves into the
;; auto-mode-alist, this function helps me get rid of them
(defun replace-auto-mode (oldmode newmode)
  (dolist (aitem auto-mode-alist)
    (if (eq (cdr aitem) oldmode)
        (setcdr aitem newmode))))

;; Need some Common Lisp Here
(require 'cl)

;; Require Final Newline
(setq mode-require-final-newline t)

;; Reload All Changes From Disk
(global-auto-revert-mode t)

;; Turn off bells
(setq ring-bell-function 'ignore)

;; Color Theme
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#2b2b2b" :foreground "#a9b7c6" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "nil" :family "Menlo"))))
 '(company-preview ((t (:foreground "darkgray" :underline t))))
 '(company-preview-common ((t (:inherit company-preview))))
 '(company-tooltip ((t (:background "lightgray" :foreground "black"))))
 '(company-tooltip-annotation ((((type x)) (:inherit company-tooltip-annotation :foreground "black")) (t (:inherit company-tooltip-annotation))))
 '(company-tooltip-common ((((type x)) (:inherit company-tooltip :weight bold)) (t (:inherit company-tooltip))))
 '(company-tooltip-common-selection ((((type x)) (:inherit company-tooltip-selection :weight bold)) (t (:inherit company-tooltip-selection))))
 '(company-tooltip-selection ((t (:background "steelblue" :foreground "white"))))
 '(font-lock-builtin-face ((t (:foreground "#a9b7c6" :weight bold))))
 '(font-lock-constant-face ((t (:foreground "#a9b7c6" :weight bold))))
 '(font-lock-function-name-face ((t (:foreground "#a9b7c6" :weight bold))))
 '(font-lock-keyword-face ((t (:foreground "#a9b7c6" :weight bold))))
 '(font-lock-type-face ((t (:foreground "#a9b7c6" :slant italic))))
 '(font-lock-variable-name-face ((t (:foreground "#a9b7c6"))))
 '(org-level-1 ((t (:foreground "#a9b7c6" :inherit outline-1 :height 1.3))))
 '(org-level-2 ((t (:foreground "#a9b7c6" :inherit outline-2 :height 1.2))))
 '(org-level-3 ((t (:foreground "#a9b7c6" :inherit outline-3 :height 1.15))))
 '(org-level-4 ((t (:foreground "#a9b7c6" :inherit outline-4 :height 1.1))))
 '(org-level-5 ((t (:foreground "#a9b7c6" :inherit outline-4 :height 1.0))))
 '(org-level-6 ((t (:foreground "#a9b7c6" :inherit outline-4 :height 1.0))))
 '(org-level-7 ((t (:foreground "#a9b7c6" :inherit outline-4 :height 1.0))))
 '(org-level-8 ((t (:foreground "#a9b7c6" :inherit outline-4 :height 1.0)))))

(load-theme 'minimal t)

;; Turn off the silly startup message
(setq inhibit-startup-message t)

;;;;;;;;;;;;;;;
;; Key Chord ;;
;;;;;;;;;;;;;;;

;; Key-chord lets you bind commands to combination of key-strokes.
(require 'key-chord)
(key-chord-mode 1)

;;;;;;;;;;;;;;;
;; Evil Mode ;;
;;;;;;;;;;;;;;;

(require 'evil)
(evil-mode 1)

;; Evil Surround Mode
(require 'evil-surround)
(global-evil-surround-mode 1)

;; Use 'jk' as <Esc>
(key-chord-define evil-insert-state-map  "jk" 'evil-normal-state)

(setq evil-insert-state-cursor '(bar))

;; Don't blink the cursor
(blink-cursor-mode 0)

;; Evil Leader Mode
(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader ",")

(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

;; Evil visual star mode
(require 'evil-visualstar)
(global-evil-visualstar-mode)

;;;;;;;;;;;;;;;;;;;;;;
;; Multiple Cursors ;;
;;;;;;;;;;;;;;;;;;;;;;

(require 'multiple-cursors)

;; When you have an active region that spans multiple lines, the following will add a cursor to each line:
(global-set-key (kbd "C-S-c C-S-c") 'mcos/edit-lines)

;; When you want to add multiple cursors not based on continuous lines, but based on keywords in the buffer
(global-set-key (kbd "C->") 'mcos/mark-next-like-this)
(global-set-key (kbd "C-<") 'mcos/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mcos/mark-all-like-this)

;; Smooth scrolling
(setq redisplay-dont-pause t
  scroll-margin 1
  scroll-step 1
  scroll-conservatively 10000
  scroll-preserve-screen-position 1
  scroll-up-aggressively 0.01
  scroll-down-aggressively 0.01)

;; Time and date displaying
(setq display-time-24hr-format t)
(display-time-mode 1)

;; ;; Minor mode hiding
 (require 'rich-minority)
 (rich-minority-mode 1)

 (setq rm-blacklist
       (format "^ \\(%s\\)$"
         (mapconcat #'identity
                    '("FlyC.*"
                      "Projectile.*"
                      "my-keys-mode"
                      "PgLn"
                      "company"
                      "Helm"
                      "wb"
                      "SP"
                      "yas"
                      "Ind"
                      "ElDoc"
                      "ARev"
                      "\$"
                      "Undo-Tree")
                    "\\|")))

;; Projectile for Project Stuff
(require 'projectile)
(projectile-global-mode)

;; RET Behaves as LFD
;; Do this so that return will also indent. Very cool.
(defun RET-behaves-as-LFD ()
  (message "RET-behaves-as-LFD")
  (let ((x (key-binding "\C-j")))
    (local-set-key "\C-m" x)))

;; Company Mode - Autocompletion
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay .2)           ; Set the popup delay to 0.25 seconds
(setq company-minimum-prefix-length 1)
(setq company-tooltip-limit 20)
(setq company-dabbrev-downcase nil)
(setq company-require-match nil)
(setq company-echo-delay 0)                          ; remove annoying blinking
(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing

(eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
     (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)))

(eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
     (define-key company-active-map (kbd "<backtab>") 'company-select-previous)))

(setq company-require-match 'never)

(setq company-frontends
      '(company-pseudo-tooltip-unless-just-one-frontend
        company-preview-frontend
        company-echo-metadata-frontend))

(defvar company-mode/enable-yas t "Enable yasnippet for all backends.")

(require 'company-yasnippet)
(defun company/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend)    (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet company-files))))

(setq company-backends (mapcar #'company/backend-with-yas company-backends))

;; Turn off company-mode in org-mode
(setq company-global-modes '(not org-mode fundamental-mode magit-mode markdown-mode text-mode))

;; Special Theme Stuff
;; for company mode


;; Smart Parens
(smartparens-global-mode t)
;; Add newline and position cursor appropriately when starting a
;; curly brace block in C like modes

(defun sp--my-create-newline-and-enter-sexp (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent. "
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(sp-with-modes '(c-mode js-mode js2-mode php-mode)
  (sp-local-pair "{" nil :post-handlers
                 '((sp--my-create-newline-and-enter-sexp "RET"))))

;; Winner mode
(winner-mode 1)

;; Show whitespace by default
(require 'ws-butler)
(ws-butler-global-mode)

;; Save and Restore Desktop Sessions
;; If we're running in a server, then name the desktop file after the server,
;; so that other servers won't clash
(when (daemonp)
 (defadvice desktop-restore-file-buffer
  (around my-desktop-restore-file-buffer-advice)
  "Be non-interactive while starting a daemon."
  (let ((noninteractive t))
   ad-do-it))
 (ad-activate 'desktop-restore-file-buffer)

 (setq desktop-dirname             "~/.emacs.d/desktop/"
  desktop-base-file-name      (concat (daemonp) ".desktop")
  desktop-base-lock-name      (concat (daemonp) ".lock")
  desktop-path                (list desktop-dirname)
  desktop-save                t
  desktop-files-not-to-save   "^$" ;reload tramp paths
  desktop-load-locked-desktop t))

(desktop-save-mode 1)

;; GRAB THE PATH
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Turn on the zshell properly
(setenv "ESHELL" (concat dotfiles-dir "eshell"))

;; Flycheck Syntax Checking
(add-hook 'after-init-hook #'global-flycheck-mode)

;; (setq-default flycheck-disabled-checkers '(go-build go-errcheck go-unconvert go-test json-jsonlist javascript-jshint))
(setq-default flycheck-disabled-checkers '(json-jsonlist javascript-jshint))

;; Rename a file and a buffer
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
 (filename (buffer-file-name)))
    (if (not filename)
 (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
   (message "A buffer named '%s' already exists!" new-name)
 (progn
   (rename-file name new-name 1)
   (rename-buffer new-name)
   (set-visited-file-name new-name)
   (set-buffer-modified-p nil))))))

;; Load all projects into magit-repo-dirs
(eval-after-load "projectile"
  '(progn (setq magit-repo-dirs (mapcar (lambda (dir)
                                         (substring dir 0 -1))
                                       (remove-if-not (lambda (project)
                                                        (file-directory-p (concat project "/.git/")))
                                                      (projectile-relevant-known-projects))))

         (setq magit-repo-dirs-depth 1)))

;; Line Numbers
(global-linum-mode t)

;; Column Numbers
(column-number-mode t)

;; Fill Column
(setq fci-rule-width 2)
(setq fci-rule-color "darkblue")

;; Markdown Mode
(add-hook 'markdown-mode-hook 'turn-off-auto-fill)
(add-hook 'markdown-mode-hook
          '(lambda() (
                      set-fill-column 120
                      visual-line-mode t )))
(add-hook 'markdown-mode-hook 'fci-mode)

;;;;;;;;;;;;;;;;;;;;;;
;; New Key Bindings ;;
;;;;;;;;;;;;;;;;;;;;;;

;; Map C-x C-m to M-x - So we don't need to searching for Alt
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; C-w is now delete the current word. So we don't have to go and find BACKSPACE
(global-set-key "\C-w" 'backward-kill-word)

;; Remap Kill-Region to C-x C-k
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

;; Magit Status
(global-set-key (kbd "C-x g") 'magit-status)
(setq magit-last-seen-setup-instructions "1.4.0")

;;;;;;;;;;;;;;;;
;; Helm Setup ;;
;;;;;;;;;;;;;;;;
(require 'helm)
(require 'helm-config)
(require 'helm-gtags)

(helm-mode 1)
(helm-autoresize-mode 1)

;; customize
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("7838757247452123ec287fd978797f63294f6d8b26b300bb883131f5b6face54" "b749694d80fcaa9bd917c83a8f83729cdd2a79d2e60d384459eeca17b56b7bb6" "cc0dbb53a10215b696d391a90de635ba1699072745bf653b53774706999208e3" default)))
 '(helm-gtags-auto-update t)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-path-style (quote relative))
 '(package-selected-packages
   (quote
    (evil-visualstar gorepl-mode php-extras ede-php-autoload deferred helm-go-package godoctor flycheck-protobuf helm-gtags ggtags php-eldoc yaml-mode xcscope ws-butler web-mode vagrant use-package terraform-mode tao-theme smartparens smart-mode-line-powerline-theme python-mode pydoc-info protobuf-mode powerline-evil plantuml-mode php-refactor-mode php-mode ox-gfm org-pandoc nav monochrome-theme minimal-theme minimal-session-saver memoize markdown-mode magit key-chord json-mode js2-refactor js-doc ipython idle-highlight-mode helm-projectile helm-open-github helm-ag gotests gotest go-projectile go-impl flycheck fill-column-indicator exec-path-from-shell evil-surround evil-leader etags-table etags-select elixir-yasnippets el-get ein dash-functional company-go coffee-mode better-defaults base16-theme alchemist airline-themes ag))))

;; key bindings
(eval-after-load "helm-gtags"
  '(progn
     (define-key helm-gtags-mode-map (kbd "M-t") 'helm-gtags-find-tag)
     (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
     (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
     (define-key helm-gtags-mode-map (kbd "M-g M-p") 'helm-gtags-parse-file)
     (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
     (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
     (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)))

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-display-header-line              nil
      helm-autoresize-max-height            25
      helm-autoresize-max-height            25
      helm-ff-file-name-history-use-recentf t)

(set-face-attribute 'helm-selection nil
                    :background "gray10"
                    :foreground "gray90")

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

(setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x

(global-set-key (kbd "C-x b") 'helm-mini)
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)

(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; Utility text functions
;; ----------------------------------------------------------------------
;; mapcar-head Like MAPCAR, but applies a different function to the first element.
(defun mapcar-head (fn-head fn-rest list)
      "like MAPCAR, but applies a different function to the first element."
      (if list
          (cons (funcall fn-head (car list)) (mapcar fn-rest (cdr list)))))

;; ----------------------------------------------------------------------
;; camelcase-region Given a region of text in snake_case format,
;; changes it to camelCase.
(defun camelcase-region (start end)
  "Changes region from snake_case to camelCase"
  (interactive "r")
  (save-restriction (narrow-to-region start end)
                    (goto-char (point-min))
                    (while (re-search-forward "_\\(.\\)" nil t)
                      (replace-match (upcase (match-string 1))))))

;; ----------------------------------------------------------------------
;; camelcase-string Given a string in snake_case format, change it to camelCase
(defun camelcase-string (s)
  "Changes string from snake_case to camelCase"
  (mapconcat 'identity (mapcar-head
                                '(lambda (word) (downcase word))
                                '(lambda (word) (capitalize (downcase word)))
                                (split-string s "_")) ""))

;; ----------------------------------------------------------------------
;; cadged largely from http://xahlee.org/emacs/elisp_idioms.html:
;;
(defun camelcase-word-or-region ()
  "Changes word or region from snake_case to camelCase"
  (interactive)
  (let (pos1 pos2 bds)
    (if (and transient-mark-mode mark-active)
        (setq pos1 (region-beginning) pos2 (region-end))
      (progn
        (setq bds (bounds-of-thing-at-point 'symbol))
        (setq pos1 (car bds) pos2 (cdr bds))))
    (camelcase-region pos1 pos2)))

;; ----------------------------------------------------------------------
;; snakecase-region
;; Given a region of text in camelCase format, changes it to snake_case.
;;
;; BUG: This is actually just a repeat of camelcase-region!
(defun snakecase-region (start end)
  "Changes region from camelCase to snake_case"
  (interactive "r")
  (save-restriction (narrow-to-region start end)
                    (goto-char (point-min))
                    (while (re-search-forward "_\\(.\\)" nil t)
                      (replace-match (upcase (match-string 1))))))

;; ----------------------------------------------------------------------
;; Given a region of text in camelCase format, changes it to snake_case.
(defun snakecase-word-or-region ()
  "Changes word or region from camelCase to snake_case"
  (interactive)
  (let (pos1 pos2 bds)
    (if (and transient-mark-mode mark-active)
        (setq pos1 (region-beginning) pos2 (region-end))
      (progn
        (setq bds (bounds-of-thing-at-point 'symbol))
        (setq pos1 (car bds) pos2 (cdr bds))))
    (snakecase-region pos1 pos2)))


;;; YAsnippet
(require 'yasnippet)
(setq yas-snippet-dirs (concat user-emacs-directory "snippets/"))

(yas-reload-all)

(define-key yas-minor-mode-map [(tab)] nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "<C-tab>") 'yas-expand)
(define-key yas-minor-mode-map (kbd "<escape>") 'yas-exit-snippet)

(setq yas-prompt-functions '(yas-completing-prompt
                             yas-ido-prompt
                             yas-dropdown-prompt))

;;;;;;;;;;;;;;;;;
;; Go Specific ;;
;;;;;;;;;;;;;;;;;
(require 'go-mode)
(require 'go-rename)
(require 'go-projectile)
(require 'gotests)
(require 'company-go)
(require 'gorepl-mode)

;; Load in GOPATH from the environment
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
(exec-path-from-shell-copy-env "GOPATH")
(exec-path-from-shell-copy-env "GOROOT")

(setq company-go-show-annotation t)

;; Custom go-mode hook
(add-hook 'go-mode-hook (lambda ()
  "Custom Go Mode Hook"

  ;; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")

  ;; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)

  (yas-minor-mode t)
  (yas-minor-mode-on)

  ;; Set the tab width to 4, which shouldn't impact gofmt, but will make the
  ;; editor spacing seem saner
  (setq tab-width 4)

  ;; Key Bindings
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "C-c t .") 'go-test-current-test)
  (local-set-key (kbd "C-c t f") 'go-test-current-file)
  (local-set-key (kbd "C-c t p") 'go-test-current-project)))

(add-hook 'go-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends) (mapcar #'company/backend-with-yas '(company-go)))
            (company-mode)))

(add-hook 'go-mode-hook 'flycheck-mode)

;; make sure that go-test wraps lines
(add-hook 'go-test-mode-hook (lambda ()
                               (visual-line-mode t)))

(define-key go-mode-map (kbd "C-c C-s") 'gorepl-run)
(define-key go-mode-map (kbd "C-c C-z") 'gorepl-run)
(define-key go-mode-map (kbd "C-c C-l") #'gorepl-run-load-current-file)
(define-key go-mode-map (kbd "C-c C-e") #'gorepl-eval-region)
(define-key go-mode-map (kbd "C-c C-r") #'gorepl-eval-line)

;;;;;;;;;;;;;;
;; PHP-Mode ;;
;;;;;;;;;;;;;;

;; Note that there's a php doc in the custom directory loaded from https://gist.github.com/stesie/6564885
(require 'php-mode)
(require 'php-extras)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-hook 'php-mode-hook
          (lambda ()
            (yas-minor-mode t)
            (yas-minor-mode-on)
            (php-enable-psr2-coding-style)
            (helm-gtags-mode)

            (setq indent-tabs-mode nil)
            (setq tab-width 4)
            (setq c-basic-offset 4)

            (setq-local php-insert-doc-access-tag   nil)
            (setq-local flycheck-phpcs-standard     "PSR2")
            (setq-local flycheck-phpmd-rulesets     '("codesize" "design" "naming" "unusedcode"))))

(add-hook 'php-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends)
                   '((php-extras-company company-dabbrev-code) company-capf company-files))))
;;;;;;;;;;;;;
;; JS2-Mode ;;
;;;;;;;;;;;;;;
(add-to-list
 'auto-mode-alist
 '("\\.js\\'" . js2-jsx-mode))

(setq-default
 js-indent-level 2
 js2-basic-offset 2
 js2-indent-switch-body t
 js2-pretty-multiline-declarations 'dynamic
 ;; Supress js2 mode errors
 js2-mode-show-parse-errors nil
 js2-mode-show-strict-warnings)

(eval-after-load
    'flycheck
  (lambda ()
    (flycheck-add-mode 'javascript-eslint 'js2-mode)
    ;; Disable jshint
    (setq-default
     flycheck-disabled-checkers
     (append flycheck-disabled-checkers
             '(javascript-jshint)))))

(defun my-javascript-mode-hook ()
  (js2-refactor-mode 1))

(add-hook
 'js2-mode-hook
 'my-javascript-mode-hook)

;;;;;;;;;;;;;;;;;
;; Coffee-Mode ;;
;;;;;;;;;;;;;;;;;

(require 'coffee-mode)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(setq coffee-tab-width 2)

;;;;;;;;;;;;;;
;; Web-Mode ;;
;;;;;;;;;;;;;;

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.jinja$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.less?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.sass?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
(sp-local-pair 'web-mode "{" "}" :actions nil)

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
)
(add-hook 'web-mode-hook  'my-web-mode-hook)

;; If there are two buffers, toggle between horizontal and vertical splitting
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
         (next-win-buffer (window-buffer (next-window)))
         (this-win-edges (window-edges (selected-window)))
         (next-win-edges (window-edges (next-window)))
         (this-win-2nd (not (and (<= (car this-win-edges)
                     (car next-win-edges))
                     (<= (cadr this-win-edges)
                     (cadr next-win-edges)))))
         (splitter
          (if (= (car this-win-edges)
             (car (window-edges (next-window))))
          'split-window-horizontally
        'split-window-vertically)))
    (delete-other-windows)
    (let ((first-win (selected-window)))
      (funcall splitter)
      (if this-win-2nd (other-window 1))
      (set-window-buffer (selected-window) this-win-buffer)
      (set-window-buffer (next-window) next-win-buffer)
      (select-window first-win)
      (if this-win-2nd (other-window 1))))))

(defun org-capture-arrange-windows-horizontally ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
         (next-win-buffer (window-buffer (next-window)))
         (this-win-edges (window-edges (selected-window)))
         (next-win-edges (window-edges (next-window)))
         (this-win-2nd (not (and (<= (car this-win-edges)
                     (car next-win-edges))
                     (<= (cadr this-win-edges)
                     (cadr next-win-edges))))))
    (delete-other-windows)
    (let ((first-win (selected-window)))
      (split-window-horizontally)
      (if this-win-2nd (other-window 1))
      (set-window-buffer (selected-window) this-win-buffer)
      (set-window-buffer (next-window) next-win-buffer)
      (select-window first-win)
      (if this-win-2nd (other-window 1))))))

;;;;;;;;;;;;;;
;; Org Mode ;;
;;;;;;;;;;;;;;
(require 'org)

(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)

(define-key org-mode-map (kbd"C-c l") 'org-store-link)

(define-key org-mode-map "\M-q" 'toggle-truncate-lines)

;; force UTF-8
(setq org-export-coding-system 'utf-8)

(setq org-directory "~/Dropbox/org/")

(setq org-default-notes-file (concat org-directory "refile.org"))

;; TODO: Better display of this list
;; Also, figure out colours for each tag
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "CANCELED")))

(setq org-use-fast-todo-selection t)
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

;; Split org-agenda windows in a reasonable manner
(setq org-agenda-window-setup 'current-window)
(setq org-agenda-files (list org-directory))

;; Patch org-capture to use vertical split window
(defadvice org-capture (after org-capture-arrange-windows activate)
  (org-capture-arrange-windows-horizontally))


(setq org-agenda-view-columns-initially nil)

;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
(setq org-capture-templates
      (quote (("t" "Todo" entry (file (concat org-directory "todo.org"))
               "* TODO %?\nCREATED: %U\n")
              ("n" "Note" entry (file (concat org-directory "refile.org"))
               "* %? :NOTE:\n%U\n%a\n")
              ("i" "Idea" entry (file (concat org-directory "refile.org"))
               "* %? :IDEA:\n%U\n%a\n")
              ("d" "Diary" entry (file+datetree (concat org-directory "diary.org"))
               "* %?\n%U\n")
              ("b" "Book" entry (file (concat org-directory "books.org"))
               "* TODO %\\1 - %\\2%?\n%U\n:AUTHOR: %^{AUTHOR}\n:TITLE: %^{TITLE}\n")
              ("m" "Meeting" entry (file (concat org-directory "refile.org"))
               "* MEETING with %? :MEETING:\n%U"))))

(setq org-agenda-skip-scheduled-if-done t)

;; Custom Agenda Views
(setq org-agenda-custom-commands
      '(("d" "Daily agenda and all TODOs"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "High-priority unfinished tasks:")))
          (agenda ""
                  ((org-agenda-ndays 1)
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo 'any))))
          (alltodo ""
                   ((org-agenda-skip-function '(or (mcos/org-skip-if-habit)
                                                   (mcos/org-skip-if-priority ?A)
                                                   (org-agenda-skip-if nil '(scheduled deadline))))
                    (org-agenda-overriding-header "\nALL normal priority tasks:")))

          (agenda ""
                  ((org-agenda-ndays 1)
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'any))
                   (org-agenda-overriding-header "\nReminders for today:")))
          (todo "DONE"
                ((org-agenda-skip-function 'mcos/org-skip-if-not-closed-today)
                 (org-agenda-overriding-header "\nClosed today:"))
                )
          )
         ((org-agenda-compact-blocks t)))
        ("w" "Weekly agenda and all TODOs"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "High-priority unfinished tasks:")))
          (agenda ""
                  ((org-agenda-span 'week)
                   (org-agenda-start-on-weekday 0)
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo 'any))))
          (alltodo ""
                   ((org-agenda-skip-function '(or (mcos/org-skip-if-habit)
                                                   (mcos/org-skip-if-priority ?A)
                                                   (org-agenda-skip-if nil '(scheduled deadline))))
                    (org-agenda-overriding-header "\nALL normal priority tasks:")))

          (agenda ""
                  ((org-agenda-span 'week)
                   (org-agenda-start-on-weekday 0)
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'any))
                   (org-agenda-overriding-header "\nReminders for this week:")))
          (todo "DONE"
                ((org-agenda-overriding-header "\nClosed this week:"))
                )
          )
         ((org-agenda-compact-blocks t)))))

(add-to-list 'org-agenda-custom-commands
             `("f." "Today"
               ((agenda ""
                        ((org-agenda-entry-types '(:timestamp :sexp))
                         (org-agenda-overriding-header
                          (concat "CALENDAR Today"
                                  (format-time-string "%a %d" (current-time))))
                         (org-agenda-span 'day)))
                (tags-todo "DEADLINE=\"<+0d>\""
                           ((org-agenda-overriding-header "DUE TODAY")
                            (org-agenda-skip-function
                             '(org-agenda-skip-if nil '(deadline)))
                            (org-agenda-sorting-strategy '(priority-down))))
                (tags-todo "DEADLINE<\"<+0d>\""
                           ((org-agenda-overriding-header "OVERDUE")
                            (org-agenda-skip-function
                             '(org-agenda-skip-if nil '(deadline)))
                            (org-agenda-sorting-strategy '(priority-down))))
                (agenda ""
                        ((org-agenda-entry-types '(:scheduled))
                         (org-agenda-overriding-header "SCHEDULED")
                         (org-agenda-skip-function
                          '(org-agenda-skip-entry-if 'todo 'done))
                         (org-agenda-sorting-strategy
                          '(priority-down time-down))
                         (org-agenda-span 'day)
                         (org-agenda-start-on-weekday nil)
                         (org-agenda-time-grid nil)))
                (todo "DONE"
                      ((org-agenda-skip-function 'mcos/org-skip-if-not-closed-today)
                       (org-agenda-overriding-header "COMPLETED TODAY"))))
               ((org-agenda-format-date "")
                (org-agenda-start-with-clockreport-mode nil)
                (org-agenda-compact-blocks nil))) t)

;; Set up a strike-through font for "DONE" items
(set-face-attribute 'org-agenda-done nil :strike-through t)

(defadvice enable-theme (after org-strike-done activate)
  "Setup org-agenda-done faces to have strike-through on"
  (and (message "Running advice")
       (set-face-attribute 'org-agenda-done nil :strike-through t)))

;; Optimize indentation for outline-style documents
(setq org-startup-indented t)

;; Autohide leading stars
(setq org-hide-leading-stars t)

;; alphabetical lists
(setq org-alphabetical-lists t)

(setq org-indent-mode t)

;; wrap the lines correctly
(setq org-startup-truncated nil)

(setq org-blank-before-new-entry (quote ((heading) (plain-list-item))))

;; Refile
;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq helm-org-format-outline-path nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-enforce-todo-dependencies t)

;; Logging state changes
(setq org-log-done t)
(setq org-log-reschedule (quote time))
(setq org-log-redeadline (quote time))
(setq org-log-into-drawer t)

(defun mcos/org-skip-if-not-closed-today (&optional subtree)
  "Skip entries that were not closed today.
Skip the current entry unless SUBTREE is not nil, in which case skip
the entire subtree."
  (let ((end (if subtree (subtree-end (save-excursion (org-end-of-subtree t)))
               (save-excursion (progn (outline-next-heading) (1- (point))))))
        (today-prefix (format-time-string "%Y-%m-%d")))
    (if (save-excursion
          (and (re-search-forward org-closed-time-regexp end t)
               (string= (substring (match-string-no-properties 1) 0 10) today-prefix)))
        nil
      end)))

(defun mcos/org-skip-if-habit (&optional subtree)
  "Skip an agenda entry if it has a STYLE property equal to \"habit\".
Skip the current entry unless SUBTREE is not nil, in which case skip
the entire subtree."
  (let ((end (if subtree (subtree-end (save-excursion (org-end-of-subtree t)))
                (save-excursion (progn (outline-next-heading) (1- (point)))))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        end
      nil)))

(defun mcos/org-skip-if-priority (priority &optional subtree)
  "Skip an agenda item if it has a priority of PRIORITY.
PRIORITY may be one of the characters ?A, ?B, or ?C.
Skips the current entry unless SUBTREE is not nil."
  (let ((end (if subtree (subtree-end (save-excursion (org-end-of-subtree t)))
                (save-excursion (progn (outline-next-heading) (1- (point))))))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        end
      nil)))

;; GPG Agent Info
;; Used by magit when determining whether to sign commits
(exec-path-from-shell-copy-env "GPG_AGENT_INFO")

;; ELIXIR
(require 'elixir-mode)

;; Must set up alchemist mode variables before enabling alchemist-mode
;; Use a different keybinding prefix than C-c a
(setq alchemist-key-command-prefix (kbd "C-c ,"))

;; Add alchemist-mode as a hook to elixir, rather than using `require`
(add-hook 'elixir-mode-hook 'alchemist-mode)

(sp-with-modes '(elixir-mode)
  (sp-local-pair "fn" "end"
         :when '(("SPC" "RET"))
         :actions '(insert navigate))
  (sp-local-pair "do" "end"
         :when '(("SPC" "RET"))
         :post-handlers '(sp-ruby-def-post-handler)
         :actions '(insert navigate)))

;; Protobuf
(require 'protobuf-mode)
(defconst my-protobuf-style
  '((c-basic-offset . 2)
    (indent-tabs-mode . nil)))

(add-hook 'protobuf-mode-hook
  (lambda () (c-add-style "my-style" my-protobuf-style t)))

;; Terraform
(require 'terraform-mode)

;; NOTE: This is so that emacs doesn't go looking for a TAGS file in the wrong place.
(setq tags-table-list nil)
