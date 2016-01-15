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

;; Set fonts and stuff
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "gray7" :foreground "light gray" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "nil" :family "Menlo")))))

;; Need some Common Lisp Here
(require 'cl)

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

(setq-default blink-cursor-mode nil)
(setq evil-insert-state-cursor '(bar))

;; Evil Leader Mode
(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader ",")

;;;;;;;;;;;;;;;;;;;;;;
;; Multiple Cursors ;;
;;;;;;;;;;;;;;;;;;;;;;

(require 'multiple-cursors)

;; When you have an active region that spans multiple lines, the following will add a cursor to each line:
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

;; When you want to add multiple cursors not based on continuous lines, but based on keywords in the buffer
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


;; Smooth scrolling
(setq redisplay-dont-pause t
  scroll-margin 1
  scroll-step 1
  scroll-conservatively 10000
  scroll-preserve-screen-position 1
  scroll-up-aggressively 0.01
  scroll-down-aggressively 0.01)

;; ;; Powerline
;; (require 'powerline)
;; (powerline-default-theme)

;; Smart mode line

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("26614652a4b3515b4bbbb9828d71e206cc249b67c9142c06239ed3418eff95e2" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))))

;; (setq sml/theme 'automatic)
(require 'smart-mode-line-powerline-theme)
(setq sml/theme 'powerline)

;; Alternatives:
;; (sml/apply-theme 'powerline)
;; (setq sml/theme 'dark)
;; (sml/apply-theme 'light)
;; (setq sml/theme 'respectful)

(setq display-time-24hr-format t)
(display-time-mode 1)

(add-hook 'after-init-hook 'sml/setup)

;; Minor mode hiding
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
                     "$"
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

;; company-mode theme
(custom-theme-set-faces
 'minimal
   `(company-tooltip ((t (:weight bold :background ,"gray10" :foreground , "gray90"))))
   `(company-tooltip-common ((t (:inherit company-tooltip :foreground , "gray35"))))
   `(company-tooltip-selection ((t (:weight bold :foreground , "gray10" :background , "gray90"))))
   `(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :foreground , "gray35"))))
   `(company-tooltip-annotation ((t (:foreground , "gray90" :background , "gray10"))))

   `(company-scrollbar-fg ((t (:background , "gray40"))))
   `(company-scrollbar-bg ((t (:background , "gray95"))))
   `(company-preview ((t (:foreground , "red" :background nil)))))

;; Require Final Newline
(setq mode-require-final-newline t)

;; Reload All Changes From Disk
(global-auto-revert-mode t)

;; Turn off bells
(setq ring-bell-function 'ignore)

;; Color Theme
(load-theme 'minimal t)
(set-face-attribute 'fringe nil
                      :foreground (face-foreground 'default)
                      :background (face-background 'default))

;; Turn off the silly startup message
(setq inhibit-startup-message t)

;; Smart Parens
;; (require 'smartparens-config)
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
(desktop-save-mode 1)

;; GRAB THE PATH
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Turn on the zshell properly
(setenv "ESHELL" (concat dotfiles-dir "eshell"))

;; Flycheck Syntax Checking
(add-hook 'after-init-hook #'global-flycheck-mode)

(setq-default flycheck-disabled-checkers '(go-build go-test json-jsonlist))

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
(add-hook 'markdown-mode-hook 'turn-on-auto-fill)
(add-hook 'markdown-mode-hook
          '(lambda() (set-fill-column 100)))
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

(helm-mode 1)
(helm-autoresize-mode 1)

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
;; (require 'company-go)
(require 'go-eldoc)

;; Set the GOPATH to $HOME/.go
;; That's what I use as my global GOPATH, because I'm a monster
;; In the future, maybe I'll get around to running emacs in server mode, then
;; I won't have to do things like this all the time

(setenv "GOPATH" "$HOME/.go")

;; Go-oracle
(load-file (concat (getenv "GOPATH") "/src/golang.org/x/tools/cmd/oracle/oracle.el"))

;; Custom go-mode hook
(add-hook 'go-mode-hook (lambda ()
  "Custom Go Mode Hook"

  ;; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")

  ;; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)

  ;; Imenu & Speedbar
  (setq imenu-generic-expression
        '(("type" "^type *\\([^ \t\n\r\f]*\\)" 1)
          ("func" "^func *\\(.*\\) {" 1)))
  (imenu-add-to-menubar "Index")
  ;; (set (make-local-variable 'company-backends) '(company-go))

  ;; Go-Oracle-Mode
  (go-oracle-mode)

  ;; Go-eldoc
  (go-eldoc-setup)

  ;; Key Bindings
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "C-c t .") 'go-test-current-test)
  (local-set-key (kbd "C-c t f") 'go-test-current-file)
  (local-set-key (kbd "C-c t p") 'go-test-current-project)))

;;;;;;;;;;;;;;
;; PHP-Mode ;;
;;;;;;;;;;;;;;

;; Note that there's a php doc in the custom directory loaded from https://gist.github.com/stesie/6564885
(require 'php-mode)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-hook 'php-mode-hook
          (lambda ()
            (yas-minor-mode t)
            (yas-minor-mode-on)
            (php-enable-psr2-coding-style)

            (setq indent-tabs-mode nil)
            (setq tab-width 4)
            (setq c-basic-offset 4)

            (setq-local php-insert-doc-access-tag   nil)
            (setq-local flycheck-phpcs-standard     "PSR2")
            (setq-local flycheck-phpmd-rulesets     '("codesize" "design" "naming" "unusedcode"))))

;;;;;;;;;;;;;;
;; JS2-Mode ;;
;;;;;;;;;;;;;;

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx" . js2-mode))

(replace-auto-mode 'javascript-mode 'js2-mode)
(replace-auto-mode 'js-mode 'js2-mode)

;; js2 configuration
(setq js2-basic-offset 2)
(setq js2-highlight-external-variables nil)
(setq js2-highlight-level 1)
(setq js2-mode-show-parse-errors nil)
(setq js2-mode-show-strict-warnings nil)

(add-hook 'js2-mode-hook
          (lambda ()
            (define-key js2-mode-map "\C-ci" 'js-doc-insert-function-doc)
            (define-key js2-mode-map "@" 'js-doc-insert-tag)))

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
(sp-local-pair 'web-mode "{" "}" :actions nil)

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
)
(add-hook 'web-mode-hook  'my-web-mode-hook)

;;;;;;;;;;;;;;
;; Org Mode ;;
;;;;;;;;;;;;;;

(require 'org)

(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(define-key org-mode-map "\M-q" 'toggle-truncate-lines)

(setq org-log-done t)

(setq org-directory "~/Dropbox/org/")

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

(setq org-use-fast-todo-selection t)
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

;; Split org-agenda windows in a reasonable manner
(setq org-agenda-window-setup 'current-window)
(setq org-agenda-files (list org-directory))

(setq org-default-notes-file (concat org-directory "refile.org"))

;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
(setq org-capture-templates
      (quote (("t" "Todo" entry (file (concat org-directory "refile.org"))
               "* TODO %?\n%U\n%a\n")
              ("n" "Note" entry (file (concat org-directory "refile.org"))
               "* %? :NOTE:\n%U\n%a\n")
              ("i" "Idea" entry (file (concat org-directory "refile.org"))
               "* %? :IDEA:\n%U\n%a\n")
              ("j" "Journal" entry (file+datetree (concat org-directory "diary.org"))
               "* %?\n%U\n")
              ("m" "Meeting" entry (file (concat org-directory "refile.org"))
               "* MEETING with %? :MEETING:\n%U"))))

;; Optimize indentation for outline-style documents
(setq org-startup-indented t)

;; Autohide leading stars
(setq org-hide-leading-stars t)

;; alphabetical lists
(setq org-alphabetical-lists t)

(setq org-indent-mode t)

;; wrap the lines correctly
(setq org-startup-truncated nil)

;; Refile
;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

(setq company-global-modes '(not org-mode))

;;;;;;;;;;;;
;; Python ;;
;;;;;;;;;;;;
(require 'python)

(setq
 python-shell-interpreter "ipython"
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")


;; -----------------------------
;; emacs IPython notebook config
;; -----------------------------

; use autocompletion, but don't start to autocomplete after a dot
(setq ein:complete-on-dot -1)
(setq ein:use-auto-complete 1)

; set python console args
(setq ein:console-args
      '("--gui=osx" "--matplotlib=osx" "--colors=Linux"))

; timeout settings
(setq ein:query-timeout 1000)

; IPython notebook
(require 'ein)

; shortcut function to load notebooklist
(defun load-ein ()
  (ein:notebooklist-load)
  (interactive)
  (ein:notebooklist-open))


;; ------------------
;; misc python config
;; ------------------

; pydoc info
(require 'pydoc-info)

;; pyflakes flymake integration
;; http://stackoverflow.com/a/1257306/347942
;; (when (load "flymake" t)
;;   (defun flymake-pyflakes-init ()
;;     (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                        'flymake-create-temp-inplace))
;;            (local-file (file-relative-name
;;                         temp-file
;;                         (file-name-directory buffer-file-name))))
;;       (list "pycheckers" (list local-file))))
;;   (add-to-list 'flymake-allowed-file-name-masks
;;                '("\\.py\\'" flymake-pyflakes-init)))
;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             (unless (eq buffer-file-name nil) (flymake-mode 1))))

; Set PYTHONPATH, because we don't load .bashrc
(defun set-python-path-from-shell-PYTHONPATH ()
  (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PYTHONPATH'")))
    (setenv "PYTHONPATH" path-from-shell)))

(if window-system (set-python-path-from-shell-PYTHONPATH))

(setq auto-mode-alist
      (append
       (list '("\\.pyx" . python-mode)
             '("SConstruct" . python-mode))
       auto-mode-alist))

; keybindings
(eval-after-load 'python
  '(define-key python-mode-map (kbd "C-c !") 'python-shell-switch-to-shell))
(eval-after-load 'python
  '(define-key python-mode-map (kbd "C-c |") 'python-shell-send-region))
