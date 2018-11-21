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
                      elixir-mode
                      elixir-yasnippets
                      alchemist
                      nav
                      ox-gfm ;; Export org as github flavored markdown
                      yaml-mode
                      protobuf-mode
                      gotests
                      terraform-mode
                      go-impl
                      godoctor
                      helm-gtags
                      gorepl-mode
                      evil-visualstar
                      gist
                      dockerfile-mode
                      helm-cider
                      evil-paredit
                      clj-refactor
                      flycheck-clojure
                      hl-sexp
                      groovy-mode
                      py-autopep8
                      elpy
                      org-gcal
                      planet-theme
                      jinja2-mode
                      rainbow-delimiters
                      pretty-mode
                      thrift
                      graphql-mode
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

(defun load-directory (dir)
      (let ((load-it (lambda (f)
		       (load-file (concat (file-name-as-directory dir) f)))
		     ))
	(mapc load-it (directory-files dir nil "\\.el$"))))

(setq custom-dir (expand-file-name "custom" dotfiles-dir))
(when (file-exists-p custom-dir)
  (load-directory custom-dir))

;; This function replaces modes in some alist with another mode
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

;; Theme - planet theme (added 2017-12-29)
(require 'planet-theme)
(load-theme 'planet t)

;; Turn off the silly startup message
(setq inhibit-startup-message t)

;; Pretty font ligatures and stuff
(require 'pretty-mode)
(global-pretty-mode t)
(global-prettify-symbols-mode 1)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;;; Fira code
;; This works when using emacs --daemon + emacsclient
(add-hook 'after-make-frame-functions (lambda (frame) (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")))
;; This works when using emacs without server/client
(set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")
;; I haven't found one statement that makes both of the above situations work, so I use both for now
(defconst fira-code-font-lock-keywords-alist
  (mapcar (lambda (regex-char-pair)
            `(,(car regex-char-pair)
              (0 (prog1 ()
                   (compose-region (match-beginning 1)
                                   (match-end 1)
                                   ;; The first argument to concat is a string containing a literal tab
                                   ,(concat "	" (list (decode-char 'ucs (cadr regex-char-pair)))))))))
          '(("\\(www\\)"                   #Xe100)
            ("[^/]\\(\\*\\*\\)[^/]"        #Xe101)
            ("\\(\\*\\*\\*\\)"             #Xe102)
            ("\\(\\*\\*/\\)"               #Xe103)
            ("\\(\\*>\\)"                  #Xe104)
            ("[^*]\\(\\*/\\)"              #Xe105)
            ("\\(\\\\\\\\\\)"              #Xe106)
            ("\\(\\\\\\\\\\\\\\)"          #Xe107)
            ("\\({-\\)"                    #Xe108)
            ("\\(\\[\\]\\)"                #Xe109)
            ("\\(::\\)"                    #Xe10a)
            ("\\(:::\\)"                   #Xe10b)
            ("[^=]\\(:=\\)"                #Xe10c)
            ("\\(!!\\)"                    #Xe10d)
            ("\\(!=\\)"                    #Xe10e)
            ("\\(!==\\)"                   #Xe10f)
            ("\\(-}\\)"                    #Xe110)
            ("\\(--\\)"                    #Xe111)
            ("\\(---\\)"                   #Xe112)
            ("\\(-->\\)"                   #Xe113)
            ("[^-]\\(->\\)"                #Xe114)
            ("\\(->>\\)"                   #Xe115)
            ("\\(-<\\)"                    #Xe116)
            ("\\(-<<\\)"                   #Xe117)
            ("\\(-~\\)"                    #Xe118)
            ("\\(#{\\)"                    #Xe119)
            ("\\(#\\[\\)"                  #Xe11a)
            ("\\(##\\)"                    #Xe11b)
            ("\\(###\\)"                   #Xe11c)
            ("\\(####\\)"                  #Xe11d)
            ("\\(#(\\)"                    #Xe11e)
            ("\\(#\\?\\)"                  #Xe11f)
            ("\\(#_\\)"                    #Xe120)
            ("\\(#_(\\)"                   #Xe121)
            ("\\(\\.-\\)"                  #Xe122)
            ("\\(\\.=\\)"                  #Xe123)
            ("\\(\\.\\.\\)"                #Xe124)
            ("\\(\\.\\.<\\)"               #Xe125)
            ("\\(\\.\\.\\.\\)"             #Xe126)
            ("\\(\\?=\\)"                  #Xe127)
            ("\\(\\?\\?\\)"                #Xe128)
            ("\\(;;\\)"                    #Xe129)
            ("\\(/\\*\\)"                  #Xe12a)
            ("\\(/\\*\\*\\)"               #Xe12b)
            ("\\(/=\\)"                    #Xe12c)
            ("\\(/==\\)"                   #Xe12d)
            ("\\(/>\\)"                    #Xe12e)
            ("\\(//\\)"                    #Xe12f)
            ("\\(///\\)"                   #Xe130)
            ("\\(&&\\)"                    #Xe131)
            ("\\(||\\)"                    #Xe132)
            ("\\(||=\\)"                   #Xe133)
            ("[^|]\\(|=\\)"                #Xe134)
            ("\\(|>\\)"                    #Xe135)
            ("\\(\\^=\\)"                  #Xe136)
            ("\\(\\$>\\)"                  #Xe137)
            ("\\(\\+\\+\\)"                #Xe138)
            ("\\(\\+\\+\\+\\)"             #Xe139)
            ("\\(\\+>\\)"                  #Xe13a)
            ("\\(=:=\\)"                   #Xe13b)
            ("[^!/]\\(==\\)[^>]"           #Xe13c)
            ("\\(===\\)"                   #Xe13d)
            ("\\(==>\\)"                   #Xe13e)
            ("[^=]\\(=>\\)"                #Xe13f)
            ("\\(=>>\\)"                   #Xe140)
            ("\\(<=\\)"                    #Xe141)
            ("\\(=<<\\)"                   #Xe142)
            ("\\(=/=\\)"                   #Xe143)
            ("\\(>-\\)"                    #Xe144)
            ("\\(>=\\)"                    #Xe145)
            ("\\(>=>\\)"                   #Xe146)
            ("[^-=]\\(>>\\)"               #Xe147)
            ("\\(>>-\\)"                   #Xe148)
            ("\\(>>=\\)"                   #Xe149)
            ("\\(>>>\\)"                   #Xe14a)
            ("\\(<\\*\\)"                  #Xe14b)
            ("\\(<\\*>\\)"                 #Xe14c)
            ("\\(<|\\)"                    #Xe14d)
            ("\\(<|>\\)"                   #Xe14e)
            ("\\(<\\$\\)"                  #Xe14f)
            ("\\(<\\$>\\)"                 #Xe150)
            ("\\(<!--\\)"                  #Xe151)
            ("\\(<-\\)"                    #Xe152)
            ("\\(<--\\)"                   #Xe153)
            ("\\(<->\\)"                   #Xe154)
            ("\\(<\\+\\)"                  #Xe155)
            ("\\(<\\+>\\)"                 #Xe156)
            ("\\(<=\\)"                    #Xe157)
            ("\\(<==\\)"                   #Xe158)
            ("\\(<=>\\)"                   #Xe159)
            ("\\(<=<\\)"                   #Xe15a)
            ("\\(<>\\)"                    #Xe15b)
            ("[^-=]\\(<<\\)"               #Xe15c)
            ("\\(<<-\\)"                   #Xe15d)
            ("\\(<<=\\)"                   #Xe15e)
            ("\\(<<<\\)"                   #Xe15f)
            ("\\(<~\\)"                    #Xe160)
            ("\\(<~~\\)"                   #Xe161)
            ("\\(</\\)"                    #Xe162)
            ("\\(</>\\)"                   #Xe163)
            ("\\(~@\\)"                    #Xe164)
            ("\\(~-\\)"                    #Xe165)
            ("\\(~=\\)"                    #Xe166)
            ("\\(~>\\)"                    #Xe167)
            ("[^<]\\(~~\\)"                #Xe168)
            ("\\(~~>\\)"                   #Xe169)
            ("\\(%%\\)"                    #Xe16a)
           ;; ("\\(x\\)"                   #Xe16b) This ended up being hard to do properly so i'm leaving it out.
            ("[^:=]\\(:\\)[^:=]"           #Xe16c)
            ("[^\\+<>]\\(\\+\\)[^\\+<>]"   #Xe16d)
            ("[^\\*/<>]\\(\\*\\)[^\\*/<>]" #Xe16f))))
(defun add-fira-code-symbol-keywords ()
  (font-lock-add-keywords nil fira-code-font-lock-keywords-alist))
(add-hook 'prog-mode-hook
          #'add-fira-code-symbol-keywords)

(add-hook 'helm-major-mode-hook
          (lambda ()
            (setq auto-composition-mode nil)))

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

;; Rainbow Delimiters when in prog mode
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;;;;;;;;;;;;;
;; Backups ;;
;;;;;;;;;;;;;

(setq version-control t     ;; Use version numbers for backups.
      kept-new-versions 9  ;; Number of newest versions to keep.
      kept-old-versions 6   ;; Number of oldest versions to keep.
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
      delete-old-versions t ;; Don't ask to delete excess backup versions.
      backup-by-copying t)  ;; Copy all files, don't rename them.

(setq vc-make-backup-files t)

;; Default and per-save backups go here:
;; (setq backup-directory-alist '(("." . ,(concat user-emacs-directory "backups/per-save"))))

(setq backup-directory-alist
          `(("." . ,(concat user-emacs-directory "backups/per-save"))))


(defun force-backup-of-buffer ()
  ;; Make a special "per session" backup at the first save of each
  ;; emacs session.
  (when (not buffer-backed-up)
    ;; Override the default parameters for per-session backups.
    (let ((backup-directory-alist '(("" . "~/.emacs.d/backup/per-session")))
          (kept-new-versions 3))
      (backup-buffer)))
  ;; Make a "per save" backup on each save.  The first save results in
  ;; both a per-session and a per-save backup, to keep the numbering
  ;; of per-save backups consistent.
  (let ((buffer-backed-up nil))
    (backup-buffer)))

(add-hook 'before-save-hook  'force-backup-of-buffer)

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

;; From https://github.com/Fuco1/smartparens/issues/286
(defun sp--org-skip-markup (ms mb me)
  (save-excursion
    (and (progn
           (goto-char mb)
           (save-match-data (looking-back "\\sw\\|\\s_\\|\\s.")))
         (progn
           (goto-char me)
           (save-match-data (looking-at "\\sw\\|\\s_\\|\\s."))))))

;; From https://github.com/Fuco1/smartparens/issues/286
(sp-with-modes sp--lisp-modes
  ;; disable ', it's the quote character!
  (sp-local-pair "'" nil :actions nil)
  ;; also only use the pseudo-quote inside strings where it serve as
  ;; hyperlink.
  (sp-local-pair "`" "'" :when '(sp-in-string-p sp-in-comment-p))
  (sp-local-pair "`" nil
                 :skip-match (lambda (ms mb me)
                               (cond
                                ((equal ms "'")
                                 (or (sp--org-skip-markup ms mb me)
                                     (not (sp-point-in-string-or-comment))))
                                (t (not (sp-point-in-string-or-comment)))))))


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

; (setq-default flycheck-disabled-checkers '(go-build go-errcheck go-unconvert go-test json-jsonlist javascript-jshint))
(setq-default flycheck-disabled-checkers '(json-jsonlist javascript-jshint go-megacheck))
(setq-default flycheck-check-syntax-automatically '(save idle-change new-line))

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
    ("a56a6bf2ecb2ce4fa79ba636d0a5cf81ad9320a988ec4e55441a16d66b0c10e0" "115d42fa02a5ce6a759e38b27304e833d57a48422c2408d5455f14450eb96554" "43c1a8090ed19ab3c0b1490ce412f78f157d69a29828aa977dae941b994b4147" "71b9b4c5d2a5126586d204e20c3fb4797f70d3d057a0c8b03bac2c51893007a2" "c4bd8fa17f1f1fc088a1153ca676b1e6abc55005e72809ad3aeffb74bd121d23" "2bdd513c17d3e7768bbc86adebfe9419169e92f1bf17bfc6c8f15a10c82c4a4d" "7838757247452123ec287fd978797f63294f6d8b26b300bb883131f5b6face54" "b749694d80fcaa9bd917c83a8f83729cdd2a79d2e60d384459eeca17b56b7bb6" "cc0dbb53a10215b696d391a90de635ba1699072745bf653b53774706999208e3" default)))
 '(helm-gtags-auto-update t)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-path-style (quote relative))
 '(nav-width 25)
 '(package-selected-packages
   (quote
    (graphql-mode thrift pretty-mode rainbow-delimiters jinja2-mode planet-theme org-gcal highlight-indentation anaconda-mode elpy py-autopep8 groovy-mode hl-sexp flycheck-clojure clj-refactor evil-paredit helm-cider cider dockerfile-mode gist evil-visualstar gorepl-mode php-extras ede-php-autoload deferred helm-go-package godoctor flycheck-protobuf helm-gtags ggtags php-eldoc yaml-mode xcscope ws-butler web-mode vagrant use-package terraform-mode smartparens smart-mode-line-powerline-theme pydoc-info protobuf-mode powerline-evil plantuml-mode php-refactor-mode php-mode ox-gfm nav minimal-session-saver memoize markdown-mode magit key-chord json-mode js2-refactor js-doc ipython idle-highlight-mode helm-projectile helm-open-github helm-ag gotests gotest go-projectile go-impl flycheck fill-column-indicator exec-path-from-shell evil-surround evil-leader etags-table etags-select elixir-yasnippets el-get ein dash-functional company-go coffee-mode better-defaults base16-theme alchemist airline-themes ag)))
 '(safe-local-variable-values
   (quote
    ((dockerfile-image-name . "entities/protoc-go")
     (dockerfile-image-name . "entities/protoc-python")
     (dockerfile-image-name . "robin/robin-email-service")
     (dockerfile-image-name . "recurrence_expander")
     (dockerfile-image-name . "entities/protoc")
     (dockerfile-image-name . "entities/protoc-php")
     (docker-image-name . "recurrence_expander")
     (docker-image-name . "entities/protoc")
     (docker-image-name . "entities/protoc-go")
     (docker-image-name . "entities/protoc-python")
     (docker-image-name . "entities/protoc-php"))))
 '(show-paren-mode t))

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
(add-to-list 'yas-snippet-dirs (concat user-emacs-directory "snippets/"))

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

(exec-path-from-shell-copy-env "GOPATH")
(exec-path-from-shell-copy-env "GOROOT")

(setq company-go-show-annotation t)

;; prevent go-projectile from screwing up GOPATH
(setq go-projectile-switch-gopath 'never)

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

  ;; eldoc shows the signature of the function at point in the status bar.
  (go-eldoc-setup)

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

(setq org-directory "/Users/mark/Dropbox/org/")

(setq org-default-notes-file (concat org-directory "refile.org"))

;; Set up google calendar sync
(require 'org-gcal)
(setq org-gcal-client-id creds-gcal-client-id
      org-gcal-client-secret creds-gcal-client-secret
      org-gcal-file-alist `((,creds-gcal-calendar-id . ,(concat org-directory creds-gcal-org-path))))

;; Split org-agenda windows in a reasonable manner
(setq org-agenda-window-setup 'current-window)
(setq org-agenda-files (list
                        org-directory
                        (concat org-directory "robin/")
                        (concat org-directory "cal/")
                        ))

;; Patch org-capture to use vertical split window
(defadvice org-capture (after org-capture-arrange-windows activate)
  (org-capture-arrange-windows-horizontally))

;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
(setq org-capture-templates
      (quote (("t" "Todo" entry (file "")
               "* TODO %?\nCREATED: %U\n"))))

;; Other possible capture templates:
;; ("n" "Note" entry (file (concat org-directory "refile.org"))
;;  "* %? :NOTE:\n%U\n%a\n")
;; ("i" "Idea" entry (file (concat org-directory "refile.org"))
;;  "* %? :IDEA:\n%U\n%a\n")
;; ("d" "Diary" entry (file+datetree (concat org-directory "diary.org"))
;;  "* %?\n%U\n")
;; ("b" "Book" entry (file (concat org-directory "books.org"))
;;  "* TODO %\\1 - %\\2%?\n%U\n:AUTHOR: %^{AUTHOR}\n:TITLE: %^{TITLE}\n")
;; ("m" "Meeting" entry (file (concat org-directory "refile.org"))
;;  "* MEETING with %? :MEETING:\n%U")

;; TODO: Better display of this list
;; '!' (for a timestamp) or '@' (for a note with timestamp)
(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s!)" "WAITING(w@)" "DELEGATED(l@)" "|" "DONE(d!)" "CANCELED(c@)")))

(setq org-use-fast-todo-selection t)
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

(setq org-agenda-view-columns-initially nil)
(setq org-agenda-span 7)
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-show-all-dates t)
(setq org-reverse-note-order t)
(setq org-fast-tag-selection-single-key (quote expert))
(setq org-fontify-done-headline t)
(setq org-agenda-skip-scheduled-if-deadline-is-shown 'not-today)

;; Simply separate agenda blocks with newlines
(setq org-agenda-block-separator "")
;; set up a strike-through font for "DONE" items
(set-face-attribute 'org-agenda-done nil :strike-through t)
(set-face-attribute 'org-headline-done nil :strike-through t)
(set-face-attribute 'org-done nil :strike-through t)

(defadvice enable-theme (after org-strike-done activate)
  "Setup org-agenda-done faces to have strike-through on"
  (and (message "Running advice")
       (set-face-attribute 'org-agenda-done nil :strike-through t)
       (set-face-attribute 'org-headline-done nil :strike-through t)
       (set-face-attribute 'org-done nil :strike-through t)))

(setq org-agenda-custom-commands
      '(("d" todo "DELEGATED" nil)
       ("c" todo "DONE|DEFERRED|CANCELLED" nil)
       ("w" todo "WAITING" nil)
       ("F" "Four Week Agenda" agenda ""
        ((org-agenda-span 28)
         (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("DONE" "CANCELED"))))
        (org-agenda-overriding-header "Four Week Task List"))
       ("k" "Standup (Kanban)" agenda ""
        ((org-agenda-start-day "-1d")
         (org-agenda-span 3) ;; yesterday, today, tomorrow.
         (org-agenda-start-on-weekday nil))
        (org-agenda-overriding-header "Standup Report"))
       ("W" "Week Agenda" agenda ""
        ((org-agenda-span 7)
         (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("DONE" "CANCELED"))))
        (org-agenda-overriding-header "Weekly Task List"))
       ("A" "Today's Priority #A Tasks" agenda ""
        ((org-agenda-skip-function
          (lambda nil
            (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#A\\]")))
         (org-agenda-span 1)
         (org-agenda-overriding-header "Today's Priority #A tasks: ")))
       ("u" "Unscheduled TODO entries" alltodo ""
        ((org-agenda-skip-function
          (lambda nil
            (org-agenda-skip-entry-if (quote scheduled) (quote deadline)
                                      (quote regexp) "\n]+>")))
         (org-agenda-overriding-header "Unscheduled TODO entries: ")))
       ))

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
(setq org-log-done 'time)
(setq org-log-reschedule (quote time))
(setq org-log-redeadline (quote time))
(setq org-log-into-drawer nil)

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

;; Open org-src editing windows in the current window
(setq org-src-window-setup 'current-window)

(setq org-export-with-sub-superscripts '{})

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
(add-to-list 'auto-mode-alist '("\\.proto$" . protobuf-mode))
(defconst my-protobuf-style
  '((c-basic-offset . 2)
    (indent-tabs-mode . nil)))

(add-hook 'protobuf-mode-hook
  (lambda () (c-add-style "my-style" my-protobuf-style t)))

;; Terraform
(require 'terraform-mode)

;; Dockerfile
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
(add-to-list 'auto-mode-alist '("\\.docker$" . dockerfile-mode))

;; note: This is so that emacs doesn't go looking for a TAGS file in the wrong place.
(setq tags-table-list nil)
(put 'downcase-region 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;
;; Clojure and Lisps ;;
;;;;;;;;;;;;;;;;;;;;;;;
(require 'cider)

;; REPL related stuff

;; REPL history file
(setq cider-repl-history-file "~/.emacs.d/cider-history")

;; nice pretty printing
(setq cider-repl-use-pretty-printing t)

;; nicer font lock in REPL
(setq cider-repl-use-clojure-font-lock t)

;; result prefix for the REPL
(setq cider-repl-result-prefix ";; => ")

;; never ending REPL history
(setq cider-repl-wrap-history t)

;; looong history
(setq cider-repl-history-size 3000)

;; eldoc for clojure
(add-hook 'cider-mode-hook #'eldoc-mode)


;; error buffer not popping up
(setq cider-show-error-buffer nil)

;; company mode for completion
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)

(require 'clj-refactor)
(require 'seq-25) ;; temporary workaround for clj-refactor, see https://github.com/clojure-emacs/clj-refactor.el/issues/365

(add-hook 'clojure-mode-hook
	  (lambda ()
	    (clj-refactor-mode 1)
	    ;; insert keybinding setup here
	    (cljr-add-keybindings-with-prefix "C-c RET")))

(add-hook 'clojure-mode-hook #'yas-minor-mode)

;; no auto sort
(setq cljr-auto-sort-ns nil)

;; do not prefer prefixes when using clean-ns
(setq cljr-favor-prefix-notation nil)

(require 'flycheck-clojure)

(eval-after-load 'flycheck '(flycheck-clojure-setup))

(require 'hl-sexp)
(add-hook 'clojure-mode-hook #'hl-sexp-mode)
(add-hook 'lisp-mode-hook #'hl-sexp-mode)
(add-hook 'emacs-lisp-mode-hook #'hl-sexp-mode)

(require 'evil-paredit)
(add-hook 'lisp-mode-hook #'evil-paredit-mode)
(add-hook 'emacs-lisp-mode-hook #'evil-paredit-mode)
(add-hook 'clojure-mode-hook #'evil-paredit-mode)
(add-hook 'cider-repl-mode-hook #'evil-paredit-mode)

(add-to-list 'evil-surround-operator-alist
             '(evil-paredit-change . change))
(add-to-list 'evil-surround-operator-alist
             '(evil-paredit-delete . delete))

;;;;;;;;;;;;
;; Groovy ;;
;;;;;;;;;;;;

(require 'groovy-mode)
(add-to-list 'auto-mode-alist '("Jenkinsfile\\'" . groovy-mode))

(add-hook 'groovy-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq tab-width 4)
            (setq c-basic-offset 4)))

;;;;;;;;;;;;;
;; Python ;;
;;;;;;;;;;;;;

(require 'elpy)
(require 'py-autopep8)
(elpy-enable)
(add-hook 'elpy-mode-hook
          'py-autopep8-enable-on-save
          )

;;;;;;;;;;;;;
;; Thrift ;;
;;;;;;;;;;;;;

(require 'thrift)

(add-to-list 'auto-mode-alist '("\\.thrift\\'" . thrift-mode))

;;;;;;;;;;;;;
;; GraphQL ;;
;;;;;;;;;;;;;

(require 'graphql-mode)


;; do not try to guess the indent offset
;; Avoid this message: "Can’t guess python-indent-offset, using defaults: 4"
;; http://stackoverflow.com/questions/18778894/emacs-24-3-python-cant-guess-python-indent-offset-using-defaults-4
;; (setq python-indent-guess-indent-offset nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#192129" :foreground "#8898a9" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "nil" :family "Fira Code")))))
