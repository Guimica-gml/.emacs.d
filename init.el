;; I definitely want this here
;;(setq debug-on-error t)

;; Chage deafult messages
(setq initial-scratch-message "")
(defun display-startup-echo-area-message ()
  (message ""))

;; Disable some annoying stuff
(setq inhibit-splash-screen t)
(setq ring-bell-function 'ignore)
(setq use-dialog-box nil)

;; Disable unwanted modes
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(blink-cursor-mode 0)
(fringe-mode 0)

;; I really need some better subword navigation
(global-subword-mode)

;; Nice window movement keybinds
(windmove-default-keybindings)

;; Scrolling
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-scroll-amount '(0.05))

;; Save temporary files out of my current working directory please
(setq backup-directory-alist '(("." . "~/.emacs.d/tmp-files/")))
(setq auto-save-file-name-transforms `((".*" "~/.emacs.d/tmp-files/" t)))
(setq lock-file-name-transforms `((".*" "~/.emacs.d/tmp-files/" t)))

;; Use the best font ever
(set-frame-font "Iosevka Custom-14")

;; Ask for y/n instead of yes/no
(setq use-short-answers t)

;; Re-enable disabled commands
(put 'dired-find-alternate-file 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Typing over a selection deletes it
(delete-selection-mode t)

;; My own keybinds
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c d") 'dired-jump)
(global-set-key (kbd "C-c r") 'restart-emacs)
(global-set-key (kbd "C-c b m") 'bookmark-set)
(global-set-key (kbd "C-c b b") 'bookmark-jump)
(global-set-key (kbd "C-c b l") 'list-bookmarks)
(global-set-key (kbd "C-c b d") 'bookmark-delete)
(global-set-key (kbd "C-c g s") 'magit-stage-modified)
(global-set-key (kbd "C-c g c") 'magit-commit)
(global-set-key (kbd "C-c g p") 'magit-push)

;; Disable some keys
(global-unset-key (kbd "<left>"))
(global-unset-key (kbd "<right>"))
(global-unset-key (kbd "<up>"))
(global-unset-key (kbd "<down>"))
(global-unset-key (kbd "<C-left>"))
(global-unset-key (kbd "<C-right>"))
(global-unset-key (kbd "<C-up>"))
(global-unset-key (kbd "<C-down>"))
(global-unset-key (kbd "<M-left>"))
(global-unset-key (kbd "<M-right>"))
(global-unset-key (kbd "<M-up>"))
(global-unset-key (kbd "<M-down>"))
(global-unset-key (kbd "C-v"))
(global-unset-key (kbd "M-v"))

(defun my/insert-line-above ()
  "Inserts a line above the cursor"
  (interactive)
  (move-beginning-of-line nil)
  (whitespace-cleanup-region (line-beginning-position) (line-end-position))
  (open-line 1)
  (indent-for-tab-command))

(defun my/insert-line-below ()
  "Inserts a line below the cursor"
  (interactive)
  (move-end-of-line nil)
  (newline 1 1)
  (indent-for-tab-command))

;; Improve movementation
(global-set-key (kbd "C-S-f") 'forward-word)
(global-set-key (kbd "C-S-b") 'backward-word)
(global-set-key (kbd "C-S-n") 'forward-paragraph)
(global-set-key (kbd "C-S-p") 'backward-paragraph)

;; Redefine emacs commands
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-o") 'my/insert-line-above)
(global-set-key (kbd "C-S-o") 'my/insert-line-below)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-unset-key (kbd "C-x C-z"))

;; Line number
;;(global-display-line-numbers-mode)
;;(setq display-line-numbers-type 'relative)
;;(setq-default display-line-numbers-grow-only t)

;; Show column number
(column-number-mode)
(setq column-number-indicator-zero-based nil)

;; The tab size should always be 4
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Shell script settings
(setq sh-basic-offset 4)

;; C settings
(require 'cc-mode)
(setq c-basic-offset 4)
(add-to-list 'c-default-style '(c-mode . "k&r"))
(c-set-offset 'arglist-close 0)

;; Python settings
(setq python-indent-offset 4)
(setq python-indent-guess-indent-offset nil)

;; Auto-generated code
(setq custom-file "~/.emacs.d/emacs-custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

;; Add melpa to package manager
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; Activate all the packages (in particular autoloads)
(package-initialize)

;; List of packages
(setq my/packages
      '(ligature yasnippet smex restart-emacs multiple-cursors move-text magit company
        glsl-mode cobol-mode rust-mode lua-mode go-mode))

;; Fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; Install the missing packages
(dolist (package my/packages)
  (unless (package-installed-p package)
    (package-install package)))

;; Uninstall removed packages
(dolist (package package-selected-packages)
  (if (and (package-install-p package) (not (member package my/packages)))
      (package-delete package)))

;; Auto complete in the minibuffer
(require 'ido)
(ido-mode t)

;; More auto complete
(require 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Moving text around is useful
(require 'move-text)
(global-set-key (kbd "M-p") 'move-text-up)
(global-set-key (kbd "M-n") 'move-text-down)

(defun my/indent-region-advice (&rest ignored)
  (let ((deactivate deactivate-mark))
    (if (region-active-p)
        (indent-region (region-beginning) (region-end))
      (indent-region (line-beginning-position) (line-end-position)))
    (setq deactivate-mark deactivate)))

(advice-add 'move-text-up :after 'my/indent-region-advice)
(advice-add 'move-text-down :after 'my/indent-region-advice)

;; Some whitespace settings
(require 'whitespace)

(defun my/set-ws-colors (&rest ignored)
  (let ((ws-trailing "#f43841")
        (ws-foreground (face-attribute 'region :background))
        (ws-background (face-attribute 'default :background)))
    (set-face-attribute 'whitespace-trailing nil :background ws-trailing :foreground ws-trailing :underline nil)
    (set-face-attribute 'region nil :distant-foreground nil)
    (set-face-attribute 'whitespace-space nil :background ws-background :foreground ws-foreground)
    (set-face-attribute 'whitespace-tab nil :background ws-background :foreground ws-foreground)))

(my/set-ws-colors)
(advice-add 'load-theme :after 'my/set-ws-colors)

(setq whitespace-style
      '(face space-tab trailing tabs tab-mark))

(setq whitespace-global-modes
      '(not shell-mode help-mode magit-mode magit-diff-mode
            ibuffer-mode dired-mode occur-mode))

(global-set-key (kbd "C-c t") 'whitespace-cleanup)
(global-whitespace-mode)

;; Multplie cursors config
(require 'multiple-cursors)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C->") 'mc/mark-all-like-this)

;; Code auto complete
(require 'company)
(setq company-frontends '(company-echo-strip-common-frontend))
(setq company-backends '((company-capf company-dabbrev-code)))
(global-company-mode)

;; Snippets
(require 'yasnippet)
(yas-global-mode)

;; Font ligatures
(ligature-set-ligatures
 'prog-mode
 '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->" "<---->" "<!--"
   "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">=" "<=>" "<==>" "<===>" "<====>" "<!---"
   "<~~" "<~" "~>" "~~>" "::" ":::" "..." "==" "!=" "===" "!=="
   ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "+:" "-:" "=:"))
(global-ligature-mode)

;; Lode some extra modes and themes
(load "~/.emacs.d/modes/odin-mode.el")
(load "~/.emacs.d/modes/simpc-mode.el")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; Set theme
(load-theme 'yehsayer)
