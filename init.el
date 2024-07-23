;; Set up package.el to work with MELPA
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")))  ;; Add other repositories if needed
(package-initialize)
(package-refresh-contents)

(setq visible-bell t)
(menu-bar-mode -1)  ; Leave this one on if you're a beginner!
(tool-bar-mode -1)
(scroll-bar-mode -1)

(use-package command-log-mode)

(column-number-mode)
(global-display-line-numbers-mode t)

;; Remove messages from the *Messages* buffer.
(setq-default message-log-max nil)

;; Kill both buffers on startup.
;; (kill-buffer "*Messages*")
;; (kill-buffer "*scratch*")

;; functions ============================================
;; boostrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
      (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
        "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
        'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)
;; ======================================================

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;; terminal
(use-package vterm
    :ensure t)

;; theme
(straight-use-package 'catppuccin-theme)
(setq catppuccin-flavor 'mocha) ;; or 'latte, 'macchiato, or 'mocha
(load-theme 'catppuccin :no-confirm)

;; splash screen
;; use the `visual-fill-column' package
(straight-use-package '(nano-splash :type git :host github
                                   :repo "rougier/nano-splash"))
(require 'nano-splash)

;; Powerline
(use-package powerline)
(require 'powerline)
(powerline-default-theme)

;; transparency
(set-frame-parameter (selected-frame) 'alpha '(85 . 65))
(add-to-list 'default-frame-alist '(alpha . (85 . 65)))

;; ivy 
(require 'ivy-rich)
(ivy-rich-mode 1)
(setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
(setq ivy-rich-path-style 'abbrev)

;; general.el keybinds
(use-package general
  :config
  (general-evil-setup t)

  (general-create-definer rune/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"))

(rune/leader-keys
  "t"  '(:ignore t :which-key "toggles")
  "tt" '(counsel-load-theme :which-key "choose theme"))

;; evil mode
;; Download Evil
(setq evil-want-keybinding nil)
(unless (package-installed-p 'evil)
  (package-install 'evil))
;; Enable Evil
(require 'evil)
(evil-mode 1)
(defun rune/evil-hook ()
  (dolist (mode '(custom-mode
                  eshell-mode
                  git-rebase-mode
                  erc-mode
                  circe-server-mode
                  circe-chat-mode
                  circe-query-mode
                  sauron-mode
                  term-mode))
   (add-to-list 'evil-emacs-state-modes mode)))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :hook (evil-mode . rune/evil-hook)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

;; Mention evil-collection-mode-list
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))


(defun rune/dont-arrow-me-bro ()
  (interactive)
  (message "Arrow keys are bad, you know?"))

  ;; Disable arrow keys in normal and visual modes
  (define-key evil-normal-state-map (kbd "<left>") 'rune/dont-arrow-me-bro)
  (define-key evil-normal-state-map (kbd "<right>") 'rune/dont-arrow-me-bro)
  (define-key evil-normal-state-map (kbd "<down>") 'rune/dont-arrow-me-bro)
  (define-key evil-normal-state-map (kbd "<up>") 'rune/dont-arrow-me-bro)
  (evil-global-set-key 'motion (kbd "<left>") 'rune/dont-arrow-me-bro)
  (evil-global-set-key 'motion (kbd "<right>") 'rune/dont-arrow-me-bro)
  (evil-global-set-key 'motion (kbd "<down>") 'rune/dont-arrow-me-bro)
  (evil-global-set-key 'motion (kbd "<up>") 'rune/dont-arrow-me-bro)

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out"))

(rune/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("99d1e29934b9e712651d29735dd8dcd431a651dfbe039df158aa973461af003e" default))
 '(package-selected-packages
   '(nord-theme evil-collection general helpful ivy-rich command-log-mode which-key use-package rainbow-delimiters modus-themes)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
