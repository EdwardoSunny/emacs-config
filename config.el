(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package evil
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  ;; let C-u be scroll
  (setq evil-want-C-u-scroll t)
  (evil-mode)
  :config
  ;; Allow undo and redo like vim
  (use-package undo-fu)
  (setq evil-undo-system `undo-fu) 

  ;; Custom function to set keybindings for vterm and other buffers
  (defun my-setup-evil-keybindings ()
    (if (derived-mode-p 'vterm-mode)
        (progn
          ;; In vterm, use vterm-yank for p and P
          (define-key evil-normal-state-map "p" 'vterm-yank)
          (define-key evil-normal-state-map "P" 'vterm-yank))
      (progn
        ;; In other buffers, use the default paste behavior
        (define-key evil-normal-state-map "p" 'evil-paste-after)
        (define-key evil-normal-state-map "P" 'evil-paste-before))))

  ;; Add hook to run our custom setup function when switching buffers
  (add-hook 'post-command-hook 'my-setup-evil-keybindings) 
)

(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-mode-list '(dashboard dired ibuffer))
  (evil-collection-init))

(use-package evil-tutor)

;; fix SPC RET and TAB in evil mode so can interact with regular emacs
(with-eval-after-load `evil-maps
    (define-key evil-motion-state-map (kbd "SPC") nil)
    (define-key evil-motion-state-map (kbd "TAB") nil)
    (define-key evil-motion-state-map (kbd "RET") nil)
)

;; allow redo, emacs 28+ only
(evil-set-undo-system 'undo-redo)

(setq org-return-follows-link t)

(defun efs/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; Bind some useful keys for evil-mode
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  (evil-normalize-keymaps)

  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell-git-prompt
  :after eshell)

(use-package eshell
  :hook (eshell-first-time-mode . efs/configure-eshell)
  :config

  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim")))

  (eshell-git-prompt-use-theme 'powerline))

(setopt eshell-prompt-function `fancy-shell)
(setopt eshell-prompt-regexp "^[^#$\n]* [$#] ")
(setopt eshell-highlight-prompt nil)

(setq company-global-modes `(not eshell-mode))

(use-package eshell-syntax-highlighting
  :after esh-mode
  :config
  (eshell-syntax-highlighting-global-mode +1))

(use-package eshell-toggle
:custom
(eshell-toggle-size-fraction 3)
(eshell-toggle-use-projectile-root t)
(eshell-toggle-run-command nil)
;; (eshell-toggle-init-function 
;;  #'eshell-toggle-init-ansi-term)
)

(defun eshell-new (name)
"Create new eshell buffer named NAME."
(interactive "sName: ")
(setq name (concat "$" name))
(eshell)
(rename-buffer name)
)

(use-package which-key
  :init
    (which-key-mode 1)
  :diminish
  :config
  (setq which-key-side-window-location 'bottom
	  which-key-sort-order #'which-key-key-order-alpha
	  which-key-allow-imprecise-window-fit nil
	  which-key-sort-uppercase-first nil
	  which-key-add-column-padding 1
	  which-key-max-display-columns nil
	  which-key-min-display-lines 6
	  which-key-side-window-slot -10
	  which-key-side-window-max-height 0.25
	  which-key-idle-delay 0.8
	  which-key-max-description-length 25
	  which-key-allow-imprecise-window-fit nil
	  which-key-separator " → " ))

(use-package counsel
  :after ivy
  :diminish
  :config (counsel-mode))

(use-package ivy
  :bind
  ;; ivy-resume resumes the last Ivy-based completion.
  (("C-c C-r" . ivy-resume)
   ("C-x B" . ivy-switch-buffer-other-window))
  :diminish
  :custom
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)
  :config
  (ivy-mode))

(use-package all-the-icons-ivy-rich
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :after ivy
  :init (ivy-rich-mode 1) ;; this gets us descriptions in M-x.
  :custom
  (ivy-virtual-abbreviate 'full
   ivy-rich-switch-buffer-align-virtual-buffer t
   ivy-rich-path-style 'abbrev)
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer))
(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

;; Copyright (C) 2004-2014  Lucas Bonnet <lucas@rincevent.net>
;; Copyright (C) 2014  Mathis Hofer <mathis@fsfe.org>
;; Copyright (C) 2014-2015  Geyslan G. Bem <geyslan@gmail.com>

;; Authors: Lucas Bonnet <lucas@rincevent.net>
;;          Mathis Hofer <mathis@fsfe.org>
;;          Geyslan G. Bem <geyslan@gmail.com>
;; URL: https://github.com/lukhas/buffer-move/
;; Version: 0.6.3
;; Package-Requires: ((emacs "24.1"))
;; Keywords: convenience

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This file is for lazy people wanting to swap buffers without
;; typing C-x b on each window. This is useful when you have :
;;
;; +--------------+-------------+
;; |              |             |
;; |    #emacs    |    #gnus    |
;; |              |             |
;; +--------------+-------------+
;; |                            |
;; |           .emacs           |
;; |                            |
;; +----------------------------+
;;
;; and you want to have :
;;
;; +--------------+-------------+
;; |              |             |
;; |    #gnus     |   .emacs    |
;; |              |             |
;; +--------------+-------------+
;; |                            |
;; |           #emacs           |
;; |                            |
;; +----------------------------+
;;
;; With buffer-move, just go in #gnus, do buf-move-left, go to #emacs
;; (which now should be on top right) and do buf-move-down.
;;
;; To use it, simply put a (require 'buffer-move) in your ~/.emacs and
;; define some keybindings. For example, i use :
;;
;; (global-set-key (kbd "<C-S-up>")     'buf-move-up)
;; (global-set-key (kbd "<C-S-down>")   'buf-move-down)
;; (global-set-key (kbd "<C-S-left>")   'buf-move-left)
;; (global-set-key (kbd "<C-S-right>")  'buf-move-right)
;;
;; Alternatively, you may let the current window switch back to the previous
;; buffer, instead of swapping the buffers of both windows. Set the
;; following customization variable to 'move to activate this behavior:
;;
;; (setq buffer-move-behavior 'move)

;;; Code:

(require 'windmove)

(defconst buffer-move-version "0.6.3"
  "Version of buffer-move.el")

(defgroup buffer-move nil
  "Swap buffers without typing C-x b on each window"
  :group 'tools)

(defcustom buffer-move-behavior 'swap
  "If set to 'swap (default), the buffers will be exchanged
  (i.e. swapped), if set to 'move, the current window is switch back to the
  previously displayed buffer (i.e. the buffer is moved)."
  :group 'buffer-move
  :type 'symbol)

(defcustom buffer-move-stay-after-swap nil
  "If set to non-nil, point will stay in the current window
  so it will not be moved when swapping buffers. This setting
  only has effect if `buffer-move-behavior' is set to 'swap."
  :group 'buffer-move
  :type 'boolean)

(defun buf-move-to (direction)
  "Helper function to move the current buffer to the window in the given
   direction (with must be 'up, 'down', 'left or 'right). An error is
   thrown, if no window exists in this direction."
  (cl-flet ((window-settings (window)
              (list (window-buffer window)
                    (window-start window)
                    (window-hscroll window)
                    (window-point window)))
            (set-window-settings (window settings)
              (cl-destructuring-bind (buffer start hscroll point)
                  settings
                (set-window-buffer window buffer)
                (set-window-start window start)
                (set-window-hscroll window hscroll)
                (set-window-point window point))))
    (let* ((this-window (selected-window))
           (this-window-settings (window-settings this-window))
           (other-window (windmove-find-other-window direction))
           (other-window-settings (window-settings other-window)))
      (cond ((null other-window)
             (error "No window in this direction"))
            ((window-dedicated-p other-window)
             (error "The window in this direction is dedicated"))
            ((window-minibuffer-p other-window)
             (error "The window in this direction is the Minibuffer")))
      (set-window-settings other-window this-window-settings)
      (if (eq buffer-move-behavior 'move)
          (switch-to-prev-buffer this-window)
        (set-window-settings this-window other-window-settings))
      (select-window other-window))))

;;;###autoload
(defun buf-move-up ()
  "Swap the current buffer and the buffer above the split.
   If there is no split, ie now window above the current one, an
   error is signaled."
  (interactive)
  (buf-move-to 'up))

;;;###autoload
(defun buf-move-down ()
  "Swap the current buffer and the buffer under the split.
   If there is no split, ie now window under the current one, an
   error is signaled."
  (interactive)
  (buf-move-to 'down))

;;;###autoload
(defun buf-move-left ()
  "Swap the current buffer and the buffer on the left of the split.
   If there is no split, ie now window on the left of the current
   one, an error is signaled."
  (interactive)
  (buf-move-to 'left))

;;;###autoload
(defun buf-move-right ()
  "Swap the current buffer and the buffer on the right of the split.
   If there is no split, ie now window on the right of the current
   one, an error is signaled."
  (interactive)
  (buf-move-to 'right))

;;;###autoload
(defun buf-move ()
  "Begin moving the current buffer to different windows.

Use the arrow keys to move in the desired direction.  Pressing
any other key exits this function."
  (interactive)
  (let ((map (make-sparse-keymap)))
    (dolist (x '(("<up>" . buf-move-up)
                 ("<left>" . buf-move-left)
                 ("<down>" . buf-move-down)
                 ("<right>" . buf-move-right)))
      (define-key map (read-kbd-macro (car x)) (cdr x)))
    (set-transient-map map t)))

;; (provide 'buffer-move)

(require 'dired)
(setq dired-listing-switches "-alh")
(add-hook 'dired-mode-hook 'auto-revert-mode)

(use-package diminish)

(use-package perspective
  :custom
  ;; NOTE! I have also set 'SCP =' to open the perspective menu.
  ;; I'm only setting the additional binding because setting it
  ;; helps suppress an annoying warning message.
  (persp-mode-prefix-key (kbd "C-c M-p"))
  :init 
  (persp-mode)
  :config
  ;; Sets a file to write to when we save states
  (setq persp-state-default-file "~/.emacs.d/sessions"))

;; This will group buffers by persp-name in ibuffer.
(add-hook 'ibuffer-hook
          (lambda ()
            (persp-ibuffer-set-filter-groups)
            (unless (eq ibuffer-sorting-mode 'alphabetic)
              (ibuffer-do-sort-by-alphabetic))))

;; Automatically save perspective states to file when Emacs exits.
(add-hook 'kill-emacs-hook #'persp-state-save)

(setq backup-directory-alist '((".*" . "~/.local/share/Trash/files")))

(add-to-list 'exec-path "~/.local/bin")

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package evil-magit
  :after magit)

(use-package lsp-mode 
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (python-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
)

(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

(use-package elpy
  :straight t
  :init
  (elpy-enable))

(setq lsp-pylsp-server-command "pylsp")
(setq lsp-ruff-lsp-server-command "ruff-lsp")
(add-hook 'python-mode-hook #'lsp-deferred)

(elpy-enable)

   ;; A python shell for every buffer
(add-hook 'elpy-mode-hook (lambda () (elpy-shell-toggle-dedicated-shell 1)))

   ;;(add-hook 'python-mode-hook #'python-cello-mode 1)
   (setq python-shell-interpreter "ipython3"
            python-shell-interpreter-args "--simple-prompt -i --pylab=qt5")

   ;; Real time syntax check in python
   (when (require 'flycheck nil t)
         (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
         (add-hook 'elpy-mode-hook 'flycheck-mode))

(use-package python-mode
  :straight nil
  :hook (python-mode . lsp-deferred) ;; when open python file, turn on LSP mode
)

;; (setq python-shell-interpreter "python3") ;; ensure use python3 as interpreter

(use-package company
    :after lsp-mode
    :hook (prog-mode . company-mode)
    :bind (:map company-active-map
           ("<tab>" . company-complete-selection))
          (:map lsp-mode-map
           ("<tab>" . company-indent-or-complete-common))
    :custom
    (company-minimum-prefix-length 1)
    (company-idle-delay 0.0))

  (use-package company-jedi)

(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'my/python-mode-hook)

(defun my-eshell-init ()
  (company-mode -1))

(add-hook 'eshell-mode-hook #'my-eshell-init) ;; disable company in eshell mode

(use-package flycheck
  :defer t
  :diminish
  :init (global-flycheck-mode))

(use-package projectile
  :straight t
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Documents/Code")
    (setq projectile-project-search-path '("~/Documents/Code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(setq projectile-mode-line "Projectile") ;; disable modeline projectile, otherwise remote connections will have massive latency

(use-package ein)

(setq tramp-default-method "ssh")

(use-package auctex)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

;; compile into PDF
(setq TeX-PDF-mode t)

(use-package company-math)
(defun my-latex-mode-setup ()
  (setq-local company-backends
              (append '((company-math-symbols-latex company-math-symbols-unicode))
                      company-backends)))

(add-hook 'LaTeX-mode-hook 'my-latex-mode-setup)
(add-hook 'after-init-hook 'global-company-mode)

(use-package general
        :config
        (general-evil-setup t)

(nvmap :states '(normal visual) :keymaps 'override :prefix "SPC"
        ;; buffers
        ","   '(ibuffer :which-key "ibuffer")
        "b c"   '(clone-indirect-buffer-other-window :which-key "clone indirect buffer other window")
        "b d"   '(kill-current-buffer :which-key "kill current buffer")
        "b n"   '(next-buffer :which-key "next buffer")
        "b p"   '(previous-buffer :which-key "previous buffer")
        "b B"   '(ibuffer-list-buffers :which-key "ibuffer list buffers")
        "b D"   '(kill-buffer :which-key "kill buffer")
        ;; search 
        "/" '(swiper :wk "swiper search")
        ;; comment 
        "c c" '(comment-line :wk "comment lines")
        ;; help 
        "h" '(:ignore t :wk "help")
        "hf" '(describe-function :wk "describe function") ;; if working in elisp ONLY file
        "hv" '(describe-variable :wk "describe variable")
        "h r r" '(reload-init-file :wk "reload emacs config")
        ;; themes 
        "t"  '(:ignore t :wk "toggles")
        "tt" '(counsel-load-theme :wk "choose theme") ;; change theme easily
        ;; file navigation 
       "."     '(find-file :which-key "find file")
       "ff"   '(find-file :which-key "find file")
       "fr"   '(counsel-recentf :which-key "recent files")
       "fs"   '(save-buffer :which-key "save file")
       "fu"   '(sudo-edit-find-file :which-key "sudo find file")
       "fy"   '(dt/show-and-copy-buffer-path :which-key "yank file path")
       "fC"   '(copy-file :which-key "copy file")
       "fD"   '(delete-file :which-key "delete file")
       "fR"   '(rename-file :which-key "rename file")
       "fS"   '(write-file :which-key "save file as...")
       "fU"   '(sudo-edit :which-key "sudo edit file")
        ;; windows 
        "wv" '(evil-window-vsplit :wk "split-window-right")
        "ws" '(evil-window-split  :wk "split-window-below")
        "wd" '(evil-window-delete :wk "delete-window")
        "wD" '(delete-other-windows :wk "delete-other-windows")
        ;; navigation 
        "wh" '(evil-window-left :wk "windmove-left") ;; vim like window movement
        "wj" '(evil-window-down :wk "windmove-down")
        "wk" '(evil-window-up :wk "windmove-up")
        "wl" '(evil-window-right :wk "windmove-right")
        "ww" '(evil-window-next :wk "windmove-next")
        ;; window move
        "wH" '(buf-move-left :wk "move window left") ;; vim like window movement
        "wJ" '(buf-move-down :wk "move window down")
        "wK" '(buf-move-up :wk "move window up")
        "wL" '(buf-move-right :wk "windmove-right")
        ;; terminal
        "ot" '(eshell-toggle :wk "toggle eshell")
        "oT" '(eshell-new :wk "open new eshell")
        ;; perspective.el workspaces
        "TAB" '(perspective-map :wk "Perspective") ;; Lists all the perspective keybindings
        ;; projectile
        "p" `(projectile-command-map :wk "Projectile command map")
        ;; AUCTex bindings
        ;; previewing 
        "lpp" '(preview-buffer :wk "preview current latex buffer") 
        "lpa" '(preview-at-point :wk "toggle latex preview at point") 
        "lpd" '(preview-document :wk "preview current latex document") 
        ;; compiling latex
        "lca" '(TeX-command-run-all :wk "compile current document") 
         ;; speedbar "file tree"
        "sb"  '(speedbar :wk "toggle speedbar file summary/tree") 
        )
  )

   (defun reload-init-file()
      (interactive)
      (load-file user-init-file)
      (load-file user-init-file)
  )

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

(global-set-key [escape] `keyboard-escape-quit)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'modus-vivendi t)

(setq visible-bell nil)
(menu-bar-mode -1) 
(tool-bar-mode -1)
(scroll-bar-mode -1)

(set-frame-parameter (selected-frame) 'alpha '(85 . 85))
(add-to-list 'default-frame-alist '(alpha . (85 . 85)))

(column-number-mode)
(setq display-line-numbers-type 'relative) 
(global-display-line-numbers-mode)

;; increase font size
  (set-face-attribute 'default nil :height 140)

  ;; (set-face-attribute 'default nil
  ;;   :font "Ubuntu"
  ;;   :height 120
  ;;   :weight 'medium)
  ;; (set-face-attribute 'variable-pitch nil
  ;;   :font "Ubuntu"
  ;;   :height 130
  ;;   :weight 'medium)
  ;; (set-face-attribute 'fixed-pitch nil
  ;;   :font "Ubuntu"
  ;;   :height 120
  ;;   :weight 'medium)
  ;; ;; Makes commented text and keywords italics.
  ;; ;; This is working in emacsclient but not emacs.
  ;; ;; Your font must have an italic face available.
  ;; (set-face-attribute 'font-lock-comment-face nil
  ;;   :slant 'italic)
  ;; (set-face-attribute 'font-lock-keyword-face nil
  ;;   :slant 'italic)

  ;; ;; Uncomment the following line if line spacing needs adjusting.
  ;; (setq-default line-spacing 0.12)

  ;; Needed if using emacsclient. Otherwise, your fonts will be smaller than expected.
  ;; (add-to-list 'default-frame-alist '(font . "Ubuntu"))
;; changes certain keywords to symbols, such as lamda!
 (setq global-prettify-symbols-mode t)

(use-package all-the-icons
    :if (display-graphic-p)
)

(use-package all-the-icons-dired
    :hook (dired-mode . (lambda () (all-the-icons-dired-mode t)))
)
;; run M-x all-the-icons-install-fonts if fonts not showing up

(use-package nerd-icons
  ;; :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
)
;; run M-x nerd-icons-install-fonts if fonts not showing up

(use-package rainbow-delimiters
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
         (clojure-mode . rainbow-delimiters-mode)))
(rainbow-delimiters-mode)

(use-package rainbow-mode
  :diminish
  :hook org-mode prog-mode)

(use-package dashboard
  :init
  (setq initial-buffer-choice 'dashboard-open)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-center-content t) ;; set to 't' for centered content
  (setq dashboard-banner-logo-title "神は神の天国にいって、世界はすべて整っているよ")
  ;;(setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
  (setq dashboard-startup-banner "~/.emacs.d/img/nerv.png")  ;; use custom image as banner
  (setq dashboard-items '((recents . 5)
                          ;; (agenda . 5 )
                          (bookmarks . 3)
                          ;; (projects . 3)
                          (registers . 3)))
  :custom
  (dashboard-modify-heading-icons '((recents . "file-text")
                                    (bookmarks . "book")))
  :config
  (dashboard-setup-startup-hook))

(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package nyan-mode
  :config
  ;; Enable animation
  (setq nyan-animate-nyancat t)
  ;; Set animation frame interval to 0.1 seconds (you can adjust as needed)
  (setq nyan-animation-frame-interval 0.1)
  ;; Set the length of the Nyan bar
  (setq nyan-bar-length 30) ;; Adjust as needed
  ;; Choose a cat face for the console mode (e.g., 0 for the default)
  (setq nyan-cat-face-number 0) ;; Adjust the face number as needed
  ;; Enable wavy trail
  (setq nyan-wavy-trail t)
  ;; Set minimum window width to disable Nyan Mode
  (setq nyan-minimum-window-width 80) ;; Adjust as needed
  ;; Start Nyan Mode
  (nyan-mode 1)
)

(delete-selection-mode 1)    ;; You can select text and delete it by typing.
(electric-indent-mode -1)    ;; Turn off the weird indenting that Emacs does by default.
(electric-pair-mode 1)       ;; Turns on automatic parens pairing
;; The following prevents <> from auto-pairing when electric-pair-mode is on.
;; Otherwise, org-tempo is broken when you try to <s TAB...
(add-hook 'org-mode-hook (lambda ()
           (setq-local electric-pair-inhibit-predicate
                   `(lambda (c)
                  (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))
(global-auto-revert-mode t)  ;; Automatically show changes if the file has changed

(add-hook 'minibuffer-inactive-mode-hook (lambda () (auto-revert-mode -1)))

(setq auto-revert-remote-files nil)

(global-display-line-numbers-mode 1) ;; Display line numbers
(global-visual-line-mode t)  ;; Enable truncated lines
(menu-bar-mode -1)           ;; Disable the menu bar 
(scroll-bar-mode -1)         ;; Disable the scroll bar
(tool-bar-mode -1)           ;; Disable the tool bar
(setq org-edit-src-content-indentation 0) ;; Set src block automatic indent to 0 instead of 2.

(use-package toc-org
    :commands toc-org-enable
    :init (add-hook `org-mode-hook `toc-org-enable)
)

(add-hook `org-mode-hook `org-indent-mode)
(use-package org-bullets)
(add-hook `org-mode-hook (lambda () (org-bullets-mode 1)))

(use-package hl-todo
  :hook ((org-mode . hl-todo-mode)
         (prog-mode . hl-todo-mode))
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(("TODO"       warning bold)
          ("FIXME"      error bold)
          ("HACK"       font-lock-constant-face bold)
          ("REVIEW"     font-lock-keyword-face bold)
          ("NOTE"       success bold)
          ("DEPRECATED" font-lock-doc-face bold))))

(require `org-tempo)
