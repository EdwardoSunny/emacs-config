(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
  			    :ref nil :depth 1
  			    :files (:defaults "elpaca-test.el" (:exclude "extensions"))
  			    :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
      (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
  	       ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
  					       ,@(when-let ((depth (plist-get order :depth)))
  						   (list (format "--depth=%d" depth) "--no-single-branch"))
  					       ,(plist-get order :repo) ,repo))))
  	       ((zerop (call-process "git" nil buffer t "checkout"
  				     (or (plist-get order :ref) "--"))))
  	       (emacs (concat invocation-directory invocation-name))
  	       ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
  				     "--eval" "(byte-recompile-directory \".\" 0 'force)")))
  	       ((require 'elpaca))
  	       ((elpaca-generate-autoloads "elpaca" repo)))
  	  (progn (message "%s" (buffer-string)) (kill-buffer buffer))
  	(error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))


;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode)
  (setq elpaca-use-package-by-default t))

(elpaca-wait)

(use-package exec-path-from-shell
:ensure t
(exec-path-from-shell-copy-env "PYTHONPATH")
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
)

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
(setq org-return-follows-link t)

;; (use-package vterm
;;        :ensure t)
;; ;; 
(use-package vterm
      :custom (vterm-install t))
  (setq shell-file-name "/bin/bash"
        vterm-max-scrollback 5000)

(use-package vterm-toggle
  :after vterm
  :config
  (setq vterm-toggle-fullscreen-p nil)
  ;; (setq vterm-toggle-scope 'project)
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                     (let ((buffer (get-buffer buffer-or-name)))
                       (with-current-buffer buffer
                         (or (equal major-mode 'vterm-mode)
                             (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                  (display-buffer-reuse-window display-buffer-at-bottom)
                  ;;(display-buffer-reuse-window display-buffer-in-direction)
                  ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                  ;;(direction . bottom)
                  ;;(dedicated . t) ;dedicated is supported in emacs27
                  (reusable-frames . visible)
                  (window-height . 0.3))))

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
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :after ivy
  :ensure t
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

(use-package dired
:ensure t 
:config (add-hook 'dired-mode-hook 'auto-revert-mode)
)

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
  (setq persp-state-default-file "~/.config/emacs/sessions"))

;; This will group buffers by persp-name in ibuffer.
(add-hook 'ibuffer-hook
          (lambda ()
            (persp-ibuffer-set-filter-groups)
            (unless (eq ibuffer-sorting-mode 'alphabetic)
              (ibuffer-do-sort-by-alphabetic))))

;; Automatically save perspective states to file when Emacs exits.
(add-hook 'kill-emacs-hook #'persp-state-save)

(setq backup-directory-alist '((".*" . "~/.local/share/Trash/files")))

(use-package magit)

(use-package lsp-mode 
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (python-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :ensure 
)

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

(use-package flycheck
  :ensure t
  :defer t
  :diminish
  :init (global-flycheck-mode))

(use-package projectile
    :config
(projectile-mode 1)
)

(use-package ein
:ensure t
)

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
        ;; terminals 
        "ot" '(vterm-toggle :wk "toggle vterm")
        ;; perspective.el workspaces
        "TAB" '(perspective-map :wk "Perspective") ;; Lists all the perspective keybindings
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
(load-theme 'timu-caribbean t)

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
    :ensure t
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

(use-package rainbow-mode
  :diminish
  :hook org-mode prog-mode)

(use-package dashboard
  :ensure t 
  :init
  (setq initial-buffer-choice 'dashboard-open)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-center-content t) ;; set to 't' for centered content
  (setq dashboard-banner-logo-title "神は神の天国にいって、世界はすべて整っているよ")
  ;;(setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
  (setq dashboard-startup-banner "/home/edward/.emacs.d/img/nerv.png")  ;; use custom image as banner
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

(use-package doom-modeline
    :ensure t
    :init (doom-modeline-mode 1))

(use-package nyan-mode
  :ensure t
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


  ;; If non-nil, cause imenu to see `doom-modeline' declarations.
  ;; This is done by adjusting `lisp-imenu-generic-expression' to
  ;; include support for finding `doom-modeline-def-*' forms.
  ;; Must be set before loading doom-modeline.
  (setq doom-modeline-support-imenu t)

  ;; How tall the mode-line should be. It's only respected in GUI.
  ;; If the actual char height is larger, it respects the actual height.
  (setq doom-modeline-height 25)

  ;; How wide the mode-line bar should be. It's only respected in GUI.
  (setq doom-modeline-bar-width 4)

  ;; Whether to use hud instead of default bar. It's only respected in GUI.
  (setq doom-modeline-hud nil)

  ;; The limit of the window width.
  ;; If `window-width' is smaller than the limit, some information won't be
  ;; displayed. It can be an integer or a float number. `nil' means no limit."
  (setq doom-modeline-window-width-limit 85)

  ;; How to detect the project root.
  ;; nil means to use `default-directory'.
  ;; The project management packages have some issues on detecting project root.
  ;; e.g. `projectile' doesn't handle symlink folders well, while `project' is unable
  ;; to hanle sub-projects.
  ;; You can specify one if you encounter the issue.
  (setq doom-modeline-project-detection 'auto)

  ;; Determines the style used by `doom-modeline-buffer-file-name'.
  ;;
  ;; Given ~/Projects/FOSS/emacs/lisp/comint.el
  ;;   auto => emacs/l/comint.el (in a project) or comint.el
  ;;   truncate-upto-project => ~/P/F/emacs/lisp/comint.el
  ;;   truncate-from-project => ~/Projects/FOSS/emacs/l/comint.el
  ;;   truncate-with-project => emacs/l/comint.el
  ;;   truncate-except-project => ~/P/F/emacs/l/comint.el
  ;;   truncate-upto-root => ~/P/F/e/lisp/comint.el
  ;;   truncate-all => ~/P/F/e/l/comint.el
  ;;   truncate-nil => ~/Projects/FOSS/emacs/lisp/comint.el
  ;;   relative-from-project => emacs/lisp/comint.el
  ;;   relative-to-project => lisp/comint.el
  ;;   file-name => comint.el
  ;;   file-name-with-project => FOSS|comint.el
  ;;   buffer-name => comint.el<2> (uniquify buffer name)
  ;;
  ;; If you are experiencing the laggy issue, especially while editing remote files
  ;; with tramp, please try `file-name' style.
  ;; Please refer to https://github.com/bbatsov/projectile/issues/657.
  (setq doom-modeline-buffer-file-name-style 'auto)

  ;; Whether display icons in the mode-line.
  ;; While using the server mode in GUI, should set the value explicitly.
  (setq doom-modeline-icon t)

  ;; Whether display the icon for `major-mode'. It respects option `doom-modeline-icon'.
  (setq doom-modeline-major-mode-icon t)

  ;; Whether display the colorful icon for `major-mode'.
  ;; It respects `nerd-icons-color-icons'.
  (setq doom-modeline-major-mode-color-icon t)

  ;; Whether display the icon for the buffer state. It respects option `doom-modeline-icon'.
  (setq doom-modeline-buffer-state-icon t)

  ;; Whether display the modification icon for the buffer.
  ;; It respects option `doom-modeline-icon' and option `doom-modeline-buffer-state-icon'.
  (setq doom-modeline-buffer-modification-icon t)

  ;; Whether display the lsp icon. It respects option `doom-modeline-icon'.
  (setq doom-modeline-lsp-icon t)

  ;; Whether display the time icon. It respects option `doom-modeline-icon'.
  (setq doom-modeline-time-icon t)

  ;; Whether display the live icons of time.
  ;; It respects option `doom-modeline-icon' and option `doom-modeline-time-icon'.
  (setq doom-modeline-time-live-icon t)

  ;; Whether to use an analogue clock svg as the live time icon.
  ;; It respects options `doom-modeline-icon', `doom-modeline-time-icon', and `doom-modeline-time-live-icon'.
  (setq doom-modeline-time-analogue-clock t)

  ;; The scaling factor used when drawing the analogue clock.
  (setq doom-modeline-time-clock-size 0.7)

  ;; Whether to use unicode as a fallback (instead of ASCII) when not using icons.
  (setq doom-modeline-unicode-fallback nil)

  ;; Whether display the buffer name.
  (setq doom-modeline-buffer-name t)

  ;; Whether highlight the modified buffer name.
  (setq doom-modeline-highlight-modified-buffer-name t)

  ;; When non-nil, mode line displays column numbers zero-based.
  ;; See `column-number-indicator-zero-based'.
  (setq doom-modeline-column-zero-based t)

  ;; Specification of \"percentage offset\" of window through buffer.
  ;; See `mode-line-percent-position'.
  (setq doom-modeline-percent-position '(-3 "%p"))

  ;; Format used to display line numbers in the mode line.
  ;; See `mode-line-position-line-format'.
  (setq doom-modeline-position-line-format '("L%l"))

  ;; Format used to display column numbers in the mode line.
  ;; See `mode-line-position-column-format'.
  (setq doom-modeline-position-column-format '("C%c"))

  ;; Format used to display combined line/column numbers in the mode line. See `mode-line-position-column-line-format'.
  (setq doom-modeline-position-column-line-format '("%l:%c"))

  ;; Whether display the minor modes in the mode-line.
  (setq doom-modeline-minor-modes nil)

  ;; If non-nil, a word count will be added to the selection-info modeline segment.
  (setq doom-modeline-enable-word-count nil)

  ;; Major modes in which to display word count continuously.
  ;; Also applies to any derived modes. Respects `doom-modeline-enable-word-count'.
  ;; If it brings the sluggish issue, disable `doom-modeline-enable-word-count' or
  ;; remove the modes from `doom-modeline-continuous-word-count-modes'.
  (setq doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))

  ;; Whether display the buffer encoding.
  (setq doom-modeline-buffer-encoding t)

  ;; Whether display the indentation information.
  (setq doom-modeline-indent-info nil)

  ;; Whether display the total line number。
  (setq doom-modeline-total-line-number nil)

  ;; Whether display the icon of vcs segment. It respects option `doom-modeline-icon'."
  (setq doom-modeline-vcs-icon t)

  ;; The maximum displayed length of the branch name of version control.
  (setq doom-modeline-vcs-max-length 12)

  ;; The function to display the branch name.
  (setq doom-modeline-vcs-display-function #'doom-modeline-vcs-name)

  ;; Whether display the icon of check segment. It respects option `doom-modeline-icon'.
  (setq doom-modeline-check-icon t)

  ;; If non-nil, only display one number for check information if applicable.
  (setq doom-modeline-check-simple-format nil)

  ;; The maximum number displayed for notifications.
  (setq doom-modeline-number-limit 99)

  ;; Whether display the workspace name. Non-nil to display in the mode-line.
  (setq doom-modeline-workspace-name t)

  ;; Whether display the perspective name. Non-nil to display in the mode-line.
  (setq doom-modeline-persp-name t)

  ;; If non nil the default perspective name is displayed in the mode-line.
  (setq doom-modeline-display-default-persp-name nil)

  ;; If non nil the perspective name is displayed alongside a folder icon.
  (setq doom-modeline-persp-icon t)

  ;; Whether display the `lsp' state. Non-nil to display in the mode-line.
  (setq doom-modeline-lsp t)

  ;; Whether display the GitHub notifications. It requires `ghub' package.
  (setq doom-modeline-github nil)

  ;; The interval of checking GitHub.
  (setq doom-modeline-github-interval (* 30 60))

  ;; Whether display the modal state.
  ;; Including `evil', `overwrite', `god', `ryo' and `xah-fly-keys', etc.
  (setq doom-modeline-modal t)

  ;; Whether display the modal state icon.
  ;; Including `evil', `overwrite', `god', `ryo' and `xah-fly-keys', etc.
  (setq doom-modeline-modal-icon t)

  ;; Whether display the modern icons for modals.
  (setq doom-modeline-modal-modern-icon t)

  ;; When non-nil, always show the register name when recording an evil macro.
  (setq doom-modeline-always-show-macro-register nil)

  ;; Whether display the mu4e notifications. It requires `mu4e-alert' package.
  (setq doom-modeline-mu4e nil)

  ;; Whether display the gnus notifications.
  (setq doom-modeline-gnus t)

  ;; Whether gnus should automatically be updated and how often (set to 0 or smaller than 0 to disable)
  (setq doom-modeline-gnus-timer 2)

  ;; Wheter groups should be excludede when gnus automatically being updated.
  (setq doom-modeline-gnus-excluded-groups '("dummy.group"))

  ;; Whether display the IRC notifications. It requires `circe' or `erc' package.
  (setq doom-modeline-irc t)

  ;; Function to stylize the irc buffer names.
  (setq doom-modeline-irc-stylize 'identity)

  ;; Whether display the battery status. It respects `display-battery-mode'.
  (setq doom-modeline-battery t)

  ;; Whether display the time. It respects `display-time-mode'.
  (setq doom-modeline-time t)

  ;; Whether display the misc segment on all mode lines.
  ;; If nil, display only if the mode line is active.
  (setq doom-modeline-display-misc-in-all-mode-lines t)

  ;; The function to handle `buffer-file-name'.
  (setq doom-modeline-buffer-file-name-function #'identity)

  ;; The function to handle `buffer-file-truename'.
  (setq doom-modeline-buffer-file-truename-function #'identity)

  ;; Whether display the environment version.
  (setq doom-modeline-env-version t)
  ;; Or for individual languages
  (setq doom-modeline-env-enable-python t)
  (setq doom-modeline-env-enable-ruby t)
  (setq doom-modeline-env-enable-perl t)
  (setq doom-modeline-env-enable-go t)
  (setq doom-modeline-env-enable-elixir t)
  (setq doom-modeline-env-enable-rust t)

  ;; Change the executables to use for the language version string
  (setq doom-modeline-env-python-executable "python") ; or `python-shell-interpreter'
  (setq doom-modeline-env-ruby-executable "ruby")
  (setq doom-modeline-env-perl-executable "perl")
  (setq doom-modeline-env-go-executable "go")
  (setq doom-modeline-env-elixir-executable "iex")
  (setq doom-modeline-env-rust-executable "rustc")

  ;; What to display as the version while a new one is being loaded
  (setq doom-modeline-env-load-string "...")

  ;; By default, almost all segments are displayed only in the active window. To
  ;; display such segments in all windows, specify e.g.
  (setq doom-modeline-always-visible-segments '(mu4e irc))

  ;; Hooks that run before/after the modeline version string is updated
  (setq doom-modeline-before-update-env-hook nil)
  (setq doom-modeline-after-update-env-hook nil)

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
