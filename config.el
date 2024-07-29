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

(use-package vterm
    :ensure t)
(setq shell-file-name "/bin/fish"
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
  :config 
  (setq which-key-side-window-location 'bottom
      which-key-sort-order #'which-key-key-order-alpha
      which-key-sort-uppercase-first nil
      which-key-add-column-padding 1
      which-key-max-display-columns nil
      which-key-min-display-lines 6
      which-key-side-window-slot -10
      which-key-side-window-max-height 0.25
      which-key-idle-delay 0.8
      which-key-max-description-length 25
      which-key-allow-imprecise-window-fit t
      which-key-separator " → " ))

(use-package counsel
  :after ivy
  :config (counsel-mode))
(use-package ivy
  :defer 0.1
  :diminish
  :bind
  (("C-c C-r" . ivy-resume)
   ("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  :config
  (ivy-mode))
(use-package ivy-rich
  :after ivy
  :custom
  (ivy-virtual-abbreviate 'full
   ivy-rich-switch-buffer-align-virtual-buffer t
   ivy-rich-path-style 'abbrev)
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer)
  (ivy-rich-mode 1)) ;; this gets us descriptions in M-x.
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

(use-package projectile
    :config
(projectile-mode 1)
)

(use-package general
        :config
        (general-evil-setup t)

(nvmap :states '(normal visual) :keymaps 'override :prefix "SPC"
        ;; buffers
         "b b"   '(ibuffer :which-key "ibuffer")
         "b c"   '(clone-indirect-buffer-other-window :which-key "clone indirect buffer other window")
         "b d"   '(kill-current-buffer :which-key "kill current buffer")
         "b n"   '(next-buffer :which-key "next buffer")
         "b p"   '(previous-buffer :which-key "previous buffer")
         "b B"   '(ibuffer-list-buffers :which-key "ibuffer list buffers")
         "b D"   '(kill-buffer :which-key "kill buffer")
        ;; search 
          "/" '(swiper :wk "swiper search")
        ;; comment 
          "TAB TAB" '(comment-line :wk "comment lines")
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
        ;; "oT" '(vterm :wk "vterm")
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

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'dracula t)

(setq visible-bell nil)
(menu-bar-mode -1) 
(tool-bar-mode -1)
(scroll-bar-mode -1)

(set-frame-parameter (selected-frame) 'alpha '(85 . 85))
(add-to-list 'default-frame-alist '(alpha . (85 . 85)))

(column-number-mode)
(setq display-line-numbers-type 'relative) 
(global-display-line-numbers-mode)

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

(use-package dashboard
  :ensure t 
  :init
  (setq initial-buffer-choice 'dashboard-open)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-center-content t) ;; set to 't' for centered content
  (setq dashboard-banner-logo-title "神は神の天国にいって、世界はすべて整っているよ")
  ;;(setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
  (setq dashboard-startup-banner "/home/edwardsun/.emacs.d/nerv.png")  ;; use custom image as banner
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

(use-package toc-org
    :commands toc-org-enable
    :init (add-hook `org-mode-hook `toc-org-enable)
)

(add-hook `org-mode-hook `org-indent-mode)
(use-package org-bullets)
(add-hook `org-mode-hook (lambda () (org-bullets-mode 1)))

(electric-indent-mode -1)

(require `org-tempo)
