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
  :init      ;; tweak evil's configuration before loading it
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (evil-mode))
(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-mode-list '(dashboard dired ibuffer))
  (evil-collection-init))
(use-package evil-tutor)

  ;; let C-u be scorll
  (setq evil-want-C-u-scroll t)

  ;; allow undo and redo like vim
  (use-package undo-fu)
  (setq evil-undo-system `undo-fu)

(use-package vterm
    :ensure t)
(setq shell-file-name "/bin/fish"
      vterm-max-scrollback 5000)

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

(use-package general
        :config
        (general-evil-setup t)

(nvmap :states '(normal visual) :keymaps 'override :prefix "SPC"
         "b b"   '(ibuffer :which-key "ibuffer")
         "b c"   '(clone-indirect-buffer-other-window :which-key "clone indirect buffer other window")
         "b d"   '(kill-current-buffer :which-key "kill current buffer")
         "b n"   '(next-buffer :which-key "next buffer")
         "b p"   '(previous-buffer :which-key "previous buffer")
         "b B"   '(ibuffer-list-buffers :which-key "ibuffer list buffers")
         "b D"   '(kill-buffer :which-key "kill buffer")
          "/" '(swiper :wk "swiper search")
          "TAB TAB" '(comment-line :wk "comment lines")
          "h" '(:ignore t :wk "help")
          "hf" '(describe-function :wk "describe function") ;; if working in elisp ONLY file
          "hv" '(describe-variable :wk "describe variable")
          "h r r" '(reload-init-file :wk "reload emacs config")
          "t"  '(:ignore t :wk "toggles")
          "tt" '(counsel-load-theme :wk "choose theme") ;; change theme easily
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
        "wv" '(split-window-right :wk "split-window-right")
        "ws" '(split-window-below :wk "split-window-below")
        "wd" '(delete-window :wk "delete-window")
        "wD" '(delete-other-windows :wk "delete-other-windows")
        "wh" '(windmove-left :wk "windmove-left") ;; vim like window movement
        "wj" '(windmove-down :wk "windmove-down")
        "wk" '(windmove-up :wk "windmove-up")
        "wl" '(windmove-right :wk "windmove-right")
        "ot" '(vterm-other-window :wk "vterm-other-window")
        "oT" '(vterm :wk "vterm")
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
(load-theme 'masked t)

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

(use-package toc-org
    :commands toc-org-enable
    :init (add-hook `org-mode-hook `toc-org-enable)
)

(add-hook `org-mode-hook `org-indent-mode)
(use-package org-bullets)
(add-hook `org-mode-hook (lambda () (org-bullets-mode 1)))

(electric-indent-mode -1)

(require `org-tempo)
