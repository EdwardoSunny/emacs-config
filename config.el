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
  (evil-collection-init)) ;; init with either config or do this for all packages (vterm, calendar, etc.)
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

(use-package general
      :config
      (general-evil-setup t)

      ;; set up SPC as global leader key

  (general-create-definer edward/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

    (edward/leader-keys
    ;; themes
        "t"  '(:ignore t :wk "toggles")
        "tt" '(counsel-load-theme :wk "choose theme") ;; change theme easily
        ;; files 
        "." '(find-file :wk "dired")
    ;; comment
    "TAB TAB" '(comment-line :wk "comment lines")
        ;; search
        "/" '(swiper :wk "swiper search")
        ;; windows
        "wv" '(split-window-right :wk "split-window-right")
        "ws" '(split-window-below :wk "split-window-below")
        "wd" '(delete-window :wk "delete-window")
        "wD" '(delete-other-windows :wk "delete-other-windows")
        "wh" '(windmove-left :wk "windmove-left") ;; vim like window movement
        "wj" '(windmove-down :wk "windmove-down")
        "wk" '(windmove-up :wk "windmove-up")
        "wl" '(windmove-right :wk "windmove-right")
        ;; buffers
        "," '(list-buffers :wk "list-buffers")
        "b" `(:ignore t :wk "buffer")
        "bb" `(switch-to-buffer :wk "switch buffer")
        "bi" `(ibuffer :wk "interactive list buffer")
        "bk" `(kill-this-buffer :wk "kill this buffer")
        "bn" `(next-buffer :wk "next buffer")
        "bp" `(previous-buffer :wk "previous buffer")
        "br" `(revert-buffer :wk "reload buffer")
        ;; terminal  
        "ot" '(vterm-other-window :wk "vterm-other-window")
        "oT" '(vterm :wk "vterm")
    ;; evalute
    "e" '(:ignore t :wk "evaluate")
        "eb" '(eval-buffer :wk "evaluate elisp in buffer") ;; if working in elisp ONLY file
        "ed" '(eval-defun :wk "evaluate defun containing or after point")
        "ee" '(eval-expression :wk "evaluate and elisp expression")
        "el" '(eval-last-sexp :wk "evaluate elisp expression before point")
        "er" '(eval-region :wk "evaluate elisp in region")
    ;; evalute
    "h" '(:ignore t :wk "help")
        "hf" '(describe-function :wk "describe function") ;; if working in elisp ONLY file
        "hv" '(describe-variable :wk "describe variable")
        "h r r" '(reload-init-file :wk "reload emacs config")
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

(use-package toc-org
    :commands toc-org-enable
    :init (add-hook `org-mode-hook `toc-org-enable)
)

(add-hook `org-mode-hook `org-indent-mode)
(use-package org-bullets)
(add-hook `org-mode-hook (lambda () (org-bullets-mode 1)))

(electric-indent-mode -1)

(require `org-tempo)
