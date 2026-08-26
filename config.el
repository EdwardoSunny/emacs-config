(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package exec-path-from-shell
  :straight t
  :init
  (setq exec-path-from-shell-arguments '("-i" "-l"))
  ;; don't nag about PATH living in .zshrc; that's deliberate here
  (setq exec-path-from-shell-check-startup-files nil)
  ;; The default warns at 500ms: "Warning: exec-path-from-shell execution
  ;; took NNNms". A warm zsh here takes ~150ms but the first launch after a
  ;; boot can cross 1s, which is expected for one login shell and not worth
  ;; a warning on every cold start. Keep the alarm for pathological cases.
  (setq exec-path-from-shell-warn-duration-millis 3000)
  :config
  ;; a terminal Emacs already inherited a good PATH, only GUI needs this
  (when (memq window-system '(mac ns x))
    ;; exec-path-from-shell hard-errors if `default-directory' is remote, since
    ;; it would otherwise run the "login shell" on the wrong machine. Reloading
    ;; the config from a Tramp buffer (SPC h r r) used to trip that. `~' is an
    ;; absolute anchor, so this is local no matter where we were called from.
    (let ((default-directory (expand-file-name "~/")))
      (exec-path-from-shell-initialize))))

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
  (setq evil-undo-system `undo-fu))

(use-package evil-collection
  :after evil
  :config
  ;; vterm/eat: terminal buffers start in insert state so keys reach the
  ;; program instead of evil, and p/P paste with the terminal's own yank.
  ;; Without these the Claude Code buffer opens in normal state and swallows
  ;; everything you type.
  (setq evil-collection-mode-list '(dashboard dired ibuffer magit vterm eat))
  (evil-collection-init))

(use-package evil-tutor)

;; fix SPC RET and TAB in evil mode so can interact with regular emacs
(with-eval-after-load `evil-maps
    (define-key evil-motion-state-map (kbd "SPC") nil)
    (define-key evil-motion-state-map (kbd "TAB") nil)
    (define-key evil-motion-state-map (kbd "RET") nil)
)

;; One RET to run an Ex command.
;; `ivy-mode' points `completion-in-region-function' at
;; `ivy-completion-in-region', which opens a *recursive* minibuffer on top of
;; the ":" prompt (allowed because we set `enable-recursive-minibuffers'). The
;; first RET only dismisses that inner ivy prompt, so ":w" needs a second RET
;; to actually run. Emacs' default UI completes in place with a *Completions*
;; buffer instead, so one RET is enough. Scoped to the Ex minibuffer, so ivy
;; still handles completion-at-point everywhere else.
(defun my/evil-ex-use-default-completion ()
  "Use the default completion UI in the evil Ex command line."
  (setq-local completion-in-region-function #'completion--in-region))

(advice-add 'evil-ex-setup :after #'my/evil-ex-use-default-completion)

;; allow redo, emacs 28+ only
(evil-set-undo-system 'undo-redo)

(setq org-return-follows-link t)

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-nerd-commenter
  :after evil
  :config
  ;; gc as a comment operator in normal/visual, like vim-commentary
  (evil-define-key '(normal visual) 'global (kbd "gc") #'evilnc-comment-operator))

(use-package vundo
  :commands vundo
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols))

(use-package undo-fu-session
  :config
  ;; compress and store undo history under ~/.emacs.d/undo-fu-session/
  (undo-fu-session-global-mode))

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

(defvar my/gnu-libtool
  (if (eq system-type 'darwin) "glibtool" "libtool")
  "Name of the GNU libtool binary.
On macOS `libtool' is Apple's static-library archiver, a different
program; Homebrew installs GNU libtool as `glibtool'.")

(defvar my/vterm-buildable-p
  (and (fboundp 'module-load)
       (executable-find "cmake")
       (executable-find my/gnu-libtool)
       t)
  "Non-nil when this machine can compile vterm's C module.")

(when my/vterm-buildable-p
  (use-package vterm
    :straight t
    :commands (vterm vterm-other-window)
    :custom
    (vterm-max-scrollback 10000)
    (vterm-kill-buffer-on-exit t)
    ;; let vterm own C-c, TAB and friends so TUIs get them
    (vterm-timer-delay 0.01)
    ;; straight.el only clones and byte-compiles the elisp - it does not run
    ;; cmake. vterm.el builds its own C module the first time it is loaded,
    ;; normally stopping to ask first. Skip the prompt and just build.
    (vterm-always-compile-module t)
    :config
    ;; no line numbers in a terminal, they steal columns from the TUI
    (add-hook 'vterm-mode-hook (lambda () (display-line-numbers-mode -1)))))

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
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  (enable-recursive-minibuffers t)
  :config
  (ivy-mode))

(use-package all-the-icons-ivy-rich
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :after ivy
  :init (ivy-rich-mode 1) ;; this gets us descriptions in M-x.
  :custom
  (ivy-virtual-abbreviate 'full)
  (ivy-rich-switch-buffer-align-virtual-buffer t)
  (ivy-rich-path-style 'abbrev)
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

;; Remote dired: turn font-lock off.
;;
;; Dired's font-lock decides each line's face by asking the filesystem about
;; the file - `file-truename', `file-exists-p', `file-directory-p', and more
;; again for symlinks. Locally that's free. Over Tramp each one is a round
;; trip, so colouring a listing costs more than fetching it.
;;
;; Measured on antpod (~170ms round trip), first visit to a directory:
;;   font-lock on   2.18s
;;   font-lock off  0.21s
;; Once Tramp's attribute cache is warm both drop to ~0.18s, so this is
;; specifically about the first visit and about anything that outlives
;; `remote-file-name-inhibit-cache'.
;;
;; This is the Tramp maintainer's own recommendation - see Emacs bug#59151.
;; Cost: no colours in remote dired. `M-x font-lock-mode' re-enables per buffer.
(add-hook 'dired-mode-hook
          (lambda ()
            (when (file-remote-p default-directory)
              (font-lock-mode -1))))

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

(use-package editorconfig
  :diminish
  :config
  (editorconfig-mode 1))

;; :defer t — magit was the single biggest startup cost (~4s) and every
;; entry point used here (magit-status, the dispatch menus, blame, log) is
;; autoloaded, so nothing needs it at init. It loads on the first SPC g …
;; instead. :custom still applies immediately; the diff-hl magit hooks are
;; plain add-hook on symbols, which is defer-safe.
(use-package magit
  :defer t
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package diff-hl
  :config
  (global-diff-hl-mode)
  ;; show changes live while typing, not only after save
  (diff-hl-flydiff-mode)
  ;; no fringes in the terminal - fall back to the margin there
  (unless (display-graphic-p)
    (diff-hl-margin-mode))
  ;; keep the gutter in sync with magit stages/unstages
  (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

(use-package eat
  :straight (:type git
                   :host codeberg
                   :repo "akib/emacs-eat"
                   :files ("*.el" ("term" "term/*.el") "*.texi"
                           "*.ti" ("terminfo/e" "terminfo/e/*")
                           ("terminfo/65" "terminfo/65/*")
                           ("integration" "integration/*")
                           (:exclude ".dir-locals.el" "*-tests.el"))))

(use-package inheritenv
  :straight (:type git :host github :repo "purcell/inheritenv"))

;; Claude goes in a vertical split on the right, not stacked below.
;; The package default is `claude-code-display-buffer-below', i.e.
;; `display-buffer-below-selected'. A side window suits a persistent chat pane
;; better than a plain split: other buffers won't get displayed into it, so
;; opening a file while point is in the Claude window can't hijack the pane,
;; and `SPC w D' (`delete-other-windows') leaves Claude visible.
(defun my/claude-code-display-buffer-right (buffer)
  "Display the Claude BUFFER in a side window on the right."
  (display-buffer buffer
                  '((display-buffer-in-side-window)
                    (side . right)
                    (slot . 0)
                    (window-width . 0.4))))

(use-package claude-code
  :straight (:type git :host github :repo "stevemolitor/claude-code.el"
                   :branch "main" :depth 1
                   :files ("*.el" (:exclude "images/*")))
  ;; C-c c is the prefix for the whole command map, works in insert state too
  :bind-keymap ("C-c c" . claude-code-command-map)
  ;; after C-c c M, bare M keeps cycling default -> auto-accept -> plan
  :bind (:repeat-map claude-code-mode-repeat-map
         ("M" . claude-code-cycle-mode))
  :custom
  ;; vterm renders the TUI faster, but only exists if its module built; fall
  ;; back to eat otherwise. locate-library, not featurep: vterm is deferred.
  (claude-code-terminal-backend (if (locate-library "vterm") 'vterm 'eat))
  (claude-code-display-window-fn #'my/claude-code-display-buffer-right)
  :config
  ;; line numbers in a terminal buffer only steal horizontal space from the TUI
  (add-hook 'claude-code-start-hook
            (lambda () (display-line-numbers-mode -1)))
  (claude-code-mode))

(use-package lsp-mode 
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  ;; python-mode is hooked to `lsp-deferred' in the Python Mode block, not
  ;; here - having both meant LSP was started twice per python buffer.
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
)

;; Suppress native compilation warnings
(setq native-comp-async-report-warnings-errors 'silent)

(setq read-process-output-max (* 1024 1024)) ;; 1MB per read from the server process
(setq gc-cons-threshold (* 100 1024 1024))   ;; fewer, larger GCs while lsp is chatting
(setq lsp-idle-delay 0.5)                    ;; batch work until typing pauses
(setq lsp-log-io nil)                        ;; logging every message is a big perf hit

;; Guard the first-time install: straight aborts the whole config load when a
;; clone fails, and this machine's network has dropped GitHub DNS before. Once
;; the repo exists locally the condition-case never fires again.
(condition-case err
    (use-package lsp-ui
      :hook (lsp-mode . lsp-ui-mode)
      :custom
      (lsp-ui-doc-enable t)
      (lsp-ui-doc-position 'at-point)
      (lsp-ui-doc-show-with-mouse t)    ; popup on mouse hover, like VS Code
      (lsp-ui-doc-show-with-cursor t)   ; resting point on a symbol pops it too
      (lsp-ui-doc-delay 0.5)            ; after this many idle seconds
      (lsp-ui-sideline-show-diagnostics t)
      (lsp-ui-sideline-show-code-actions t)
      (lsp-ui-sideline-show-hover nil)) ; hover text inline is noise, doc popup covers it
  (error (message "lsp-ui unavailable this session (offline?): %s"
                  (error-message-string err))))

;; scope-aware highlighting from the server (lsp-mode feature, not lsp-ui):
;; clangd supports it, pylsp doesn't - python just keeps regular font-lock
(setq lsp-semantic-tokens-enable t)

(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

(use-package elpy
  :straight t
  :init
  (elpy-enable))

(setq lsp-pylsp-server-command "pylsp")
(setq lsp-ruff-lsp-server-command "ruff-lsp")

;; LSP used to be attached to python-mode from three places at once: here, the
;; :hook in the lsp-mode block, and the :hook in the python-mode block. The
;; python-mode one is the single source now. `elpy-enable' was likewise called
;; twice - once here and once in elpy's own :init.

   ;; A python shell for every buffer
(add-hook 'elpy-mode-hook (lambda () (elpy-shell-toggle-dedicated-shell 1)))

   ;;(add-hook 'python-mode-hook #'python-cello-mode 1)
   ;; ipython when it exists, plain python3 otherwise so `run-python' never
   ;; dies on a machine without it. --pylab=qt5 was dropped: it needs
   ;; matplotlib + PyQt installed in ipython's own environment, and without
   ;; them every shell start printed a ModuleNotFoundError traceback.
   (if (executable-find "ipython3")
       (setq python-shell-interpreter "ipython3"
             python-shell-interpreter-args "--simple-prompt -i")
     (setq python-shell-interpreter "python3"
           python-shell-interpreter-args "-i"))

   ;; Real time syntax check in python
   (when (require 'flycheck nil t)
         (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
         (add-hook 'elpy-mode-hook 'flycheck-mode))

(defun my/python-project-venv ()
  "Find the project virtualenv for the current buffer, or nil.
Walks up from the buffer's directory looking for a .venv/ or venv/
directory that contains bin/python."
  (unless (file-remote-p default-directory)
    (let ((start (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
          found)
      (locate-dominating-file
       start
       (lambda (dir)
         (let ((hit (seq-find
                     (lambda (name)
                       (file-executable-p
                        (expand-file-name (concat name "/bin/python") dir)))
                     '(".venv" "venv"))))
           (when hit (setq found (expand-file-name hit dir)))
           hit)))
      found)))

(defun my/python-apply-env (env)
  "Point the python IDE at ENV, a virtualenv or conda env directory.
Sets jedi's environment for pylsp (buffer-locally, read when the
workspace initializes) and activates the env with pyvenv so new
subprocesses inherit its PATH."
  (setq-local lsp-pylsp-plugins-jedi-environment env)
  (when (and (fboundp 'pyvenv-activate)
             (not (equal (bound-and-true-p pyvenv-virtual-env)
                         (file-name-as-directory (expand-file-name env)))))
    (pyvenv-activate env)))

(defun my/python-auto-env ()
  "Auto-wire the project's .venv into the IDE, when there is one."
  (when-let ((venv (my/python-project-venv)))
    (my/python-apply-env venv)
    (message "python env: %s" (abbreviate-file-name venv))))

;; lsp-deferred delays the server start, so by the time the workspace
;; initializes the buffer-local jedi environment is already in place,
;; regardless of hook order
(add-hook 'python-mode-hook #'my/python-auto-env)

(defun my/python-conda-envs ()
  "Conda/mamba environment directories from the usual install locations."
  (let (envs)
    (dolist (base '("~/miniconda3" "~/anaconda3" "~/miniforge3"
                    "~/mambaforge" "~/.conda" "/opt/miniconda3"
                    "/opt/homebrew/Caskroom/miniconda/base"))
      (let ((dir (expand-file-name "envs" base)))
        (when (file-directory-p dir)
          (dolist (env (directory-files dir t "\\`[^.]"))
            (when (file-executable-p (expand-file-name "bin/python" env))
              (push env envs))))))
    (nreverse envs)))

(defun my/python-choose-env (env)
  "Pick ENV (a conda env or any virtualenv directory) for this buffer's IDE.
Offers discovered conda envs plus the auto-detected project venv; any
other directory can be typed in. Restarts pylsp so jedi switches
immediately."
  (interactive
   (list (let ((cands (append (when-let ((v (my/python-project-venv)))
                                (list v))
                              (my/python-conda-envs))))
           (if cands
               (completing-read "Python env (or type a path): " cands nil nil)
             (read-directory-name "Python env directory: ")))))
  (setq env (expand-file-name env))
  (unless (file-executable-p (expand-file-name "bin/python" env))
    (user-error "%s has no bin/python" env))
  (my/python-apply-env env)
  (when (and (fboundp 'lsp-workspaces) (lsp-workspaces))
    (lsp-workspace-restart (car (lsp-workspaces))))
  (message "python env: %s" (abbreviate-file-name env)))

(use-package python-mode
  :straight nil
  :hook (python-mode . lsp-deferred) ;; when open python file, turn on LSP mode
)

;; (setq python-shell-interpreter "python3") ;; ensure use python3 as interpreter

  (use-package company
    :after lsp-mode
    :hook (prog-mode . company-mode)
    :bind (:map company-active-map
           ("<tab>" . company-complete-selection)
           :map lsp-mode-map
           ("<tab>" . company-indent-or-complete-common))
    :custom
    (company-minimum-prefix-length 1)
    (company-idle-delay 0.0)
    ;; Keep company out of evil's ":" command line.
    ;;
    ;; `global-company-mode' adds `company--minibuffer-on' to
    ;; `minibuffer-setup-hook' at depth 100, and with `company-global-minibuffer'
    ;; at its default of t that turns company on in any minibuffer that has a
    ;; buffer-local `completion-at-point-functions'. `evil-ex-setup' adds exactly
    ;; such a local capf, so every ":" prompt got company - and with idle-delay
    ;; 0.0 and prefix-length 1 above, typing ":w" instantly popped a list of ex
    ;; commands plus every Emacs command. Worse, `company-active-map' binds RET
    ;; to `company-complete-selection', so the first RET only accepted the
    ;; candidate and ":w" needed a second RET to actually run.
    ;;
    ;; The variable also accepts a predicate, called with the minibuffer
    ;; current. Evil sets `evil-ex-original-buffer' buffer-locally just before
    ;; `evil-ex-setup', so it is a reliable marker for the Ex prompt. This keeps
    ;; company in other minibuffers, like `eval-expression'.
    (company-global-minibuffer
     (lambda () (not (local-variable-p 'evil-ex-original-buffer)))))

;; company box enables a box with icons to show up during competion much like vscode completions.
(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

(defun my-eshell-init ()
  (company-mode -1))

(add-hook 'eshell-mode-hook #'my-eshell-init) ;; disable company in eshell mode

(use-package flycheck
  :defer t
  :diminish
  :init (global-flycheck-mode))


(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets)

(use-package apheleia
  :commands (apheleia-format-buffer apheleia-mode)
  ;; load shortly after startup rather than during it
  :defer 1
  :config
  (apheleia-global-mode +1))

(use-package dumb-jump
  :config
  (setq dumb-jump-prefer-searcher 'rg)
  ;; register as the xref fallback: lsp's backend still wins when it's active
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;; multiple candidates land in the minibuffer (ivy) instead of an *xref* window
(setq xref-show-definitions-function #'xref-show-definitions-completing-read)

(use-package avy
  :commands (avy-goto-char-timer avy-goto-line))

(use-package ws-butler
  :diminish
  :hook (prog-mode . ws-butler-mode))

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

;; Keep projectile out of the mode line. `projectile-mode-line' (what used to
;; be here) no longer exists in projectile - nothing reads it, so it was a
;; no-op and the remote latency it was meant to fix was still there. The live
;; knob is `projectile-dynamic-mode-line', which recomputes the project name
;; on every window-configuration change; over Tramp that means constant
;; round trips. It needs `setopt' rather than `setq' because its :set function
;; is what removes the hook once `projectile-mode' is already on.
(setopt projectile-dynamic-mode-line nil)

(use-package wgrep
  :custom
  ;; save the touched buffers when applying, no "modified buffer" pile afterwards
  (wgrep-auto-save-buffer t))

(use-package ein)

(setq tramp-default-method "ssh")

;; --- fewer round trips -------------------------------------------------
(setq tramp-verbose 1                               ; default 3; logging isn't free
      remote-file-name-inhibit-locks t              ; no .#lock files over ssh
      remote-file-name-inhibit-auto-save-visited t  ; don't auto-save remote buffers
      tramp-completion-use-auth-sources nil)        ; skip auth-source scans while completing

;; Raise `tramp-verbose' to 6 and check *tramp/...* buffers when debugging.

(with-eval-after-load 'tramp-sh
  ;; The big one. Above this size Tramp stops streaming a file inline over the
  ;; connection it already has and spawns a fresh scp process instead. The
  ;; default is 10 KiB, so nearly every open and save pays for a new
  ;; connection. 1 MiB keeps ordinary source files inline.
  (setq tramp-copy-size-limit (* 1024 1024)
        tramp-use-scp-direct-remote-copying t))

;; --- connections that survive -------------------------------------------
;; Tramp's computed ControlMaster options end in ControlPersist=no: the ssh
;; master dies the moment Tramp lets go of it, so revisiting a host after
;; `SPC r d', an idle disconnect, or an Emacs restart pays the full ssh
;; handshake again. ControlPersist=yes daemonizes the master instead, so the
;; next connection to the same host is instant.
;;
;; The ControlPath is *relative* on purpose - this matches what Tramp itself
;; computes on macOS (see Bug#19702 in tramp-sh.el): unix sockets cap the
;; path at ~104 chars, and an absolute /var/folders/... path plus the hash
;; blows past it and ssh refuses to make the socket. ssh's cwd here is the
;; temp dir, so the sockets still land there. %%C is the hashed
;; host/user/port, doubled because the string is used as a format string.
;; Cost: idle ssh master processes linger after Emacs exits. They're
;; harmless, but `SPC r D' (tramp-cleanup-all-connections) kills them.
(with-eval-after-load 'tramp
  (setq tramp-ssh-controlmaster-options
        "-o ControlMaster=auto -o ControlPath=tramp.%%C -o ControlPersist=yes"))

;; --- let async processes reuse the connection --------------------------
;; Tramp 2.7+. Without this, every async process - magit, grep, compile -
;; opens its own ssh connection. Protocol matches `tramp-default-method'.
(connection-local-set-profile-variables
 'remote-direct-async-process
 '((tramp-direct-async-process . t)))

(connection-local-set-profiles
 '(:application tramp :protocol "ssh")
 'remote-direct-async-process)

;; `compile' deliberately switches ssh connection sharing off. Undo that.
(with-eval-after-load 'compile
  (remove-hook 'compilation-mode-hook
               #'tramp-compile-disable-ssh-controlmaster-options))

;; Magit hangs when staging hunks on a direct-async connection unless it uses
;; a pty instead of a pipe - see the `magit-tramp-pipe-stty-settings'
;; docstring. Caveat: pty mode breaks on repos with DOS line endings.
(with-eval-after-load 'magit
  (setq magit-tramp-pipe-stty-settings 'pty))

;; --- stop other packages hammering the connection ----------------------
;; vc shells out to git every time you open a remote file, which is the most
;; noticeable single source of latency. Magit doesn't go through vc.el, so
;; this costs nothing but the VC modeline and `vc-' commands on remote files.
(with-eval-after-load 'tramp
  (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)" vc-ignore-dir-regexp tramp-file-name-regexp)))

;; recentf stats every entry when it cleans up; over ssh that stalls Emacs.
(setq recentf-auto-cleanup 'never)

;; Don't auto-register remote dirs as projectile projects (same guard Doom
;; ships): visiting a file on a server otherwise adds the project to the
;; known list, and later projectile features try to index it over ssh.
;; Explicitly invoked SPC p commands inside a remote project still work.
(setq projectile-ignored-project-function #'file-remote-p)

;; Measured against antpod (~170ms round trip): re-listing a directory costs
;; 0.8s once the attribute cache has expired, and 0.00s while it is still
;; warm. The default window is only 10 seconds, so any pause longer than that
;; makes revisiting a directory expensive again. 60s covers normal browsing.
;; Cost: a listing can be up to a minute stale - `g' in dired forces a refresh.
(setq remote-file-name-inhibit-cache 60)

;; Backups of remote files were being *copied to the local trash dir* through
;; Tramp (backup-directory-alist sends everything there), so the first save
;; of each session paid a full extra file transfer. No backups for remote
;; files at all; git is the backup on servers anyway.
(setq backup-enable-predicate
      (lambda (name)
        (and (normal-backup-enable-predicate name)
             (not (file-remote-p name)))))

;; auto-save (#file#) for remote buffers goes to a local dir, not over ssh
(setq tramp-auto-save-directory
      (expand-file-name "tramp-autosave" user-emacs-directory))

;; Make Tramp use the remote login shell's PATH instead of its hardcoded
;; /usr/bin:/bin. Without this, anything pip/cargo installs on the server
;; (~/.local/bin - pylsp lives there) is invisible to Emacs.
(with-eval-after-load 'tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(defvar-local my/lsp-remote-ok nil
  "Non-nil when this remote buffer opted in to LSP via `my/lsp-remote-start'.")

(defun my/lsp-skip-remote (orig &rest args)
  "Don't auto-start LSP on Tramp buffers - only `my/lsp-remote-start' may.
Root-hunting over ssh costs round trips in every remote buffer, so it
has to be an explicit per-buffer decision, not a find-file side effect."
  (unless (and (file-remote-p default-directory)
               (not my/lsp-remote-ok))
    (apply orig args)))

;; advice-add works on autoloads, so this catches the first remote file too
(advice-add 'lsp :around #'my/lsp-skip-remote)
(advice-add 'lsp-deferred :around #'my/lsp-skip-remote)

(defun my/lighten-remote-buffer ()
  "Switch off per-keystroke remote round trips in Tramp buffers."
  (when (file-remote-p default-directory)
    (when (bound-and-true-p flycheck-mode)
      (flycheck-mode -1))
    ;; eldoc runs on *every cursor movement*. With lsp or elpy behind it that
    ;; is a round trip per keypress - the single worst offender for "typing
    ;; lags". The article disables it on remote for exactly this reason.
    (eldoc-mode -1)
    ;; capf is what company queries; on remote its backends hit the filesystem.
    ;; Killing it leaves dabbrev-style completion, which is local and instant.
    (setq-local completion-at-point-functions nil)
    ;; `company-idle-delay' is 0.0 globally; on a remote buffer that's a
    ;; completion pass on every keystroke.
    (setq-local company-idle-delay 0.3)))

;; find-file-hook runs after the major mode and its hooks, so anything
;; global-flycheck-mode just switched on gets switched back off here.
(add-hook 'find-file-hook #'my/lighten-remote-buffer)

;; --- doom-modeline ------------------------------------------------------
;; doom-modeline recomputes the file name, icon, VCS state and env from hooks
;; that fire constantly. The worst is `evil-insert-state-exit-hook': every
;; single time you leave insert mode it re-derives the buffer file name, which
;; over Tramp is a stall on every edit. See doom-modeline-segments.el:355.
;;
;; The article removes these hooks outright. Advising instead keeps the
;; modeline fully working locally and only skips the work on remote buffers.
(defun my/skip-on-remote (orig &rest args)
  "Run ORIG only when the current buffer is local."
  (unless (file-remote-p default-directory)
    (apply orig args)))

(dolist (fn '(doom-modeline-update-buffer-file-name
              doom-modeline-update-buffer-file-icon
              doom-modeline-update-vcs
              doom-modeline-update-env))
  (advice-add fn :around #'my/skip-on-remote))

;; --- memoize what gets asked over and over ------------------------------
;; "If sending calls over TRAMP is so expensive, the best thing we can do is
;; not run them." `project-current', `vc-git-root' and `magit-toplevel' are
;; called constantly, and for a given directory the answer never changes.
(defun memoize-remote (key cache orig-fn &rest args)
  "Memoize ORIG-FN's result in CACHE when KEY is a remote path."
  (if (and key (file-remote-p key))
      (if-let ((current (assoc key (symbol-value cache))))
          (cdr current)
        (let ((current (apply orig-fn args)))
          (set cache (cons (cons key current) (symbol-value cache)))
          current))
    (apply orig-fn args)))

(defvar project-current-cache nil)
(defun memoize-project-current (orig &optional prompt directory)
  (memoize-remote (or directory
                      (bound-and-true-p project-current-directory-override)
                      default-directory)
                  'project-current-cache orig prompt directory))
(advice-add 'project-current :around #'memoize-project-current)

(defvar vc-git-root-cache nil)
(defun memoize-vc-git-root (orig file)
  (let ((value (memoize-remote (file-name-directory file)
                               'vc-git-root-cache orig file)))
    ;; vc-git-root sometimes returns nil even when a root is there; don't
    ;; cache that or the directory stays broken for the session.
    (when (null (cdr (car vc-git-root-cache)))
      (setq vc-git-root-cache (cdr vc-git-root-cache)))
    value))
(advice-add 'vc-git-root :around #'memoize-vc-git-root)

(defvar magit-toplevel-cache nil)
(defun memoize-magit-toplevel (orig &optional directory)
  (memoize-remote (or directory default-directory)
                  'magit-toplevel-cache orig directory))
(advice-add 'magit-toplevel :around #'memoize-magit-toplevel)

(defun tramp-flush-memoized-caches ()
  "Forget everything `memoize-remote' has cached.
Run this if a project root or git root changes under you."
  (interactive)
  (setq project-current-cache nil
        vc-git-root-cache nil
        magit-toplevel-cache nil)
  (message "Tramp memo caches cleared"))

;; --- magit ---------------------------------------------------------------
;; A single magit command can be 30 shell invocations. Prefer `magit-dispatch'
;; and `magit-file-dispatch' over the status buffer on remote repos.
(with-eval-after-load 'magit
  (setq magit-commit-show-diff nil          ; C-c C-d shows it on demand
        magit-branch-direct-configure nil   ; don't read git vars in branch menu
        magit-refresh-status-buffer nil))   ; refresh manually with g

;; lsp-mode only starts servers on remote files through clients registered
;; with :remote? t. Register tramp-aware copies of the two servers I use.
(with-eval-after-load 'lsp-mode
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "pylsp")
                    :major-modes '(python-mode python-ts-mode)
                    :remote? t
                    :server-id 'pylsp-remote))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "clangd")
                    :major-modes '(c-mode c++-mode)
                    :remote? t
                    :server-id 'clangd-remote)))

(defun my/lsp-remote-start ()
  "Opt this remote buffer in to LSP and start it.
Undoes the per-buffer economies from `my/lighten-remote-buffer' that LSP
needs back, then starts lsp. The server must exist on the remote host."
  (interactive)
  (unless (file-remote-p default-directory)
    (user-error "Local buffer - LSP already starts automatically here"))
  (setq my/lsp-remote-ok t)
  ;; my/lighten-remote-buffer nils capf buffer-locally; drop that override so
  ;; lsp-completion-mode can install its completion function
  (kill-local-variable 'completion-at-point-functions)
  (eldoc-mode 1)
  (lsp))

;; antpod - mirrors the `antpod' alias in ~/.zshrc. Non-default port goes after
;; a #; the -i ~/.ssh/id_ed25519 from the alias is redundant here since ssh
;; tries that key by default.
(defun visit-remote-project-1 ()
  (interactive)
  (find-file "/ssh:edwardosunny@198.145.108.45#15358:~/"))
;; petri1 - mirrors the `petri1' alias in ~/.zshrc (autoresearch petri).
(defun visit-remote-project-2 ()
  (interactive)
  (find-file "/ssh:root@213.173.110.225#28297:~/"))
(defun visit-remote-project-3 ()
  (interactive)
  (find-file "/ssh:edward@scai4.cs.ucla.edu:~/"))

;; ad hoc servers (lambda) go last so easier to remove/add
(defun visit-remote-project-4 ()
  (interactive)
  (find-file "/ssh:u-ril@192.168.0.69:~/"))
(defun visit-remote-project-5 ()
  (interactive)
  (find-file "/ssh:ubuntu@104.171.203.34:~/"))

(defun dired-local-home ()
  "Open dired on the local home directory, even from a Tramp buffer."
  (interactive)
  ;; `expand-file-name' resolves ~ against the local HOME regardless of a
  ;; remote `default-directory', so this is always local.
  (dired (expand-file-name "~/")))

(defun tramp-disconnect-here ()
  "Drop the Tramp connection this buffer is using.
Buffers stay open and silently reconnect next time they're touched."
  (interactive)
  (tramp-cleanup-this-connection))

(defun tramp-disconnect-everything ()
  "Drop every Tramp connection and kill all remote buffers."
  (interactive)
  (tramp-cleanup-all-buffers))

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
;; changes certain keywords to symbols, such as lambda!
;; (this was a `setq' on the mode variable before, which never actually
;; turned the mode on - it needs to be called as a function)
(global-prettify-symbols-mode 1)

(use-package all-the-icons
    :if (display-graphic-p)
)

(use-package all-the-icons-dired
    ;; Icons stat every entry to pick a glyph. In a local dired that's free; in
    ;; a Tramp dired it's one round trip per file, so skip it on remote paths.
    :hook (dired-mode . (lambda ()
                          (unless (file-remote-p default-directory)
                            (all-the-icons-dired-mode t))))
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
  ;; (setq dashboard-startup-banner "~/.emacs.d/img/nerv.png")  ;; use custom image as banner
  (setq dashboard-startup-banner "~/.emacs.d/img/guts.png") 
  (setq dashboard-image-banner-max-height 750)   ;; max custom banner image height 
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

(savehist-mode 1)            ;; Minibuffer history (M-x, file prompts, swiper) survives restarts.
(save-place-mode 1)          ;; Reopening a file puts point back where it was last time.
;; save-place normally stats every remembered file when saving its list;
;; with remote files in the list that means ssh round trips on exit.
(setq save-place-forget-unreadable-files nil)

(global-display-line-numbers-mode 1) ;; Display line numbers
(global-visual-line-mode t)  ;; Enable truncated lines
(menu-bar-mode -1)           ;; Disable the menu bar 
(scroll-bar-mode -1)         ;; Disable the scroll bar
(tool-bar-mode -1)           ;; Disable the tool bar
(setq org-edit-src-content-indentation 0) ;; Set src block automatic indent to 0 instead of 2.

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/org-roam")
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n")
      :unnarrowed t)
     ("p" "paper" plain 
      "* Summary\n%?\n\n* Key Claims\n\n* Method\n\n* Results\n\n* Strengths\n\n* Weaknesses\n\n* Connections\n"
      :if-new (file+head "papers/${slug}.org"
                         "#+title: ${title}\n#+filetags: :paper:\n#+date: %U\n#+ROAM_KEY: ${ref}\n#+ARXIV: \n\n")
      :unnarrowed t)
     ("i" "idea" plain 
      "* Description\n%?\n\n* Motivation\n\n* Related Notes\n\n* Methods\n\n* Next Steps\n"
      :if-new (file+head "ideas/${slug}.org"
                         "#+title: ${title}\n#+filetags: :idea:\n#+date: %U\n")
      :unnarrowed t)
     ("c" "concept" plain 
      "* Definition\n%?\n\n* Explanation\n\n* Examples\n\n* Related Concepts\n"
      :if-new (file+head "concepts/${slug}.org"
                         "#+title: ${title}\n#+filetags: :concept:\n#+date: %U\n")
      :unnarrowed t)
     ("m" "meeting" plain 
      "* Agenda\n\n* Notes\n%?\n\n* Action Items\n"
      :if-new (file+head "meetings/${slug}.org"
                         "#+title: ${title}\n#+filetags: :meeting:\n#+date: %U\n")
      :unnarrowed t)
     ("P" "project" plain 
      "* Methods Overview\n%?\n\n* Long Term Milestones\n\n* DO-List\n\n* Related Literature\n\n* Resources\n"
      :if-new (file+head "projects/${slug}.org"
                         "#+title: ${title}\n#+filetags: :project:\n#+date: %U\n")
      :unnarrowed t)
     ("n" "person" plain 
      "* Affiliation\n%?\n\n* Research Interests\n\n* Areas of Expertise\n\n* Interesting Publications\n\n- [[roam:]]\n\n* Notes\n\n* Related Projects\n\n* Meetings\n"
      :if-new (file+head "people/${slug}.org"
                         "#+title: ${title}\n#+filetags: :person:\n#+date: %U\n")
      :unnarrowed t)))
  :config
  (org-roam-setup)

  ;; 1. Inbox location
  (setq org-default-notes-file
        (expand-file-name "refile.org" org-roam-directory))

  ;; 2. Fast capture → inbox
  (setq org-capture-templates
        '(("r" "Refile (inbox)" entry
           (file org-default-notes-file)
           "* %?\n%U\n")))

  ;; 3. Refile targets: inbox + all Org-roam notes
  (defun my/org-roam-refile-targets ()
    "Return a list of all Org files under `org-roam-directory` for `org-refile-targets`."
    (mapcar (lambda (f) (list f :maxlevel 3))
            (directory-files-recursively org-roam-directory "\\.org$")))

  (setq org-refile-targets
        (append
         `((,org-default-notes-file :maxlevel . 3))
         (my/org-roam-refile-targets)))

  ;; 4. Quality-of-life refinements
  (setq org-outline-path-complete-in-steps nil
        org-refile-use-outline-path 'file
        org-refile-allow-creating-parent-nodes 'confirm))

(use-package org-roam-ui
  :straight
    (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
    :after org-roam
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
    ;; :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(use-package org-download
  :after org
  :defer nil
  :config
  ;; Save into a fixed directory (we'll override per-call below)
  (setq org-download-method 'directory
        org-download-heading-lvl 0
        org-download-timestamp "org_%Y%m%d-%H%M%S_"
        org-image-actual-width 900

        ;; Read PNG data from clipboard via xclip (Ubuntu)
        org-download-screenshot-method
        "xclip -selection clipboard -t image/png -o > %s")

  (defun my/org-roam-insert-clipboard-image ()
    "Save clipboard image into `org-roam-directory`/img and insert link."
    (interactive)
    (unless (boundp 'org-roam-directory)
      (user-error "org-roam-directory is not set"))
    (let* ((img-dir (expand-file-name "img" org-roam-directory)))
      (unless (file-directory-p img-dir)
        (make-directory img-dir t))
      ;; Temporarily override image dir so org-download writes into Roam img/
      (let ((org-download-image-dir img-dir))
        (org-download-screenshot))))

  ;; Evil: in ORG buffers, in *insert* state, C-v inserts clipboard image
  (with-eval-after-load 'evil
    (evil-define-key 'insert org-mode-map
      (kbd "C-v") #'my/org-roam-insert-clipboard-image)))

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

;; on a fresh clone without --recurse-submodules the submodule is an empty
;; directory; pull it here so the config bootstraps itself
(let ((chrono-el (expand-file-name "lisp/chronoscope/chronoscope.el"
                                   user-emacs-directory)))
  (unless (file-exists-p chrono-el)
    (message "chronoscope submodule missing, fetching it...")
    (let ((default-directory user-emacs-directory))
      (call-process "git" nil "*chronoscope-submodule*" nil
                    "submodule" "update" "--init" "lisp/chronoscope"))))

(use-package chronoscope
  :straight nil
  :load-path "lisp/chronoscope"
  :commands (chronoscope chronoscope-stopwatch chronoscope-timer))

(use-package sudo-edit
  :commands (sudo-edit sudo-edit-find-file))

(defun dt/show-and-copy-buffer-path ()
  "Show the full path of the current file in the minibuffer and copy it."
  (interactive)
  (let ((file-name (or (buffer-file-name) list-buffers-directory)))
    (if file-name
        (progn
          (message "%s" file-name)
          (kill-new file-name))
      (error "Buffer not visiting a file"))))

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
    ;; code: xref binds pick lsp when it's running, dumb-jump/rg otherwise,
    ;; so they work in remote buffers too (see Code Navigation)
    "c" '(:ignore t :wk "code")
    "c c" '(comment-line :wk "comment lines")
    "c a" '(lsp-execute-code-action :wk "code action")
    "c b" '(xref-go-back :wk "go back from definition")
    "c d" '(xref-find-definitions :wk "find definition")
    "c D" '(xref-find-references :wk "find references")
    "c e" '(flycheck-list-errors :wk "list errors")
    "c f" '(apheleia-format-buffer :wk "format buffer")
    "c h" '(lsp-describe-thing-at-point :wk "hover docs")
    "c n" '(flycheck-next-error :wk "next error")
    "c p" '(flycheck-previous-error :wk "previous error")
    "c R" '(lsp-rename :wk "rename symbol (lsp)")
    "c s" '(counsel-imenu :wk "jump to symbol in buffer")
    "c v" '(my/python-choose-env :wk "pick python env (conda/venv)")
    ;; git
    "g" '(:ignore t :wk "git")
    "g g" '(magit-status :wk "magit status")
    "g d" '(magit-dispatch :wk "magit dispatch (cheap on tramp)")
    "g f" '(magit-file-dispatch :wk "magit file dispatch")
    "g b" '(magit-blame-addition :wk "git blame")
    "g l" '(magit-log-buffer-file :wk "log for this file")
    "g j" '(diff-hl-next-hunk :wk "next changed hunk")
    "g k" '(diff-hl-previous-hunk :wk "previous changed hunk")
    "g s" '(diff-hl-stage-current-hunk :wk "stage hunk at point")
    "g x" '(diff-hl-revert-hunk :wk "revert hunk at point")
    ;; jump (avy)
    "j" '(:ignore t :wk "jump")
    "j j" '(avy-goto-char-timer :wk "jump to visible text")
    "j l" '(avy-goto-line :wk "jump to visible line")
    ;; undo tree
    "u" '(vundo :wk "visual undo tree")
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
    "fh"   '(dired-local-home :which-key "dired LOCAL home (escape tramp)")
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
    ;; resize windows
    "w[" '(evil-window-decrease-width :wk "decrease-window-width")
    "w]" '(evil-window-increase-width :wk "increase-window-width")
    "w-" '(evil-window-decrease-height :wk "decrease-window-height")
    "w=" '(evil-window-increase-height :wk "increase-window-height")
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
    "ov" '(vterm :wk "open vterm")
    "oV" '(vterm-other-window :wk "open vterm other window")
    ;; chronoscope timer/stopwatch (local package, see Chronoscope section)
    "os" '(chronoscope :wk "stopwatch/timer buffer")
    "oS" '(chronoscope-timer :wk "start countdown timer")
    ;; perspective.el workspaces
    "TAB" '(perspective-map :wk "Perspective") ;; Lists all the perspective keybindings
    ;; projectile
    "p" `(projectile-command-map :wk "Projectile command map")
    ;; claude code (full command map is on C-c c)
    "a" '(claude-code-transient :wk "claude code menu")
    ;; AUCTex bindings
    ;; previewing 
    "lpp" '(preview-buffer :wk "preview current latex buffer") 
    "lpa" '(preview-at-point :wk "toggle latex preview at point") 
    "lpd" '(preview-document :wk "preview current latex document") 
    ;; compiling latex
    "lca" '(TeX-command-run-all :wk "compile current document") 
    ;; speedbar "file tree"
    "sb"  '(speedbar :wk "toggle speedbar file summary/tree") 
    ;; ssh / remote
    "r" '(:ignore t :wk "remote")
    "r l" '(my/lsp-remote-start :wk "start LSP in this remote buffer")
    "r1"  '(visit-remote-project-1 :wk "connect to antpod")
    "r2"  '(visit-remote-project-2 :wk "connect to petri1")
    "r3"  '(visit-remote-project-3 :wk "connect to the server 3") 
    "r4"  '(visit-remote-project-4 :wk "connect to the server 4") 
    "r5"  '(visit-remote-project-5 :wk "connect to the server 5")
    ;; disconnecting
    "rd"  '(tramp-disconnect-here :wk "drop this tramp connection")
    "rD"  '(tramp-disconnect-everything :wk "drop ALL tramp conns + buffers")
    ;; org
    "oc"  '(org-capture :wk "org capture")
    ;; org roam
    "of"  '(org-roam-node-find :wk "org roam find node")
    "ol"  '(org-roam-buffer-toggle :wk "org roam buffer toggle")
    ;; org roam ui
    "og"  '(org-roam-ui-mode :wk "org roam ui graph")
    )
  )

(defun reload-init-file()
  (interactive)
  (load-file user-init-file)
  (load-file user-init-file)
  )

;; regular org
;; C-c C-l inserts link
(global-set-key (kbd "C-SPC") 'completion-at-point)
;; org-roam
(global-set-key (kbd "C-a") 'org-roam-node-insert)
;; refile C-c C-w

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

(global-set-key [escape] `keyboard-escape-quit)

(when (and (memq window-system '(mac ns x))
           (fboundp 'exec-path-from-shell-copy-envs))
  (let ((default-directory (expand-file-name "~/")))
    (ignore-errors
      (exec-path-from-shell-copy-envs
       '("ANTHROPIC_API_KEY" "OPENROUTER_API_KEY" "OPENAI_API_KEY")))))

(use-package clanker
  :straight nil
  :load-path "~/Documents/personal/clanker.el"
  :custom
  (clanker-binary "/opt/homebrew/bin/clanker")
  (clanker-keymap-prefix "C-c k")
  :config
  (clanker-mode 1))

(nvmap :states '(normal visual) :keymaps 'override :prefix "SPC"
  "k"   '(:ignore t :wk "clanker")
  "k k" '(clanker-edit :wk "edit region (Cmd+K)")
  "k g" '(clanker-generate :wk "generate at point")
  "k f" '(clanker-fix :wk "fix region")
  "k e" '(clanker-explain :wk "explain region")
  "k c" '(clanker-chat :wk "chat")
  "k a" '(clanker-agent :wk "agent task")
  "k n" '(clanker-new-session :wk "new session")
  "k t" '(clanker-completion-mode :wk "toggle tab completion")
  "k m" '(clanker-set-model :wk "switch model")
  "k r" '(clanker-set-effort :wk "reasoning effort")
  "k i" '(clanker-complete :wk "complete at point")
  "k s" '(clanker-add-context :wk "reference region in chat"))
