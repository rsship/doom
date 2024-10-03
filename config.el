;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(setq user-full-name "Salih Bozkaya aka (lord vader)"
       user-mail-address "bozkayasalih01x@gmail.com")

(dap-mode 1)
(which-key-mode -1)

(use-package vertico
  :init
  (vertico-mode)
  (vertico-flat-mode))

(use-package orderless
        :custom
        (completion-styles '(orderless basic))
        (completion-category-defaults nil)
        (completion-category-overrides '((file (styles partial-completion)))))

(setq minor-mode-alist nil)
(setq minor-mode-map-alist nil)
(require 'yasnippet)
(yas-global-mode 1)

(load-theme 'gruber-darker t)
(set-frame-font "Iosevka 17" nil t)
(setq display-line-numbers-type 'relative)

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

(setq doom-leader-key "C-x"
      doom-localleader-key "C-x")

(setq org-directory "~/org/")
(after! evil
        (defun my-escape ()
        (interactive)
        (evil-force-normal-state))

        (define-key evil-normal-state-map "zz" 'my-escape)
        (define-key evil-visual-state-map "zz" 'my-escape)
        (define-key evil-insert-state-map "zz" 'my-escape))

(setq evil-insert-state-cursor '(box "yellow")
      evil-normal-state-cursor '(box "yellow")
      evil-emacs-state-cursor  '(box "yellow"))

(custom-set-faces
 '(mc/cursor-face ((t (:inherit cursor)))))

(add-hook 'window-setup-hook #'toggle-frame-fullscreen)


(map! "C-x C-p" #'evil-window-up
      "C-x C-n" #'evil-window-down
      "C-x C-]" #'split-window-vertically
      "C-x k" #'kill-current-buffer)

;;; custom elisp functions
;;; comment in/out

(defun toggle-comment-region ()
  "Toggle comment on region if active, otherwise on current line."
  (interactive)
  (if (region-active-p)
      (let ((start (region-beginning))
            (end (region-end)))
        (comment-or-uncomment-region start end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

(global-set-key (kbd "C-x C-/") 'toggle-comment-region)

(setq doom-asterisk-buffer-regexp (rx bos "*" (* (not (any "*"))) "*" eos))


(setq +workspaces-main-buffer-filter
      (lambda (buf)
        (not (string-match-p doom-asterisk-buffer-regexp (buffer-name buf)))))

(after! lsp-mode
  (setq lsp-diagnostics-provider :none)
  (setq sp-ui-sideline-enable nil)
  (setq sp-modeline-diagnostics-enable nil)
  (setq sp-modeline-diagnostics-enable nil)
  (setq sp-signature-render-documentation nil)
  (setq sp-enable-symbol-highlighting nil)
  (setq sp-headerline-breadcrumb-enable nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-keymap-prefix "C-c l"))


(defun my/focus-on-compilation-buffer (buffer desc)
  "Focus on the compilation window BUFFER."
  (when (buffer-live-p buffer)
    (let ((window (get-buffer-window buffer)))
      (when window
        (select-window window)))))

(add-hook 'compilation-start-hook
          (lambda (proc)
            (when (eq (process-status proc) 'run)
              (my/focus-on-compilation-buffer (process-buffer proc) nil))))
;;; dired mode
(require 'dired)
(setq-default dired-dwim-target t)
(setq dired-listing-switches "-alh")

;;; Move Text
(require 'move-text)
(global-set-key (kbd "M-n") 'move-text-down)
(global-set-key (kbd "M-p") 'move-text-up)

(global-set-key(kbd "C-x P p") 'affe-find)
(global-set-key(kbd "C-x C-C") 'project-compile)

(defun my-delete-word-no-kill (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (delete-region
   (point)
   (progn
     (forward-word arg)
     (point))))

(defun my-read-shell-command-advice (orig-fun &rest args)
  "Advice to temporarily bind `kill-word' to our custom function."
  (let ((kill-word-fn (symbol-function 'kill-word)))
    (unwind-protect
        (progn
          (fset 'kill-word #'my-delete-word-no-kill)
          (apply orig-fun args))
      (fset 'kill-word kill-word-fn))))

(advice-add 'read-shell-command :around #'my-read-shell-command-advice)


(defun toggle-maximize-buffer ()
  "Maximize buffer if it's not maximized, restore if it is."
  (interactive) ;; toggle
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))

(global-set-key (kbd "C-x 0") 'toggle-maximize-buffer)

(defun my/rectangle-mark-cursor ()
  (setq cursor-type '(bar . 1)))

(defun my/restore-cursor ()
  (setq cursor-type 'box))

(add-hook 'rectangle-mark-mode #'my/rectangle-mark-cursor)

(defun my/rectangle-mark-mode-cursor-advice (&rest _)
  (if (bound-and-true-p rectangle-mark-mode)
      (my/rectangle-mark-cursor)
    (my/restore-cursor)))

(advice-add 'rectangle-mark-mode :after #'my/rectangle-mark-mode-cursor-advice)
(defun my/hl-line-range-function ()
  (when (not rectangle-mark-mode)
    (cons (line-beginning-position) (line-beginning-position 2))))

(setq hl-line-range-function #'my/hl-line-range-function)

(use-package multiple-cursors
  :ensure t
  :config
  (define-key input-decode-map [?\C-m] [C-m])
  (define-key evil-emacs-state-map (kbd "C-n") 'mc/mark-next-like-this)
  (define-key evil-normal-state-map (kbd "C-n") 'mc/mark-next-like-this)
  (define-key evil-emacs-state-map (kbd "C-.") 'mc/skip-to-next-like-this)
  (define-key evil-normal-state-map (kbd "C-.") 'mc/skip-to-next-like-this)
  (define-key evil-emacs-state-map (kbd "C-,") 'mc/skip-to-previous-like-this)
  (define-key evil-normal-state-map (kbd "C-,") 'mc/skip-to-previous-like-this)
  (define-key evil-normal-state-map (kbd "C-\\") 'mc/mmlte--down)
  (define-key evil-emacs-state-map (kbd "C-\\") 'mc/mmlte--down)
  (define-key evil-emacs-state-map (kbd "<return>") 'mc/keyboard-quit))


(after! multiple-cursors
  (defun my-mc-insert-newline ()
    "Insert a newline and indent at all cursor positions."
    (interactive)
    (mc/execute-command-for-all-cursors 'newline-and-indent))

  (define-key mc/keymap (kbd "M-<return>") 'my-mc-insert-newline))

(defun my/rectangle-mark-and-backward ()
  "Extend rectangle-mark selection and move backward one character."
  (interactive)
  (if (not rectangle-mark-mode)
      (rectangle-mark-mode 1)
    (rectangle-mark-mode-extend))
  (backward-char 1))

(define-key evil-normal-state-map (kbd ":") 'ignore)
(define-key evil-visual-state-map (kbd ":") 'ignore)
(global-set-key (kbd "C-x c-c") 'ignore )

(setq-default compilation-scroll-output t
              make-backup-files nil)

(setq compilation-always-kill t)
(setq read-process-output-max (* 1024 1024))
(setq gc-cons-threshold 100000000)


(when (eq system-type 'darwin)
  (setq ns-command-modifier 'meta)
  (setq ns-option-modifier 'none))

(use-package emacs
  :custom
  (enable-recursive-minibuffers t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  :init
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

(defun hide-minor-modes ()
  (setq minor-mode-alist nil))
(add-to-list 'mode-line-format '(:eval (hide-minor-modes)) t)

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Rust mode setup
(use-package rust-mode
  :ensure t
  :hook (rust-mode . lsp))

(use-package lsp-mode
  :ensure t
  :hook (rust-mode . lsp)
  :commands lsp)

(use-package dap-mode
  :ensure t
  :config
  (dap-auto-configure-mode)
  (require 'dap-lldb)
)
(use-package dap-ui
  :ensure t
  :config
  (dap-ui-mode 1)
  (dap-tooltip-mode 1))

(setq dap-lldb-debug-program `("/usr/bin/lldb"))

(defun cargo-project-root ()
  (let ((root (locate-dominating-file default-directory "Cargo.toml")))
    (unless root
      (error "Not inside a Cargo project"))
    root))

(setq dap-lldb-debugged-program-function
      (lambda () (read-file-name "Select binary: " (concat (cargo-project-root) "target/debug/"))))



(setq compilation-window-height 20)

(defun my-display-buffer-function (buffer alist)
  (if (and (eq major-mode 'compilation-mode)
           (not (get-buffer-window buffer)))
      (let ((window (split-window-vertically)))
        (set-window-buffer window buffer)
        window)
    (display-buffer-use-some-window buffer alist)))

(setq display-buffer-function 'my-display-buffer-function)

(add-to-list 'display-buffer-alist
             '("*compilation*"
               (display-buffer-reuse-window
                display-buffer-in-side-window)
               (side . bottom)
               (reusable-frames . visible)
               (window-height . 0.3)))


