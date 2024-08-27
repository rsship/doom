;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Salih Bozkaya aka (lord vader)"
       user-mail-address "bozkayasalih01x@gmail.com")

(setq doom-leader-key "C-x"
      doom-localleader-key "C-x")

(load-theme 'gruber-darker t)
(set-frame-font "Iosevka 17" nil t)

(setq display-line-numbers-type 'relative)

(setq org-directory "~/org/")
(after! evil
        (defun my-escape ()
        (interactive)
        (evil-force-normal-state))

        (define-key evil-normal-state-map "zz" 'my-escape)
        (define-key evil-visual-state-map "zz" 'my-escape)
        (define-key evil-insert-state-map "zz" 'my-escape))

        (after! evil
        (define-key evil-normal-state-map (kbd ":") #'execute-extended-command)
        (define-key evil-visual-state-map (kbd ":") #'execute-extended-command))



(ido-mode 1)
(ido-everywhere 1)
(setq evil-insert-state-cursor '(box "yellow")
      evil-normal-state-cursor '(box "yellow"))

(add-hook 'window-setup-hook #'toggle-frame-fullscreen)


(map! "C-x C-p" #'evil-window-up
      "C-x C-n" #'evil-window-down
      "C-x C-]" #'split-window-vertically
      "C-x k" #'kill-current-buffer)

(global-set-key (kbd "C-x SPC") 'rectangle-mark-mode)


;;;;;; custom elisp functions

;;; comment in or out
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

(after! ido
  (add-to-list 'ido-ignore-buffers doom-asterisk-buffer-regexp))

;; Hide these buffers from +workspace/display
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

;; Enable Company mode for completion
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

(require 'yasnippet)
(yas-global-mode 1)


;;; root compilation

;; (defun my/compilation-start-in-root (command &optional comint)
;;   "Start compilation in the project's root directory and focus on the compilation window."
;;   (interactive
;;    (list
;;     (read-from-minibuffer "Compile command: "
;;                           (eval compile-command))
;;     current-prefix-arg))
;;   (let ((default-directory (or (projectile-project-root)
;;                                default-directory)))
;;     (compile command comint)))

;; (global-set-key [remap compile] 'my/compilation-start-in-root)

;; Function to focus on the compilation window
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

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->")         'mc/mark-next-like-this)
(global-set-key (kbd "C-<")         'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<")     'mc/mark-all-like-this)
(global-set-key (kbd "C-\"")        'mc/skip-to-next-like-this)
(global-set-key (kbd "C-:")         'mc/skip-to-previous-like-this)

;;; dired mode
(require 'dired)
(setq-default dired-dwim-target t)
(setq dired-listing-switches "-alh")

;;; Move Text
(require 'move-text)
(global-set-key (kbd "M-n") 'move-text-down)
(global-set-key (kbd "M-p") 'move-text-up)


(global-set-key (kbd "C-x P p") 'affe-find)
(global-set-key (kbd "C-x C C") 'project-compile)


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



(global-set-key (kbd "C-M-h") 'backward-sexp)
(global-set-key (kbd "C-M-l") 'forward-sexp)

(defun toggle-maximize-buffer ()
  "Maximize buffer if it's not maximized, restore if it is."
  (interactive) ;; toggle
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))

(global-set-key (kbd "C-x 0") 'toggle-maximize-buffer)
