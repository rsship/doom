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
      "C-x C-k" #'kill-current-buffer
      "C-x 0" #'doom/window-maximize-buffer)

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



(unless (package-installed-p 'lsp-mode)
  (package-install 'lsp-mode))

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


(unless (package-installed-p 'company)
  (package-install 'company))


;; Enable Company mode for completion
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

(defun my/compilation-start-in-root (command &optional comint)
  "Start compilation in the project's root directory and focus on the compilation window."
  (interactive
   (list
    (read-from-minibuffer "Compile command: "
                          (eval compile-command))
    current-prefix-arg))
  (let ((default-directory (or (projectile-project-root)
                               default-directory)))
    (compile command comint)))

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

(global-set-key [remap compile] 'my/compilation-start-in-root)

;;; rectangle mode visualization

;; (defface my-rectangle-mark-face
;;   '((t (:background "light blue" :extend t)))
;;   "Face for rectangle-mark-mode that only highlights the cursor line.")

;; (defun my-rectangle-mark-advice (&rest _)
;;   "Advice to change rectangle-mark-mode face."
;;   (if rectangle-mark-mode
;;       (setq-local face-remapping-alist
;;                   (cons '(region my-rectangle-mark-face) face-remapping-alist))
;;     (setq-local face-remapping-alist
;;                 (remove '(region my-rectangle-mark-face) face-remapping-alist))))

;; (advice-add 'rectangle-mark-mode :after #'my-rectangle-mark-advice)
