;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Salih Bozkaya"
       user-mail-address "bozkayasalih01x@gmail.com")

(setq doom-leader-key "C-x"
      doom-localleader-key "C-x")

(load-theme 'gruber-darker t)

(setq display-line-numbers-type 'relative)

(setq org-directory "~/org/")
(after! evil
  ;; Define a function to simulate ESC

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
;;(ido-ubiquitous-mode 1)
(setq evil-insert-state-cursor '(box "yellow")
      evil-normal-state-cursor '(box "yellow"))

(add-hook 'window-setup-hook #'toggle-frame-fullscreen)


(after! dired
  (map! "C-x C-p" #'dired))

(map! "C-x C-p" #'evil-window-up
        "C-x C-n" #'evil-window-down)

(map! "C-x C-]" #'split-window-vertically)
(map! "C-x k" #'kill-current-buffer)
(map! "C-x 0" #'doom/window-maximize-buffer)

;; (global-set-key (kbd "M-x") 'smex)
;; (global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; ;; This is your old M-x.
;; (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

