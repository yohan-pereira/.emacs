(require 'package)

(add-to-list 'package-archives '("elpa" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("f782ed87369a7d568cee28d14922aa6d639f49dd676124d817dd82c8208985d0" "eb0a314ac9f75a2bf6ed53563b5d28b563eeba938f8433f6d1db781a47da1366" default)))
 '(package-selected-packages
   (quote
    (go-mode yaml-mode evil-magit magit exec-path-from-shell markdown-mode helm-ag robe enh-ruby-mode auto-complete evil-smartparens smartparens ag dirtree paredit pastels-on-dark-theme dracula-theme geiser use-package helm evil-visual-mark-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;(require 'evil)
;;(evil-mode t)
;;(setq evil-want-C-u-scroll t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package helm
  :ensure t
  :config
  ;; helm
  ;; helm settings (TAB in helm window for actions over selected items,
  ;; C-SPC to select items)
  (require 'helm-config)
  (require 'helm-misc)
  (require 'helm-locate)
  (setq helm-quick-update t)
  (setq helm-bookmark-show-location t)
  (setq helm-buffers-fuzzy-matching t)

  (global-set-key (kbd "M-x") 'helm-M-x)
  ;; helm-find-files
  (global-set-key (kbd "C-x C-f") 'helm-find-files)

  (defun helm-my-buffers ()
    (interactive)
    (let ((helm-ff-transformer-show-only-basename nil))
      (helm-other-buffer '(helm-c-source-buffers-list)
         helm-c-source-elscreen
         helm-c-source-ctags
         helm-c-source-recentf
         helm-c-source-locate
       "*helm-my-buffers*"))))

(use-package helm-projectile
  :ensure t
  :config
  (require 'helm-projectile)
  (helm-projectile-on))

(use-package helm-ag
  :ensure t
  :config
  (require 'helm-projectile))

(use-package ag
  :ensure t)

(use-package evil
  :ensure t
  :config
  (add-to-list 'evil-emacs-state-modes 'geiser-repl-mode)
  (evil-mode t)
  (setq evil-want-C-u-scroll t)
  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-normal-state-map "\C-p" 'helm-projectile-find-file)
  (define-key evil-normal-state-map (kbd "C-S-p") 'helm-projectile-find-file-in-known-projects)
  (evil-set-initial-state 'term-mode 'emacs)
  (evil-set-initial-state 'shell-mode 'emacs)
  ;; make underscore and hyphen part of words during search
  (modify-syntax-entry ?_ "w")
  (modify-syntax-entry ?- "w")

  (define-key evil-insert-state-map (kbd "C-u")
    (lambda ()
      (interactive)
      (evil-delete (point-at-bol) (point)))))

(use-package org
  :ensure geiser)

(use-package projectile
  :ensure t
  :config
  (define-key
  global-map
  (kbd "<M-return>")
  (lambda ()
    (interactive)
    (projectile-run-shell))))

(use-package dirtree
  :ensure t)

(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)
  (add-hook 'minibuffer-setup-hook 'turn-on-smartparens-strict-mode))

(use-package evil-smartparens
  :ensure t
  :config
  (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))

(use-package eyebrowse
  :ensure t
  :config
  (eyebrowse-mode t)
  (setq lexical-binding t)
  (eyebrowse-setup-opinionated-keys))

(use-package dracula-theme
  :ensure t
  :config
  (load-theme 'dracula t)) 

(use-package shackle
  :ensure t
  :config
  (setq helm-display-function 'pop-to-buffer) ; make helm play nice
  (setq shackle-rules '(("\\`\\*helm.*?\\*\\'" :regexp t :popup t)))) 

(use-package auto-complete
  :ensure t
  :config
  (ac-config-default))

(use-package enh-ruby-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist
	       '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode)))

(use-package robe
  :ensure t
  :config
  (add-hook 'ruby-mode-hook 'robe-mode)
  (add-hook 'enh-ruby-mode-hook 'robe-mode)
  (add-hook 'robe-mode-hook 'ac-robe-setup)
  (define-key robe-mode-map (kbd "<C-return>") 'robe-jump)
  )

(use-package markdown-mode
  :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package magit
  :ensure t)

(use-package evil-magit
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package go-mode
  :ensure t)

(setq geiser-default-implementation 'racket)


(org-babel-do-load-languages
  'org-babel-load-languages
  '((scheme . t)
    (emacs-lisp . t)
    (ruby . t)
    (sh . t)))

;; esc quits
;;(defun minibuffer-keyboard-quit ()
;;  "Abort recursive edit.
;;In Delete Selection mode, if the mark is active, just deactivate it;
;;then it takes a second \\[keyboard-quit] to abort the minibuffer."
;;  (interactive)

;;      (setq deactivate-mark  t)
;;    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
;;    (abort-recursive-edit)))
;;(define-key evil-normal-state-map [escape] 'keyboard-quit)
;;(define-key evil-visual-state-map [escape] 'keyboard-quit)
;;(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
;;(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
;;(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
;;(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
;;(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
;;(global-set-key [escape] 'evil-exit-emacs-state)


(tool-bar-mode -1)
(toggle-scroll-bar -1)

;; mac specif
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)
(define-key global-map "\M-v" 'yank)
(define-key global-map "\M-c" 'ns-copy-including-modify)

;; ansi term related


;; kill bufffer on exit
(defun oleh-term-exec-hook ()
  (let* ((buff (current-buffer))
         (proc (get-buffer-process buff)))
    (set-process-sentinel
     proc
     `(lambda (process event)
        (if (string= event "finished\n")
            (kill-buffer ,buff))))))

(add-hook 'term-exec-hook 'oleh-term-exec-hook)

(setq js-indent-level 2)
