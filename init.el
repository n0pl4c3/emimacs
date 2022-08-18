;; Variables
;; Fonts
(defvar emimacs/default-fixed-font-size 150)
(defvar emimacs/default-variable-font-size 165)
(defvar emimacs/default-fixed-font "FiraCode Nerd Font")
(defvar emimacs/default-variable-font "ETBembo")

;;
;; Disable Startup Message
(setq inhibit-startup-message t)

;; Disable unneeded UI elements
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)

;; Enable Line Numbers
(global-display-line-numbers-mode 1)

(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(defun emimacs/configure-font-faces ()
 ;; Set Font
 (set-face-attribute 'default nil :font emimacs/default-fixed-font :height emimacs/default-fixed-font-size)

 ;; Set the fixed pitch face
 (set-face-attribute 'fixed-pitch nil :font emimacs/default-fixed-font  :height emimacs/default-fixed-font-size)

 ;; Set the variable pitch face
 (set-face-attribute 'variable-pitch nil :font emimacs/default-variable-font :height emimacs/default-variable-font-size :weight 'regular))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (emimacs/configure-font-faces))))
  (emimacs/configure-font-faces))

;; Package Sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; To keep the modeline cleaner
(unless (package-installed-p 'diminish)
  (package-install 'diminish))

(require 'diminish)

;; make customize use it's own file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Ivy (completions)
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :init
  (ivy-mode 1))

;; Icons for Doom Modeline
;; First time usage: Install fonts
(use-package all-the-icons
  :ensure t)

;; Counsel (Ivy for builtin commands)
(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-b" . counsel-switch-buffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil))

;; Doom Modeline for now
(use-package doom-modeline
  :ensure t
  :custom
  (doom-modeline-height 35)
  (column-number-mode t)
  :init (doom-modeline-mode 1))

;; Raindow Delimiters for LISP
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; For learning
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.5))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; Doom Themes for the time being
;; TODO  Replace with selfmade one later
(use-package doom-themes)

;; Set Theme 
(load-theme 'doom-fairy-floss)

;; For keybinding definitions
(use-package general)
(use-package hydra)

;; Projectile
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Repositories")
    (setq projectile-project-search-path '("~/Repositories")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; Magit
(use-package magit
  :commands
  (magit-status magit-get-current-branch))

;; TODO initial setup
(use-package forge)

;; Orgmode

(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(defun emimacs/org-font-config ()
  (dolist (face '((org-level-1 . 1.5)
                  (org-level-2 . 1.25)
                  (org-level-3 . 1.15)
                  (org-level-4 . 1.10)
                  (org-level-5 . 1.05)
                  (org-level-6 . 1.05)
                  (org-level-7 . 1.05)
                  (org-level-8 . 1.05)))
    (set-face-attribute (car face) nil :font emimacs/default-variable-font :weight 'regular :height (cdr face)))

  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))

(defun emimacs/org-mode-setup ()
  (org-indent-mode 1)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . emimacs/org-mode-setup)
  :config
  (emimacs/org-font-config)
  (setq org-agenda-start-with-log-mode t)
  (require 'org-habit)
  
  :custom
  (org-ellipsis " ")
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-agenda-files '("~/Orgfiles" "~/Orgfiles/Projects" "~/Orgfiles/Literature"))
  (org-todo-keywords
    '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")))) 

(use-package org-bullets
 :after org
 :hook (org-mode . org-bullets-mode)
 :custom
 (org-bullets-bullet-list '("◉" "●" "⋄")))

(defun emimacs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . emimacs/org-mode-visual-fill))

;; Scheme
(use-package geiser-guile :ensure t)

;; org-babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (scheme . t)))

(setq org-confirm-babel-evaluate nil)

;; Structure Templates
(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("sc" . "src scheme"))

;; Auto-Tangle Config File
(defun emimacs/org-babel-auto-tangle ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/Repositories/emimacs/emimacs.org"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))
  
(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'emimacs/org-babel-auto-tangle)))

;; Parinfer
(use-package parinfer-rust-mode
  :hook emacs-lisp-mode
  :init
  (setq parinfer-rust-auto-download t))
    
