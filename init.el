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

;; Set Font
(set-face-attribute 'default nil :font "Fira Code NF" :height 140)

;; Set Theme 
(load-theme 'wombat)

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
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil))

;; Doom Modeline for now
(use-package doom-modeline
  :ensure t
  :custom
  (doom-modeline-height 30)
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
