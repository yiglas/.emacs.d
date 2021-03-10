;; +title: Devin Sackett's Emacs configuration

;; TODO treemacs as buffer to windmove to
;; TODO treemacs only shows current projectile project
;; TODO spelling
;; TODO add .sln to projectile / .cake
;; TODO change this file to be tangleable
;; TODO remove un wanted items in mode-line
;; TODO remove border from title bar (make it look the same)
;; TODO add vc gutter and treemacs
;; TODO switch 'C-s' to be i-search, then 'C-s' to move to next search and 'C-r' to move to previous search
;; TODO switch swiper to be 'M-s'
;; TODO yasnippet

(setq user-emacs-directory "~/code/playing/.emacs.d.new/")

(setq user-full-name "Devin Sackett")
(setq user-mail-address "dsac@hotmail.com")

;; ---------------------------------------------------------------------
;; * Startup performance
(defun yiglas-display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'yiglas-display-startup-time)

;; ** Helper functions
(defun yiglas-reload-init-file ()
  "Reload init.el"
  (interactive)
  (load-file (expand-file-name (concat user-emacs-directory "init.el"))))

;; ---------------------------------------------------------------------
;; ** Package system setup
;; [[https://github.com/jwiegley/use-package][use-package]]
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
(setq use-package-verbose t)

;; ---------------------------------------------------------------------
;; * Keep folders clean
;; [[https://github.com/emacscollective/no-littering/blob/master/no-littering.el][no-littering]] package keeps the Emacs
;; configuration folder clean!
(use-package no-littering)

;; no-littering doesn't set this by default. Setting auto saved files
;; to the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;; ---------------------------------------------------------------------
;; * Basic UI configuration
(setq inhibit-startup-message t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)
(show-paren-mode t)

(set-face-attribute 'default nil :font "JetBrains Mono")

(column-number-mode)
(line-number-mode)

(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)
(setq visible-bell 1)
(setq ring-bell-function 'ignore)

(defvar yiglas-frame-transparency '(90 . 90))

;; (set-frame-parameter (selected-frame)  'alpha yiglas-frame-transparency)
;; (add-to-list 'default-frame-alist `(alpha . ,yiglas-frame-transparency))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; disable line number for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		treemacs-mode-hook
		debugger-mode-hook
		compilation-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode -1))))

(defun yiglas-make-frame ()
  "."
  (interactive)
  (select-frame (make-frame))
  (switch-to-buffer "*scratch*"))

(defun yiglas-delete-frame-or-kill-emacs ()
  "Delete the current frame or completely kill Emacs if there is only one frame."
  (interactive)
  (if (> (length (frame-list)) 1)
      (delete-frame)
    (save-buffers-kill-emacs)))

(defun yiglas-comment-line ()
  (interactive)
  (save-excursion (comment-line 1)))

(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-set-key (kbd "M-n") 'yiglas-make-frame)
(global-set-key (kbd "C-x C-c") 'yiglas-delete-frame-or-kill-emacs)
(global-set-key (kbd "C-c <left>") 'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <down>") 'windmove-down)
(global-set-key (kbd "M-<return>") 'toggle-frame-maximized)
(global-set-key (kbd "C-x k") 'kill-current-buffer)
(global-set-key (kbd "s-/") 'yiglas-comment-line)

;; simplify yes/no prompts
(fset 'yes-or-no-p 'y-or-n-p)

(add-hook 'prog-mode-hook (lambda () (toggle-truncate-lines 1)))

;; ---------------------------------------------------------------------
;; * Command log mode
;; [[https://github.com/lewang/command-log-mode][command-log-mode]] helps me see what commands are running and when
(use-package command-log-mode
  :commands command-log-mode)

;; ---------------------------------------------------------------------
;; * Themeing
(use-package doom-themes
  :init (load-theme 'doom-nord t)
  :config
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config))

;; ---------------------------------------------------------------------
;; * Modeline
;; [[https://github.com/seagle0128/doom-modeline][doom-modeline]] is a very attractive and rich (yet very minimal) mode line configuration.
(use-package all-the-icons)

;; (use-package doom-modeline
;;   :init (doom-modeline-mode t)
;;   :custom
;;   (doom-modeline-height 15)
;; 	   (doom-modeline-buffer-file-name-style 'buffer-name)
;; 	   (doom-modeline-major-mode-icon nil)
;; 	   (doom-modeline-continuous-word-count-modes nil)
;; 	   (doom-modeline-buffer-encoding nil))

(use-package doom-modeline
  :init (doom-modeline-mode t)
  :custom-face
  (mode-line ((t (:height 0.85))))
  (mode-line-inactive ((t (:height 0.85))))
  :custom
  (doom-modeline-height 15)
  (doom-modeline-bar-width 6)
  (doom-modeline-lsp t)
  (doom-modeline-github nil)
  (doom-modeline-persp-name nil)
  (doom-modeline-buffer-file-name-style 'truncate-except-project)
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-buffer-encoding nil))

;; ---------------------------------------------------------------------
;; * Which key
;; [[https://github.com/justbur/emacs-which-key][which-key]] is a useful UI panel that appears when you start pressing any key binding in Emacs.
(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.5))

;; ---------------------------------------------------------------------
;; * Ivy and Counsel
;; [[https://oremacs.com/swiper/][Ivy]] is an excellent completion framework for Emacs.
;; [[https://github.com/Yevgnen/ivy-rich][ivy-rich]] adds extra columns to a few of the Counsel commands to provide more information about each item.
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
  :config
  (ivy-mode t))

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode t)
  :config
  (plist-put ivy-rich-display-transformers-list
	     'counsel-M-x
	     '(:columns ((counsel-M-x-transformer (:width 60))
			 (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))))

(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-functiono #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode t)
  (setq counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)"))

;; ---------------------------------------------------------------------
;; * Helpful
;; [[https://github.com/Wilfred/helpful][Helpful]] adds a lot of very helpful (get it?) information to Emacs' =describe-= command buffers.
(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; ---------------------------------------------------------------------
;; * Text Scalling
;; [[https://github.com/abo-abo/hydra][Hydra]] to design a transient key binding for quickly adjusting the scale of the text on screen
(use-package hydra
  :defer t)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

;; ---------------------------------------------------------------------
;; * Org
;; [[https://orgmode.org/][Org Mode]] is one of the hallmark features of Emacs.
(defun yiglas-org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))))))

(defun yiglas-org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode t)
  (visual-line-mode t))

(use-package org
  :pin org
  :commands (org-capture org-agenda)
  :hook (org-mode . yiglas-org-mode-setup)
  :config
  (setq org-ellipsis " ▾")

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  (setq org-todo-keywords
    '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
      (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  (setq org-refile-targets
    '(("Archive.org" :maxlevel . 1)
      ("Tasks.org" :maxlevel . 1)))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-tag-alist
    '((:startgroup)
       ; Put mutually exclusive tags here
       (:endgroup)
       ("@errand" . ?E)
       ("@home" . ?H)
       ("@work" . ?W)
       ("agenda" . ?a)
       ("planning" . ?p)
       ("publish" . ?P)
       ("batch" . ?b)
       ("note" . ?n)
       ("idea" . ?i)))

  ;; Configure custom agenda views
  (setq org-agenda-custom-commands
	'(("d" "Dashboard"
	   ((agenda "" ((org-deadline-warning-days 7)))
	    (todo "NEXT"
		  ((org-agenda-overriding-header "Next Tasks")))
	    (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

	  ("n" "Next Tasks"
	   ((todo "NEXT"
		  ((org-agenda-overriding-header "Next Tasks")))))

	  ("W" "Work Tasks" tags-todo "+work-email")
          ;; Low-effort next actions
	  ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
	   ((org-agenda-overriding-header "Low Effort Tasks")
            (org-agenda-max-todos 20)
            (org-agenda-files org-agenda-files)))

	  ("w" "Workflow Status"
	   ((todo "WAIT"
		  ((org-agenda-overriding-header "Waiting on External")
		   (org-agenda-files org-agenda-files)))
            (todo "REVIEW"
		  ((org-agenda-overriding-header "In Review")
		   (org-agenda-files org-agenda-files)))
	    (todo "PLAN"
		  ((org-agenda-overriding-header "In Planning")
		   (org-agenda-todo-list-sublevels nil)
		   (org-agenda-files org-agenda-files)))
            (todo "BACKLOG"
		  ((org-agenda-overriding-header "Project Backlog")
		   (org-agenda-todo-list-sublevels nil)
		   (org-agenda-files org-agenda-files)))
            (todo "READY"
		  ((org-agenda-overriding-header "Ready for Work")
		   (org-agenda-files org-agenda-files)))
            (todo "ACTIVE"
		  ((org-agenda-overriding-header "Active Projects")
		   (org-agenda-files org-agenda-files)))
            (todo "COMPLETED"
		  ((org-agenda-overriding-header "Completed Projects")
		   (org-agenda-files org-agenda-files)))
            (todo "CANC"
		  ((org-agenda-overriding-header "Cancelled Projects")
		   (org-agenda-files org-agenda-files)))))))

  (setq org-capture-templates
	`(("t" "Tasks / Projects")
          ("tt" "Task" entry (file+olp "~/Projects/Code/emacs-from-scratch/OrgFiles/Tasks.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

          ("j" "Journal Entries")
          ("jj" "Journal" entry
           (file+olp+datetree "~/Projects/Code/emacs-from-scratch/OrgFiles/Journal.org")
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
           :clock-in :clock-resume
           :empty-lines 1)
          ("jm" "Meeting" entry
           (file+olp+datetree "~/Projects/Code/emacs-from-scratch/OrgFiles/Journal.org")
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)

          ("w" "Workflows")
          ("we" "Checking Email" entry (file+olp+datetree "~/Projects/Code/emacs-from-scratch/OrgFiles/Journal.org")
           "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

          ("m" "Metrics Capture")
          ("mw" "Weight" table-line (file+headline "~/Projects/Code/emacs-from-scratch/OrgFiles/Metrics.org" "Weight")
           "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))

  (define-key global-map (kbd "C-c j")
    (lambda () (interactive) (org-capture nil "jj")))

  (yiglas-org-font-setup))

;; ** Nicer header bullets
;; [[https://github.com/sabof/org-bullets][org-bullets]] replaces the heading stars in =org-mode= buffers with nicer looking characters that you can control.
(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; ** Configure babel languages
;; To execute or export code in =org-mode= code blocks, you'll need to set up =org-babel-load-languages= for each language you'd like to use.  [[https://orgmode.org/worg/org-contrib/babel/languages.html][This page]] documents all of the languages that you can use with =org-babel=.
(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)))
  (push '("conf-unix" . conf-unix) org-src-lang-modes))

;; ** Structure templates
;; Org Mode's [[https://orgmode.org/manual/Structure-Templates.html][structure templates]] feature enables you to quickly insert code blocks into your Org files in combination with =org-tempo= by typing =<= followed by the template name like =el= or =py= and then press =TAB=.  For example, to insert an empty =emacs-lisp= block below, you can type =<el= and press =TAB= to expand into such a block.
(with-eval-after-load 'org
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-list")))

;; ** Auto-tangle configuration files
(defun yiglas-org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
		      (expand-file-name user-emacs-directory))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'yiglas-org-babel-tangle-config)))

;; ---------------------------------------------------------------------
;; * Development

;; ** IDE features

;; *** treemacs
;; [[https://github.com/Alexander-Miller/treemacs][treemacs]] enables a tree layout file explorer for Emacs
(use-package treemacs
  :bind ("C-x t" . treemacs)
  :init
  (setq treemacs-is-never-other-window t
	treemacs-sorting 'alphabetic-case-insensitive-asc)
  :config
  (treemacs-follow-mode -1))

(defun yiglas-project-root (&optional dir)
  (let ((projectile-project-root (unless dir projectile-project-root))
	projectile-require-project-root
	(projectile-project-root dir))))

(defun yiglas-project-p (&optional dir)
  (and (yiglas-project-root dir) t))

(defun yiglas-treemacs-toggle ()
  (interactive
   (require 'treemacs)
   (pcase (treemacs-current-visibility)
     (`visible (delete-window (treemacs-get-local-window)))
     (_ (if (yiglas-project-p)
	    (treemacs-add-and-display-current-project)
	  (treemacs))))))



;; **** treemacs-projectile
(use-package treemacs-projectile
  :after (treemacs projectile))

;; **** treemacs-magit
(use-package treemacs-magit
  :after (treemacs magit))

;; **** treemacs perspectives
(use-package treemacs-persp
  :after treemacs
  :config (treemacs-set-scope-type 'Perspectives))

;; *** Persp


;; *** lsp-mode
;; [[https://emacs-lsp.github.io/lsp-mode/][lsp-mode]] enables IDE-like functionality for many different programming languages.
(defun yiglas-lsp-mode-setup ()
  (setq lsp-header-line-breadcrumbs-segments '(path-up-to-project file symbols))
  (local-set-key (kbd "C--") 'pop-global-mark)
  (local-set-key (kbd "<f12>") 'lsp-find-definition)
  (local-set-key (kbd "C-<f12>") 'lsp-find-implementation)
  (lsp-headerline-breadcrumbs-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . yiglas-lsp-mode-setup)
  :bind
  (:map lsp-mode-map
	("<tab>" . company-indent-or-complete-common))
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t)
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]tools\\'"))

;; **** lsp-ui
;; [[https://emacs-lsp.github.io/lsp-ui/][lsp-ui]] is a set of UI enhancements built on top of =lsp-mode= which make Emacs feel even more like an IDE.
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

;; *** lsp-treemacs
;; [[https://github.com/emacs-lsp/lsp-treemacs][lsp-treemacs]] provides nice tree views for different aspects of your code like symbols in a file, references of a symbol, or diagnostic messages (errors and warnings) that are found in your code.

;; Try these commands with =M-x=:

;; - =lsp-treemacs-symbols= - Show a tree view of the symbols in the current file
;; - =lsp-treemacs-references= - Show a tree view for the references of the symbol under the cursor
;; - =lsp-treemacs-error-list= - Show a tree view for the diagnostic messages in the project

;; This package is built on the [[https://github.com/Alexander-Miller/treemacs][treemacs]] package which might be of some interest to you if you like to have a file browser at the left side of your screen in your editor.
(use-package lsp-treemacs
  :after lsp)

;; **** lsp-ivy
;; [[https://github.com/emacs-lsp/lsp-ivy][lsp-ivy]] integrates Ivy with =lsp-mode= to make it easy to search for things by name in your code.  When you run these commands, a prompt will appear in the minibuffer allowing you to type part of the name of a symbol in your code.  Results will be populated in the minibuffer so that you can find what you're looking for and jump to that location in the code upon selecting the result.

;; Try these commands with =M-x=:

;; - =lsp-ivy-workspace-symbol= - Search for a symbol name in the current project workspace
;; - =lsp-ivy-global-workspace-symbol= - Search for a symbol name in all active project workspaces
(use-package lsp-ivy
  :after lsp)

;; ** Debugging with dap-mode
;; [[https://emacs-lsp.github.io/dap-mode/][dap-mode]] is an excellent package for bringing rich debugging capabilities to Emacs via the [[https://microsoft.github.io/debug-adapter-protocol/][Debug Adapter Protocol]].  You should check out the [[https://emacs-lsp.github.io/dap-mode/page/configuration/][configuration docs]] to learn how to configure the debugger for your language.  Also make sure to check out the documentation for the debug adapter to see what configuration parameters are available to use for your debug templates!
(use-package dap-mode
  :custom
  (lsp-enable-dap-auto-configure nil)
  :commands dap-debug
  :config
  (dap-ui-mode t)
  (require 'dap-node))

;; ** Prettier-js
;; [[https://github.com/prettier/prettier-emacs][prettier]] formats the current buffer.
(use-package prettier-js
  :hook (typescript-mode . prettier-js-mode))

;; ** Format-all
;; [[https://github.com/lassik/emacs-format-all-the-code][format-all]] formats the current buffer
(use-package format-all
  :hook (prog-mode . format-all-mode))

;; ** Rainbow delimiters
;; [[https://github.com/Fanael/rainbow-delimiters][rainbow-delimiters]] is useful in programming modes because it colorizes nested parentheses and brackets according to their nesting depth.  This makes it a lot easier to visually match parentheses in Emacs Lisp code without having to count them yourself.
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; ** Company
;; [[http://company-mode.github.io/][Company Mode]] provides a nicer in-buffer completion interface than =completion-at-point= which is more reminiscent of what you would expect from an IDE.
(use-package company
  :hook (prog-mode . company-mode)
  :bind
  (:map company-active-map
	("<tab>" . company-complete-selection))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(eval-after-load 'company-mode
  '(lambda () (local-set-key (kbd "s-." 'company-search-candidates))))

;; *** Company-box
;; [[https://github.com/sebastiencs/company-box][company-box]] to further enhance the look of the completions with icons and better overall presentation.
(use-package company-box
  :hook (company-mode . company-box-mode))

;; ** Projectile
;; [[https://projectile.mx/][Projectile]] is a project management library for Emacs which makes it a lot easier to navigate around code projects for various languages.
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/code")
    (setq projectile-project-search-path `("~/code")))
  (setq projectile-switch-project-action #'projectile-dired))

;; *** Counsel projectile
(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

;; ** Magit
;; [[https://magit.vc/][Magit]]
(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; ** Forge
(use-package forge
  :after magit)

;; ** Smartparens
(use-package smartparens
  :init
  (smartparens-global-mode)
  :config
  (require 'smartparens-config))

;; ** Languages

;; *** Typescript
;; This is a basic configuration for the TypeScript language so that =.ts= files activate =typescript-mode= when opened.  We're also adding a hook to =typescript-mode-hook= to call =lsp-deferred= so that we activate =lsp-mode= to get LSP features every time we edit TypeScript code.
(use-package typescript-mode
  :mode "\\.ts\\'"
  :mode "\\.tsx\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

;; *Important note!*  For =lsp-mode= to work with TypeScript (and JavaScript) you will need to install a language server on your machine.  If you have Node.js installed, the easiest way to do that is by running the following command:
;; npm install -g typescript-language-server typescript
;; This will install the [[https://github.com/theia-ide/typescript-language-server][typescript-language-server]] and the TypeScript compiler package

;; *** Csharp
(use-package csharp-mode
  :mode "\\.cs\\'"
  :mode "\\.csx\\'"
  :hook ((csharp-mode . lsp)
	 (csharp-mode . csharp-tree-sitter-mode)))

(use-package tree-sitter
  :after csharp-mode-hook)

(use-package tree-sitter-langs)

(use-package sharper
  :bind ("C-x d" . sharper-main-transient))

;; ** Extra


;; *** Eshell-toggle
(use-package eshell-toggle
  :bind ("C-M-'" . eshell-toggle)
  :custom
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-use-projectile-root t)
  (eshell-toggle-run-command nil))

;; *** Dimmer
(use-package dimmer
  :config
  (dimmer-configure-which-key)
  (dimmer-mode t))

;; ** Hl-todo
(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
	hl-todo-keyword-faces
	`(;; For things that need to be done, just not today.
          ("TODO" warning bold)
          ;; For problems that will become bigger problems later if not
          ;; fixed ASAP.
          ("FIXME" error bold)
          ;; For tidbits that are unconventional and not intended uses of the
          ;; constituent parts, and may break in a future update.
          ("HACK" font-lock-constant-face bold)
          ;; For things that were done hastily and/or hasn't been thoroughly
          ;; tested. It may not even be necessary!
          ("REVIEW" font-lock-keyword-face bold)
          ;; For especially important gotchas with a given implementation,
          ;; directed at another user other than the author.
          ("NOTE" success bold)
          ;; For things that just gotta go and will soon be gone.
          ("DEPRECATED" font-lock-doc-face bold)
          ;; For a known bug that needs a workaround
          ("BUG" error bold)
          ;; For warning about a problematic or misguiding code
          ("XXX" font-lock-constant-face bold))))

;; ** Spelling
(setq ispell-program-name "aspell")
(setq ispell-extra-args '("--camel-case" "--sug-mode=ultra" "--lang=en_US" "--run-together" "--run-together-limit=16"))

(use-package wucuo
  :hook ((text-mode prog-mode) . wucuo-start))

;; ** Git gutter
(use-package git-gutter
  ;; :straight git-gutter-fringe
  :diminish
  :hook ((text-mode prog-mode) . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 2))
