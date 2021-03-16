(setq user-full-name "Devin Sackett")
(setq user-mail-address "dsac@hotmail.com")

(scroll-bar-mode -1)

(tool-bar-mode -1)

(tooltip-mode -1)

(menu-bar-mode -1)

(setq inhibit-startup-message t
      initial-scratch-message ""
      initial-major-mode 'fundamental-mode)

(setq visible-bell 1)
(setq ring-bell-function 'ignore)

(defun yiglas-display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'yiglas-display-startup-time)

;; (defvar bootstrap-version)
;; (let ((bootstrap-file
;;        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
;;       (bootstrap-version 5))
;;   (unless (file-exists-p bootstrap-file)
;;     (with-current-buffer
;;         (url-retrieve-synchronously
;;          "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
;;          'silent 'inhibit-cookies)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))
;;   (load bootstrap-file nil 'nomessage))

;; (setq straight-use-package-by-default t)

;; (straight-use-package 'use-package)

;; (require 'use-package)
;; (setq use-package-always-ensure t)
;; (setq use-package-verbose t)

;; (use-package git) ;; ensure we can install from git sources

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

(defmacro :func (&rest body)
  (if (< (length body) 1)
      `(lambda () ,@body)
    (pcase (car body)
      ;; command symbol
      ((and v (pred commandp))
       `(lambda () (call-interactively (quote ,v))))
      ;; function symbol
      ((and v (pred symbolp))
       `(lambda () (,v)))
      ;; quoted command symbol
      ((and v (pred consp) (guard (eq 'quote (car v))) (pred commandp (cadr v)))
       `(lambda () (call-interactively ,v)))
      ;; quoted function symbol
      ((and v (pred consp) (guard (eq 'quote (car v))))
       `(lambda () (,(cadr v))))
      ;; body format
      (_ `(lambda () ,@body)))))

(defmacro :command (&rest body)
  (if (< (length body) 1)
      `(lambda () (interactive) ,@body)
    (pcase (car body)
      ;; command symbol
      ((and v (pred commandp))
       `(lambda () (interactive) (call-interactively (quote ,v))))
      ;; function symbol
      ((and v (pred symbolp))
       `(lambda () (interactive) (,v)))
      ;; quoted command symbol
      ((and v (pred consp) (guard (eq 'quote (car v))) (pred commandp (cadr v)))
       `(lambda () (interactive) (call-interactively ,v)))
      ;; quoted function symbol
      ((and v (pred consp) (guard (eq 'quote (car v))))
       `(lambda () (interactive) (,(cadr v))))
      ;; body forms
      (_ `(lambda () (interactive) ,@body)))))

;; (defmacro :after (package &rest body)
;;   (declare (indent defun))
;;   `(with-eval-after-load ',package ,@body))

(defmacro :after (package &rest body)
  (declare (indent defun) (debug t))
  (if (symbolp package)
      (list (if (or (not (bound-and-true-p byte-compile-current-file))
                    (require package nil 'noerror))
                #'progn
              #'with-no-warnings)
            ;; we intentially avoid `with-eval-after-load' to prevent eager
            ;; macro expansion from pulling (or failing to pull) in autoload
            ;; macros/packages
            `(eval-after-load ',package ',(macroexp-progn body)))
    (let ((p (car package)))
      (cond ((not (keywordp p))
             `(:after (:and ,@package) ,@body))
            ((memq p '(:or :any))
             (macroexp-progn
              (cl-loop for next in (cdr package)
                       collect `(:after ,next ,@body))))
            ((memq p '(:and :all))
             (dolist (next (cdr package))
               (setq body `((:after ,next ,@body))))
             (car body))))))

(defmacro :hook (hook-name &rest body)
  (declare (indent defun))
  (let* ((hook-name (format "%s-hook" (symbol-name hook-name)))
         (hook-sym (intern hook-name))
         (first (car body))
         (local (eq :local first))
         (body (if local (cdr body) body))
         (first (car body))
         (body (if (consp first)
                   (if (eq (car first) 'quote)
                       first
                     `(lambda () ,@body))
                 `'.first)))
    `(add-hook ',hook-sym ,body nil ,local)))

(defmacro :push (sym &rest body)
  (declare (indent defun))
  (if (consp body)
      `(setq ,sym (-snoc ,sym ,@body))
    `(add-to-list ,sym ,body)))

(defmacro :bind (key &rest body)
  (declare (indent defun))
  (pcase key
    ;; kbd string resolving symbol
    ((and k (pred symbolp) (pred boundp) (guard (stringp (eval key))))
     `(global-set-key (kbd ,(eval key)) ,(eval `(:command ,@body))))
    ;; partial mode symbol
    ((pred symbolp)
     (let ((mode (intern (format "%s-map" key)))
           (key (eval (car body)))
           (body (eval `(:command ,@(cdr body)))))
       `(define-key ,mode (kbd ,key) ,body)))
    ;; global binding
    (_ `(global-set-key (kbd ,key) ,(eval `(:command ,@body))))))

(:bind "<escape>" keyboard-escape-quit)

(:bind "C-x C-b" buffer-menu)

(defun yiglas-make-frame ()
  (interactive)
  (select-frame (make-frame))
  (switch-to-buffer "*scratch*"))

(:bind "M-n" yiglas-make-frame)

(:bind "C-c <left>" windmove-left)
(:bind "C-c <right>" windmove-right)
(:bind "C-c <up>" windmove-up)
(:bind "C-c <down>" windmove-down)

(:bind "M-<return>" toggle-frame-maximized)

(:bind "C-x k" kill-current-buffer)

(defun yiglas-comment-line ()
  (interactive)
  (save-excursion (comment-line 1)))

(:bind "s-/" yiglas-comment-line)

(defun yiglas-open-settings ()
  (interactive)
  (find-file "~/.emacs.d/init.org"))

(:bind "s-," yiglas-open-settings)

(set-face-attribute 'default nil :font "JetBrains Mono")

(column-number-mode)

(set-fringe-mode 10)

(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                debugger-mode-hook
                compilation-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode -1))))

(use-package doom-themes
  :init (load-theme 'doom-nord t)
  :config
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config))

(use-package all-the-icons)

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

(defvar yiglas-frame-transparency '(90 . 90))

;; (set-frame-parameter (selected-frame) 'alpha yiglas-frame-transparency)
;; (:push default-frame-alist '(alpha . ,yiglas-frame-transparency))

(setq ad-redefinition-action 'accept)

(use-package no-littering)

(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(show-paren-mode t)

(use-package prettier-js
  :hook (typescript-mode . prettier-js-mode))

(use-package format-all
  :hook (prog-mode . format-all-mode))

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.5))

(use-package company
  :hook (prog-mode . company-mode)
  :bind
  (:map company-active-map
        ("<tab>" . company-complete-selection))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(:after company
  (:bind "s-." company-search-candidates))

(use-package company-box
  :hook (company-mode . company-box-mode))

;; (use-package smartparens
;;   :init (smartparens-global-mode)
;;   :defer 1
;;   :config
;;   (require 'smartparens-config))

(electric-pair-mode 1)

(use-package dimmer
  :defer 1
  :config
  (dimmer-configure-which-key)
  (dimmer-mode t))

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

(fset 'yes-or-no-p 'y-or-n-p)

(use-package persistent-soft)

(defvar yiglas-cache-location "yiglas-emacs-cache")

(defmacro :cache-set (n v)
  (declare (indent defun))
  `(persistent-soft-store ,n ,v "yiglas-data-store"))

(defmacro :cache-get (n)
  (declare (indent defun))
  `(persistent-soft-fetch ,n "yiglas-data-store"))

(defun yiglas-set-frame-dimensions ()
  (interactive)
  (when-let (dims (:cache-get 'last-frame-size))
    (cl-destructuring-bind ((a-left . a-top) a-width a-height a-fullscreen) dims
      (set-frame-position (selected-frame) a-left a-top)
      (set-frame-size (selected-frame) a-width a-height))))

(add-hook 'emacs-startup-hook #'yiglas-set-frame-dimensions)

(defun yiglas-save-frame-dimensions ()
  (:cache-set 'last-frame-size
    (list (frame-position)
          (frame-width)
          (frame-height)
          (frame-parameter nil 'fullscreen))))

(add-hook 'kill-emacs-hook #'yiglas-save-frame-dimensions)

(:hook prog-mode (toggle-truncate-lines 1))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :bind
  (("C-h s" . #'helpful-symbol)
   ("C-h c" . #'helpful-command)
   ("C-h f" . #'helpful-function)
   ("C-h v" . #'helpful-variable)
   ("C-h k" . #'helpful-key)
   ("C-h m" . #'helpful-mode)
   ("C-h C-h" . #'helpful-at-point)))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  (setq projectile-project-root-files-bottom-up
        (append '(".projectile" ; projectile's root marker
                  ".git" ; git VCS root dir
                  "README.md"
                  ))
        projectile-project-root-files '()
        projectile-project-root-files-top-down-recurring '("Makefile"))
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (setq projectile-globally-ignored-files '(".DS_Store" "TAGS")
        projectile-auto-discover nil
        projectile-switch-project-action #'projectile-dired)
  (when (file-directory-p "~/code")
    (setq projectile-project-search-path `("~/code"))))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

(use-package ivy
  :diminish
  :bind
  (("s-f" . swiper)
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
  :defer 1
  :init  (ivy-rich-mode t)
  :config
  (plist-put ivy-rich-display-transformers-list
             'counsel-M-x
             '(:columns ((counsel-M-x-transformer (:width 60))
                         (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))))

(use-package counsel
  :defer 1
  :bind
  (("C-M-j" . 'counsel-switch-buffer)
   :map minibuffer-local-map
   ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-functiono #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode t)
  (setq counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)"))

(use-package persp-mode
  :commands persp-switch-to-buffer
  :config
  (setq persp-autokill-buffer-on-remove 'kill-weak
        persp-reset-windows-on-nil-window-conf nil
        persp-nil-hidden t
        persp-auto-save-fname "autosave"
        persp-switch-to-added-buffer nil
        persp-kill-foreign-buffer-behaviour 'kill
        persp-remove-buffers-from-nil-persp-behaviour nil
        persp-auto-resume-time -1
        persp-auto-save-opt (if noninteractive 0 1)))

(use-package treemacs
  :bind ("C-x t" . treemacs)
  :init
  (setq treemacs-sorting 'alphabetic-case-insensitive-asc
        treemacs-follow-mode t
        treemacs-follow-after-init t
        treemacs-is-never-other-window nil)
  :config
  (global-set-key (kbd "C-x t") 'treemacs)
  (treemacs-follow-mode -1))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package treemacs-persp
  :after (treemacs persp-mode)
  :config
  (treemacs-set-scope-type 'Perspectives))

(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package forge
  :after magit)

(use-package org
  :commands (org-capture org-agenda)
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

  (custom-theme-set-faces
   'user
   '(org-block ((t (:inherit fixed-pitch))))
   '(org-code ((t (:inherit (shadow fixed-pitch)))))
   '(org-document-info ((t (:foreground "dark orange"))))
   '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
   '(org-link ((t (:foreground "royal blue" :underline t))))
   '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-property-value ((t (:inherit fixed-pitch))) t)
   '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
   '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
   '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))))

(defun yiglas-org-mode ()
  (org-indent-mode)
  (variable-pitch-mode t)
  (visual-line-mode t))

(:hook org-mode 'yiglas-org-mode)

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-remove-leading-stars t)
  (org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●")))

(:after org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)))
  (push '("conf-unix" . conf-unix) org-src-lang-modes))

(:after org
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp")))

(defun yiglas-org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
		      (expand-file-name user-emacs-directory))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'yiglas-org-babel-tangle-config)))

(use-package flycheck
  :init (global-flycheck-mode)
  :defer 1)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :bind
  (("<f12>" . 'lsp-find-definition)
   ("C-<f12>" . 'lsp-find-implementation)
   ("C--" . 'pop-global-mark)
   :map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t)
  (lsp-headerline-breadcrumb-mode -1))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package eglot
  :commands (eglot eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '(csharp-mode . ("/Users/dsac/.emacs.d/var/lsp/server/omnisharp-roslyn/v1.37.7/run")))
  (add-to-list 'eglot-server-programs '(typescript-mode . ("deno" "lsp"))))

(use-package dap-mode
  :custom
  (lsp-enable-dap-auto-configure nil)
  :commands dap-debug
  :config
  (dap-ui-mode t)
  (require 'dap-node))

(use-package tree-sitter
  :defer 1)

(use-package tree-sitter-langs
  :defer 1)

(use-package typescript-mode
  :mode "\\.ts\\'"
  :mode "\\.tsx\\'"
  :config
  (:hook typescript-mode 'lsp-deferred)
  (setq typescript-indent-level 2))

(use-package csharp-mode
  :mode "\\.cs\\'"
  :mode "\\.csx\\'"
  :config
  (:hook csharp-mode 'lsp-deferred)
  (add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-tree-sitter-mode))
  (add-to-list 'auto-mode-alist '("\\.csx\\'" . csharp-tree-sitter-mode)))

(defun yiglas-csharp-lsp-mode ()
  ;; todo push this to .dir-locals.el???
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]tools\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\].docz\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]TestOutput\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]Terraform\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]QAAutomation\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]bin\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]certs\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]LoadTests\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]Artifacts\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]E2E\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]Database\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]Workspace\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\].azuredevops\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\].docker\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\].log\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\].vs\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]Dependencies\\'"))

(:hook lsp-mode 'yiglas-csharp-lsp-mode)

(use-package sharper
  :bind
  ("C-x d" . sharper-main-transient))

(use-package dotenv-mode
  :mode "\\.env\\'"
  :mode "\\.env.example\\'")

(use-package eshell-toggle
  :bind ("C-M-'" . eshell-toggle)
  :custom
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-use-projectile-root t)
  (eshell-toggle-run-command nil))

(use-package git-gutter
  :diminish
  :hook ((text-mode prog-mode) . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 2))

(use-package persistent-scratch
  :commands persistent-scratch-setup-default
  :hook (after-init . persistent-scratch-setup-default))

(setq ispell-program-name "aspell")
(setq ispell-extra-args '("--camel-case" "--sug-mode=ultra" "--lang=en_US" "--run-together" "--run-together-limit=16"))

(use-package wucuo
  :hook ((text-mode prog-mode) . wucuo-start))
