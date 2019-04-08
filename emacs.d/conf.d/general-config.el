;;; general-config --- Summary
;;; Commentary:
;;; use-package configurations for packages I always want to load


;;; Code:

;;; Writing
;;;---------------------------------------------------------

(when (memq 'dictcc packages-to-configure)
  (use-package dictcc
    :bind (("s-t" . dictcc))
    :config (setq dictcc-source-lang "en"
                  dictcc-destination-lang "sv")))


;;; Emacs behaviour
;;;---------------------------------------------------------
(when (memq 'ivy packages-to-configure)
  (use-package ivy
    :init (ivy-mode 1)
    (setq ivy-use-virtual-buffers t
          enable-recursive-minibuffers t
          ivy-use-selectable-prompt t)
    :bind (:map ivy-minibuffer-map
                (("C-j" . 'ivy-immediate-done)
                 ("RET" . 'ivy-alt-done)))
    :diminish ivy-mode))

(when (memq 'swiper packages-to-configure)
  (use-package swiper
    :bind ("C-s" . 'swiper)))

(when (memq 'counsel packages-to-configure)
  (use-package counsel
    :init (counsel-mode 1)
    :bind (("<f1> l" . counsel-find-library)
           ("<f2> i" . counsel-info-lookup-symbol)
           ("<f2> u" . counsel-unicode-char)
           ("C-c g" . 'counsel-git)
           ("C-c j" . 'counsel-git-grep)
           ("C-c k" . 'counsel-ag)
           ("C-c l" . 'counsel-locate)
           :map ivy-minibuffer-map
           ("C-r" . 'counsel-minibuffer-history))
    :diminish counsel-mode))
(when (memq 'ivy-prescient packages-to-configure)
  (use-package ivy-prescient
    :init (ivy-prescient-mode 1)
    :bind (("<f6>" . ivy-resume))
    :diminish ivy-prescient-mode)
  (prescient-persist-mode 1))

(when (memq 'company-prescient packages-to-configure)
  (use-package company-prescient
    :init (company-prescient-mode 1)
    :diminish company-prescient-mode))

(when (memq 'ivy-rich packages-to-configure)
  (use-package ivy-rich
    :config (setq ivy-rich-path-style 'abbrev)
    :init (ivy-rich-mode 1)))

(when (memq 'all-the-icons-ivy packages-to-configure)
  (use-package all-the-icons-ivy
    :config (all-the-icons-ivy-setup)))

(when (memq 'ido packages-to-configure)
  (use-package ido
    :init (ido-mode t)
    (setq ido-enable-flex-matching t
          ido-everywhere t
          ido-create-new-buffer 'always
          ido-default-file-method 'selected-window
          ido-default-buffer-method 'selected-window)))

(when (memq 'move-text packages-to-configure)
  (use-package move-text
    :config
    (move-text-default-bindings)))

(when (memq 'smex packages-to-configure)
  (use-package smex
    :bind (("M-x" . smex)
           ("M-X" . smex-major-mode-commands)
           ("C-c C-c M-x" . execute-extended-command))
    :config (setq smex-history-length 20)))

(when (memq 'yafolding packages-to-configure)
  (use-package yafolding
    :hook (prog-mode . yafolding-mode)))

(when (memq 'visual-regexp-steroids packages-to-configure)
  (use-package visual-regexp-steroids
    :bind (("C-c r" . vr/replace)
           ("C-c q" . vr/query-replace)
           ("C-c s" . vr/isearch-forward))))

(when (memq 'paradox packages-to-configure)
  (use-package paradox
    :config (setq paradox-execute-asynchronously nil
		  paradox-lines-per-entry 2)))
;;;---------------------------------------------------------

;;;---------------------------------------------------------

;;; Code editing modes and helper functions
;;;---------------------------------------------------------
;;;;; Major modes
;;;;; ------------------------------------------------------
(when (memq 'dockerfile-mode packages-to-configure)
  (use-package dockerfile-mode))

(when (memq 'toml-mode packages-to-configure)
  (use-package toml-mode
    :diminish toml-mode))

(when (memq 'json-mode packages-to-configure)
  (use-package json-mode
    :hook (prog-mode . highlight-indent-guides-mode)
    :config (setq highlight-indent-guides-method 'character
                  highlight-indent-guides-responsive 'top)))

(when (memq 'markdown-mode packages-to-configure)
  (use-package markdown-mode
    :mode "\\.markdown\\'" "\\.md\\'")
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

(when (memq 'markdown-mode packages-to-configure)
  (autoload 'gfm-mode "markdown-mode"
    "Major mode for editing GitHub Flavored Markdown files" t)
  (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode)))

(when (memq 'graphviz-dot-mode packages-to-configure)
  (use-package graphviz-dot-mode
    :config (setq graphviz-dot-view-command "eog"
                  graphviz-dot-view-edit-command t
                  graphviz-dot-save-before-view t)))

(when (memq 'irony packages-to-configure)
  (use-package irony
    :hook ((c-mode . irony-mode)
           (c++-mode . irony-mode)
           (irony-mode . irony-cdb-autosetup-compile-options))))


;;;;; ------------------------------------------------------
(when (memq 'ggtags packages-to-configure)
  (use-package ggtags
    :config (setq ggtags-mode-line-project-name nil)
    :hook (prog-mode . ggtags-mode)
    :diminish ggtags-mode))

(when (memq 'highlight-indent-guides packages-to-configure)
  (use-package highlight-indent-guides
    :hook (prog-mode . highlight-indent-guides-mode )
    :config (setq highlight-indent-guides-method 'character
                  highlight-indent-guides-responsive 'top)
    :diminish highlight-indent-guides-mode))

(when (memq 'diff-hl packages-to-configure)
  (use-package diff-hl
    :bind (("C-x v g" . diff-hl-diff-goto-hunk)
           ("C-x v r" . diff-hl-revert-hunk)
           ("C-c v p" . diff-hl-previous-hunk)
           ("C-c v n" . diff-hl-next-hunk))
    :demand
    :config
    (diff-hl-flydiff-mode)
    (global-diff-hl-mode)))

(when (memq 'flylisp packages-to-configure)
  (use-package flylisp
    :hook (emacs-lisp-mode . flylisp-mode)))

(when (memq 'call-graph packages-to-configure)
  (use-package call-graph
    :commands call-graph
    :init (setq cg-initial-max-depth 10)))

(use-package git-commit
  :init
  (global-git-commit-mode))

(when (memq 'change-inner packages-to-configure)
  (use-package change-inner
    :bind (("M-i" . change-inner)
           ("M-o" . change-outer))))

(when (memq 'aggressive-indent packages-to-configure)
  (use-package aggressive-indent
    :config
    (add-to-list
     'aggressive-indent-dont-indent-if
     '(and (derived-mode-p 'c++-mode)
           (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                               (thing-at-point 'line)))))
    :diminish))

(when (memq 'rtags packages-to-configure)
  (use-package rtags
    :bind ("M-." . rtags-find-symbol-at-point)
    :config (setq rtags-path "/mnt/storage/src/rtags/bin")))


;; LSPmode and CCLS server
(when (memq 'ccls packages-to-configure)
  (use-package ccls))

(when (memq 'lsp-mode packages-to-configure)
  (use-package lsp-mode
    :init (setq lsp-prefer-flymake nil)
    :hook (((c-mode c++-mode python-mode) .
            (lambda () (require 'ccls) (lsp)))
           (python-mode . lsp))
    :commands lsp))

(when (memq 'lsp-ui packages-to-configure)
  (use-package lsp-ui
    :init (setq lsp-ui-doc-header t
                lsp-ui-doc-include-signature t
                lsp-ui-doc-use-webkit nil
                lsp-ui-sideline-delay 1)
    :commands lsp-ui-mode))

(when (memq 'company-lsp packages-to-configure)
  (use-package company-lsp
    :commands company-lsp))


;; Built-in
(use-package whitespace
  :bind (("C-c T w" . whitespace-mode))
  :hook ((prog-mode . whitespace-mode)
         (text-mode . whitespace-mode)
         (conf-mode . whitespace-mode))
  :config (setq whitespace-line-column nil)
  (setq whitespace-style '(face indentation::space tabs trailing))
  :diminish whitespace-mode)


;;;;; Flycheck linters
;;;;; ------------------------------------------------------
(when (memq 'flycheck packages-to-configure)
  (use-package flycheck
    :config
    :init (progn (global-flycheck-mode)
                 (setq flycheck-emacs-lisp-load-path 'inherit))))

(when (memq 'flycheck-checkbashisms packages-to-configure)
  (use-package flycheck-checkbashisms
    :config
    (flycheck-checkbashisms-setup)))

(when (memq 'flycheck-popup-tip packages-to-configure)
  (use-package flycheck-popup-tip
    :after flycheck
    :hook  (flycheck-mode . flycheck-popup-tip-mode)))

(when (memq 'flycheck packages-to-configure)
  (flycheck-define-checker proselint
    "A linter for prose."
    :command ("proselint" source-inplace)
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ": "
              (id (one-or-more (not (any " "))))
              (message) line-end))
    :modes (text-mode markdown-mode gfm-mode)))

(when (memq 'flycheck packages-to-configure)
  (add-to-list 'flycheck-checkers 'proselint))

(when (memq 'flycheck-flawfinder packages-to-configure)
  (use-package flycheck-flawfinder
    :after flycheck
    :config (flycheck-flawfinder-setup)
    (flycheck-add-next-checker 'c/c++-gcc '(warning . flawfinder))))

(when (memq 'flycheck-ycmd packages-to-configure)
  (use-package flycheck-ycmd
    :after flycheck
    :config
    (flycheck-ycmd-setup)))

(when (memq 'flycheck-rtags packages-to-configure)
  (use-package flycheck-rtags))

(when (memq 'flycheck-irony packages-to-configure)
  (use-package flycheck-irony
    :after flycheck
    :hook (flycheck-mode . flycheck-irony-setup)))
;;;;; ------------------------------------------------------

;;;;; Company code completion and backends
;;;;; ------------------------------------------------------
(when (memq 'company packages-to-configure)
  (use-package company
    :bind (("s-:" . company-complete))
    :init (global-company-mode)
    :diminish))

(when (memq 'company-jedi packages-to-configure)
  (use-package company-jedi
    :after company
    :init (add-to-list 'company-backends 'company-jedi)))

(when (memq 'company-rtags packages-to-configure)
  (use-package company-rtags
    :after company
    :init (add-to-list 'company-backends 'company-rtags)))

(when (memq 'company-c-headers packages-to-configure)
  (use-package company-c-headers
    :after company
    :init (add-to-list 'company-backends 'company-c-headers)))

(when (memq 'company-shell packages-to-configure)
  (use-package company-shell
    :after company
    :init (add-to-list 'company-backends 'company-shell)))

(when (memq 'company-ycmd packages-to-configure)
  (use-package company-shell
    :after company
    :init (company-ycmd-setup)))

(when (memq 'company-quickhelp packages-to-configure)
  (use-package company-quickhelp
    :after company
    :init (company-quickhelp-mode)))

(when (memq 'company-irony packages-to-configure)
  (use-package company-irony
    :after company
    :init (add-to-list 'company-backends 'company-irony)))

(when (memq 'company-restclient packages-to-configure)
  (use-package company-restclient
    :after company
    :init (add-to-list 'company-backends 'company-restclient)))

;;;;; ------------------------------------------------------
;;;---------------------------------------------------------

;;; Visuals

;;; Do not load when starting server. But for some reason it works to
;;; load them after startup.
;;;---------------------------------------------------------
(when (and (memq 'ample-zen-theme packages-to-configure) (not (daemonp)))
  (use-package ample-zen-theme)
  :config
  (load-theme 'ample-zen t))

(when (and (memq 'mode-icons packages-to-configure) (not (daemonp)))
  (use-package mode-icons
    :hook (flycheck-after-syntax-check . mode-icons-reset)
    :init (mode-icons-mode)
    (add-hook 'flycheck-after-syntax-check-hook 'mode-icons-reset)))
;;;---------------------------------------------------------

;;; Productivity
;;;---------------------------------------------------------
(when (memq 'todotxt packages-to-configure)
  (use-package todotxt
    :bind (("C-x t" . todotxt))
    :config (setq todotxt-file "~/.todo/todo.txt")))

(when (memq 'cheatsheet packages-to-configure)
  (use-package cheatsheet
    :bind ("s-c" . cheatsheet-show)
    :init (load "my-cheatsheet" t)))

(when (memq 'projectile packages-to-configure)
  (use-package projectile
    :config
    (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
    :diminish projectile-mode)
  (when (memq 'ivy packages-to-configure)
    (setq projectile-completion-system 'ivy)))

(when (memq 'counsel-projectile packages-to-configure)
  (use-package counsel-projectile
    :init (counsel-projectile-mode 1)))

(when (memq 'magit-todos packages-to-configure)
  :config ((setq magit-todos-ignore-case t)
	   (magit)))

(when (memq 'ripgrep packages-to-configure)
  (use-package ripgrep))

(when (memq 'neotree packages-to-configure)
  (use-package neotree
    :config (setq neo-theme (if (display-graphic-p) 'icons 'arrow))))

(when (memq 'which-key packages-to-configure)
  (use-package which-key))

;;;---------------------------------------------------------


;;; Network
;;;---------------------------------------------------------
(when (memq 'restclient packages-to-configure)
  (use-package restclient))

;;;---------------------------------------------------------

(provide 'general-config)
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; general-config.el ends here
