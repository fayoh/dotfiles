;;; general-config --- Summary
;;; Commentary:
;;; use-package configurations for packages I always want to load


;;; Code:

;;; Writing
;;;---------------------------------------------------------
(use-package dictcc
  :bind (("s-t" . dictcc))
  :config (setq dictcc-source-lang "en"
                dictcc-destination-lang "sv"))
(use-package flyspell
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :config (setq flyspell-issue-message-flag nil)
  :diminish flyspell-mode
  :delight)

(use-package flyspell-popup
  :hook (flyspell-mode . flyspell-popup-auto-correct-mode)
  :after flyspell
  :bind (("C-:" . flyspell-popup-correct)))
;;;---------------------------------------------------------


;;; Emacs behaviour
;;;---------------------------------------------------------
(use-package ido
  :init (ido-mode t)
  (setq ido-enable-flex-matching t
        ido-everywhere t
        ido-create-new-buffer 'always
        ido-default-file-method 'selected-window
        ido-default-buffer-method 'selected-window))

(use-package move-text
  :config
  (move-text-default-bindings))

(use-package smex
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-c C-c M-x" . execute-extended-command))
  :config (setq smex-history-length 20))

(use-package diminish)
;;;---------------------------------------------------------

;;; Code editing modes and helper functions
;;;---------------------------------------------------------
;;;;; Major modes
;;;;; ------------------------------------------------------
(use-package json-mode
  :hook (prog-mode . highlight-indent-guides-mode)
  :config (setq highlight-indent-guides-method 'character
                highlight-indent-guides-responsive 'top))

(use-package markdown-mode
  :mode "\\.markdown\\'" "\\.md\\'")
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(autoload 'gfm-mode "markdown-mode"
   "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

(use-package graphviz-dot-mode
  :config (setq graphviz-dot-view-command "eog"
                graphviz-dot-view-edit-command t
                graphviz-dot-save-before-view t))
;;;;; ------------------------------------------------------

(use-package ggtags
  :config (setq ggtags-mode-line-project-name nil)
  :hook (prog-mode . ggtags-mode))

(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode )
  :config (setq highlight-indent-guides-method 'character
                highlight-indent-guides-responsive 'top))

(use-package whitespace
  :bind (("C-c T w" . whitespace-mode))
  :hook ((prog-mode . whitespace-mode)
         (text-mode . whitespace-mode)
         (conf-mode . whitespace-mode))
  :config (setq whitespace-line-column nil)
  (setq whitespace-style '(face indentation::space tabs trailing))
  :diminish whitespace-mode
  :delight)

(use-package yasnippet
  :init
  (let ((orig (lookup-key global-map (kbd "TAB"))))
    (yas-global-mode 1)
    (define-key yas-minor-mode-map (kbd "TAB") orig)
    (define-key yas-minor-mode-map [(tab)] orig))
  :diminish yas-minor-mode)

(use-package yasnippet-snippets)

(use-package change-inner
  :bind (("M-i" . change-inner)
         ("M-o" . change-outer)))

;;;;; Flycheck linters
;;;;; ------------------------------------------------------
(use-package flycheck
  :config
  :init (progn (global-flycheck-mode)
               (setq flycheck-emacs-lisp-load-path 'inherit)))

(use-package flycheck-demjsonlint)

(use-package flycheck-checkbashisms
  :config
  (flycheck-checkbashisms-setup))

(use-package flycheck-popup-tip
  :after flycheck
  :hook  (flycheck-mode . flycheck-popup-tip-mode))

(flycheck-define-checker proselint
  "A linter for prose."
  :command ("proselint" source-inplace)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ": "
	    (id (one-or-more (not (any " "))))
	    (message) line-end))
  :modes (text-mode markdown-mode gfm-mode))

(add-to-list 'flycheck-checkers 'proselint)
;;;;; ------------------------------------------------------

;;;;; Company code completion and backends
;;;;; ------------------------------------------------------
(use-package company
  :bind (("s-:" . company-complete))
  :init (global-company-mode)
  :diminish)

(use-package company-jedi
  :after company
  :init (add-to-list 'company-backends 'company-jedi))

(use-package company-c-headers
  :after company
  :init (add-to-list 'company-backends 'company-c-headers))

(use-package company-shell
  :after company
  :init (add-to-list 'company-backends 'company-shell))

(use-package company-quickhelp
  :ensure t
  :after company
  :init (company-quickhelp-mode))
;;;;; ------------------------------------------------------
;;;---------------------------------------------------------

;;; Visuals
;;;---------------------------------------------------------
(use-package ample-zen-theme)

(use-package mode-icons
  :after flycheck
  :init (progn
          (mode-icons-mode)
          (add-hook 'flycheck-after-syntax-check-hook 'mode-icons-reset)))
;;;---------------------------------------------------------

;;; Productivity
;;;---------------------------------------------------------
(use-package todotxt
  :bind (("C-x t" . todotxt))
  :config (setq todotxt-file "~/.todo/todo.txt"))

(use-package cheatsheet
  :bind ("s-c" . cheatsheet-show)
  :init (load "my-cheatsheet"))
;;;---------------------------------------------------------

(provide 'general-config)
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; general-config.el ends here
