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

(when (memq 'flyspell packages-to-configure)
  (use-package flyspell
    :hook ((text-mode . flyspell-mode)
	   (prog-mode . flyspell-prog-mode))
    :config (setq flyspell-issue-message-flag nil)
    :diminish flyspell-mode
    :delight))

(when (memq 'flyspell-popup packages-to-configure)
  (use-package flyspell-popup
    :hook (flyspell-mode . flyspell-popup-auto-correct-mode)
    :after flyspell
    :bind (("C-:" . flyspell-popup-correct))))
;;;---------------------------------------------------------


;;; Emacs behaviour
;;;---------------------------------------------------------
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
;;;---------------------------------------------------------

;;; Code editing modes and helper functions
;;;---------------------------------------------------------
;;;;; Major modes
;;;;; ------------------------------------------------------
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

;; Built-in
(use-package whitespace
  :bind (("C-c T w" . whitespace-mode))
  :hook ((prog-mode . whitespace-mode)
         (text-mode . whitespace-mode)
         (conf-mode . whitespace-mode))
  :config (setq whitespace-line-column nil)
  (setq whitespace-style '(face indentation::space tabs trailing))
  :diminish whitespace-mode
  :delight)

(when (memq 'yasnippet packages-to-configure)
  (use-package yasnippet
    :init
    (let ((orig (lookup-key global-map (kbd "TAB"))))
      (yas-global-mode 1)
      (define-key yas-minor-mode-map (kbd "TAB") orig)
      (define-key yas-minor-mode-map [(tab)] orig))
    :diminish yas-minor-mode))

(when (memq 'change-inner packages-to-configure)
  (use-package change-inner
    :bind (("M-i" . change-inner)
	   ("M-o" . change-outer))))

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

(when (memq 'company-c-headers packages-to-configure)
  (use-package company-c-headers
    :after company
    :init (add-to-list 'company-backends 'company-c-headers)))

(when (memq 'company-shell packages-to-configure)
  (use-package company-shell
    :after company
    :init (add-to-list 'company-backends 'company-shell)))

(when (memq 'company-quickhelp packages-to-configure)
  (use-package company-quickhelp
    :ensure t
    :after company
    :init (company-quickhelp-mode)))
;;;;; ------------------------------------------------------
;;;---------------------------------------------------------

;;; Visuals
;;;---------------------------------------------------------
(when (memq 'ample-zen-theme packages-to-configure)
  (use-package ample-zen-theme))

(when (memq 'mode-icons packages-to-configure)
  (use-package mode-icons
    :after flycheck
    :init (progn
	    (mode-icons-mode)
	    (add-hook 'flycheck-after-syntax-check-hook 'mode-icons-reset))))
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
    :init (load "my-cheatsheet")))
;;;---------------------------------------------------------

(provide 'general-config)
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; general-config.el ends here
