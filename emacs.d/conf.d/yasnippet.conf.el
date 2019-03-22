;;; yasnippet.conf.el --- use-package definitions for yasnippet packages -*- lexical-binding: t -*-

;; Author: Daniel Bengtsson

;;; Commentary:

;;; Code:

(when (memq 'yasnippet packages-to-configure)
  (use-package yasnippet
    :init
    (let ((orig (lookup-key global-map (kbd "TAB"))))
      (yas-global-mode 1)
      (define-key yas-minor-mode-map (kbd "TAB") orig) ; TODO: use bind-keymap
      (define-key yas-minor-mode-map [(tab)] orig))
    :diminish yas-minor-mode)

  (use-package yasnippet-snippets)

  (when (memq 'ivy packages-to-configure)
    (use-package ivy-yasnippet
      :bind ("s-<tab>" . ivy-yasnippet))))

(provide 'yasnippet.conf)
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; yasnippet.conf.el ends here

