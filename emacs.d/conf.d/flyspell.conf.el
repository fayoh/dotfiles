;;; flyspell.conf.el --- use-package definitions for flyspell packages -*- lexical-binding: t -*-

;; Author: Daniel Bengtsson

;;; Commentary:

;;; Code:

(when (memq 'flyspell packages-to-configure)
  (use-package flyspell
    :hook ((text-mode . flyspell-mode)
           (prog-mode . flyspell-prog-mode))
    :config (setq flyspell-issue-message-flag nil)
    :diminish flyspell-mode
    :delight)

  (when (memq 'flyspell-popup packages-to-configure)
    (use-package flyspell-popup
      :after flyspell
      :bind (("C-:" . flyspell-popup-correct)))))

(provide 'flyspell.conf)
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; flyspell.conf.el ends here

