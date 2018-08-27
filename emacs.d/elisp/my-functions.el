;;; my-functions --- Helper functions
;;; Commentary:

;;; Code:
(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

;; Find todo items in directory. Defaults to current directory
(defun balle-grep-todos-in-dir (dir &optional not-recursive)
  "Grep recursively for TODO comments in the given directory (DIR).
NOT-RECURSIVE do not enter subdirectories"
  (interactive "Ddirectory:")
  (let ((recur "-r"))
    (if not-recursive
        (setq recur "")
      )
    (grep (concat "grep -nH -I " recur " -E \"*TODO:|FIXME:|XXX:?\" " dir " 2>/dev/null"))
    )
  (enlarge-window 7)
  )

;; Make sure scripts are executable on save.
;; Code taken from StefanKamphausen on emacswiki.org
(add-hook 'after-save-hook
        #'(lambda ()
        (and (save-excursion
               (save-restriction
                 (widen)
                 (goto-char (point-min))
                 (save-match-data
                   (looking-at "^#!"))))
             (not (file-executable-p buffer-file-name))
             (shell-command (concat "chmod u+x " buffer-file-name))
             (message
              (concat "Saved as script: " buffer-file-name)))))

(provide 'my-functions)
;;; my-functions.el ends here
