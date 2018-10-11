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

(defmacro measure-time (&rest body)
  "Measure and return the running time of the code block BODY."
  (declare (indent defun))
  (let ((start (make-symbol "start")))
    `(let ((,start (float-time)))
       ,@body
       (- (float-time) ,start))))

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


(defun package-list-unaccounted-packages ()
  "Like `package-list-packages', but show only the packages that.
are installed and are not in `packages-to-install'.  Useful for
cleaning out unwanted packages."
  (interactive)
  (package-show-package-list
   (remove-if-not (lambda (x) (and (not (memq x packages-to-configure))
				   (not (package-built-in-p x))
				   (package-installed-p x)))
		  (mapcar 'car package-archive-contents))))

(provide 'my-functions)
;;; my-functions.el ends here
