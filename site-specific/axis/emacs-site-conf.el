;;; axis-config --- Summary
;;; Commentary:
;;; Startup only needed for axis environment

;;; Code:
(require 'use-package)
(setq user-mail-address "danielfb@axis.com")

(defun get-oe-module-name ()
  "Find the git top directory and use that as module name.
This should work for all gits checked out with devtool modify

BODGE: since git rev-parse returns a path whitout a final '/' we
can use the function for getting the filename to get the top
level directory"
  (file-name-nondirectory (shell-command-to-string
                           "git rev-parse --show-toplevel")))

;; If BUILDDIR is set we assume that we are compiling an OE package
;; that has been checked out via devtool
;; and will use myffbuild to correctly build
;; NOTE: Needs to have an emacs run from a shell where oe-initenv has been
;; run for the correct tree
(setq build-tree (getenv "BUILDDIR"))
(require 'compile)
(if (bound-and-true-p build-tree)
    (add-hook 'prog-mode-hook
              (lambda ()
                (set (make-local-variable 'compile-command)
                     (concat "myffbuild "
                             (get-oe-module-name))))))

;; Set title to reflect what build tree we can compile in this instance
;; TODO: Sort out only dist and build dirs (sunshade:q6000-e)
(if build-tree
    (let* ((dirs (reverse (split-string build-tree "/")))
           (unit (car dirs))
           (dist (car (cdr (cdr dirs)))))
      (setq frame-title-format (list "%b @ " dist ":" unit))))

(defun axis-devtool-co (name)
  "Check out git to (NAME) your workspace."
  (interactive "sModule name:")
  (async-shell-command (concat "devtool modify -w "
                               name)))

(defun axis-devtool-rm (name)
  "Remove git (NAME) from your workspace."
  (interactive "sModule name:")
  (async-shell-command (concat "devtool reset "
                               name)))

(provide 'axis-config)
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; emacs-site-conf.el ends here
