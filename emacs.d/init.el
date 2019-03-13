;;;;;; init.el --- Daniel Bengtsson's init.el File For GNU Emacs

;; Copyright (C) 2018-2019  Daniel Bengtsson

;;; Commentary:

;; This is my personal startup file for GNU Emacs.  It has only recently
;; been tested on GNU Emacs 26.1, though it may run on other versions.
;;
;; Set some basic Emacs settings and load further configuration files
;; for additional customisation.
;;
;; First, the optional file early-init.el is called if it exists on
;; the load path. Here settings like proxy servers for the local
;; network can be done.
;;
;; At the end all elisp code in the directory "conf.d" and its sub
;; directories is loaded. Here I have placed all use-package calls.


;;; Code:
;;;TODO: minimize terminalmacs
;;  (cond ((display-graphic-p)
;;           ;; Graphical code goes here.
;;           )
;;          (t
;;           ;; Console-specific code
;;           ))
;; Update: use :when window-system in use-package

;; ===========================================================================
;; General settings and internal packages.
;; ===========================================================================
(add-to-list 'load-path '"~/.emacs.d/elisp/")
(load "my-functions" t) ; Load convenience functions used later in the init
(load "early-init") ; Load early-init if it exists

;; Package setup
;; -------------
(require 'package)

(add-to-list 'package-archives
             '("MELPA" . "http://melpa.org/packages/") t)
(package-initialize)                       ; Not needed for emacs27
(defun package--save-selected-packages (&rest opt) nil) ;Don't write the list
;;fetch the list of packages available if not already downloaded
(unless package-archive-contents
  (package-refresh-contents))

;; List of packages to install
(defvar packages-to-install
  '(
    use-package
    all-the-icons-ivy
    ample-zen-theme  ;; No workie when starting server
    call-graph
    ccls
    change-inner
    cheatsheet
    company
    company-c-headers
    company-jedi
    company-lsp
    company-prescient
    company-quickhelp
    company-shell
    counsel
    counsel-projectile
    dictcc
    diff-hl
    diminish
    flycheck
    flycheck-checkbashisms
    flycheck-flawfinder
    flycheck-popup-tip
    flylisp
    flyspell
    flyspell-popup
    git-commit
    graphviz-dot-mode
    highlight-indent-guides
    ivy
    ivy-prescient
    ivy-rich
    json-mode
    load-dir
    lsp-mode
    lsp-ui
    markdown-mode
    mode-icons ; No workie when starting server
    move-text
    projectile
    swiper
    whitespace
    yafolding
    yasnippet
    yasnippet-snippets
    )
  "Packages to install from package manager.")

(defvar packages-to-configure
  (append '(git-commit) packages-to-install)
  "Include deps or manually cloned packages that needs to be configured.")

; install the missing packages
(dolist (package packages-to-install)
  (unless (package-installed-p package)
    (package-install package)))

;; Visuals
;; -------
(show-paren-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)
(global-font-lock-mode t)
(set-face-attribute 'mode-line nil :font "DejaVu Sans Mono-10")
(set-face-attribute 'mode-line-inactive nil :font "DejaVu Sans Mono-10")


;; Start up directly
;; -----------------
(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)


;; Global modes
;; ------------
(global-cwarn-mode 1)

;; Locale and environment
;; ----------------------
(setq european-calendar-style 't              ; European style calendar
      calendar-week-start-day 1               ; Week starts monday
      ps-paper-type 'a4                       ; Specify printing format
      ispell-dictionary "american")           ; Set ispell dictionary
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment 'utf-8)
(set-selection-coding-system 'utf-8)

(setq grep-command "grep -i -nH -e ")

;; Personal
;; --------
(setq user-full-name "Daniel Bengtsson"
      user-mail-address "daniel.f.bengtsson@gmail.com")

;; Behaviour
;; ---------
(fset 'yes-or-no-p 'y-or-n-p)
(electric-pair-mode 1)
(setq-default
 display-buffer-reuse-frames t        ; Compilations in the same buffer
 make-backup-files nil                ; Don't litter my file system
 mouse-yank-at-point t                ; Respect point
 compilation-read-command nil         ; No need to ask every time
 indent-tabs-mode nil                 ; No tabs please
 show-paren-when-point-inside-paren t ; Highlight parens around point
 show-trailing-whitespace t           ; Always show trailing whitespace
 kill-read-only-ok t                  ; Copy text in read only buffers
 case-fold-search t                   ; Case insensitive search
 vc-follow-symlinks t                 ; Follow symlinks to repos
 gc-cons-threshold (* 10 1024 1024)   ; Reduce the frequency of garbage collection (default is 0.76MB, this sets it to 10MB)
 tramp-default-method "ssh"           ; Faster than the default setting scp.
 package-check-signature nil          ; So there seems to be a bug here
 load-prefer-newer t)                 ; Don't load old bytecode
(require 'log-edit)
;; Move custom configuration variables set by Emacs, to a separate file
(require 'custom)
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;; Keyboard bindings
;; -----------------
;; See conf.d/*-config.el for package specific bindings
(bind-keys ("<f4>" . menu-bar-mode)
	   ("M-<" . hippie-expand)
	   ("C-c c" . compile)
	   ("C-c o" . ff-find-other-file)
	   ("C-n" . next-error)
	   ("C-p" . previous-error)
	   ("<C-tab>" . other-window)
	   ("C-c t" . fayoh/grep-todos-in-dir)
	   ("s-SPC" . just-one-space))

;; Load package configs
;; --------------------
(require 'use-package)
(use-package load-dir
  :demand t)
(setq load-dir-recursive t)
(load-dir-one (concat user-emacs-directory "conf.d"))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
