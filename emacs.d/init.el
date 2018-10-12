;;;;;; init.el --- Daniel Bengtsson's init.el File For GNU Emacs

;; Copyright (C) 2018  Daniel Bengtsson

;; Author: Andrew Kensler
;; Version: 20180825
;; Keywords: local, convenience

;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the "Software"),
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included
;; in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
;; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.

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
(add-to-list 'load-path "~/.emacs.d/elisp/")
(load "my-functions") ; Load convenience functions used later in the init
(load "early-init" t) ; Load early-init if it exists

;; Package setup
;; -------------
(require 'package)

(add-to-list 'package-archives
             '("MELPA" . "http://melpa.org/packages/") t)
(package-initialize)
(defun package--save-selected-packages (&rest opt) nil) ;Don't write the list
;;fetch the list of packages available if not already downloaded
(unless package-archive-contents
  (package-refresh-contents))

;; List of packages to install
(defvar packages-to-install
  '(
    use-package
    ample-zen-theme
    change-inner
    cheatsheet
    company
    company-c-headers
    company-jedi
    company-quickhelp
    company-shell
    counsel
    dictcc
    diminish
    flycheck
    flycheck-checkbashisms
    flycheck-popup-tip
    flyspell
    flyspell-popup
    ggtags
    graphviz-dot-mode
    highlight-indent-guides
    ivy
    ivy-prescient
    json-mode
    markdown-mode
    mode-icons
    move-text
    whitespace
    yasnippet
    yasnippet-snippets
    )
    "Packages to install from package manager.")

(defvar packages-to-configure
  (append '() packages-to-install)
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
(set-language-environment "Latin-1")
(setq european-calendar-style 't              ; European style calendar
      calendar-week-start-day 1               ; Week starts monday
      ps-paper-type 'a4                       ; Specify printing format
      ispell-dictionary "english")            ; Set ispell dictionary
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
 vc-follow-symlinks t)                ; Follow symlinks to repos

;; Move custom configuration variables set by Emacs, to a separate file
(require 'custom)
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;; Keyboard bindings
;; -----------------
;; See conf.d/*-config.el for package specific bindings
(global-set-key (kbd "M-<") 'hippie-expand)
(global-set-key [f4] 'menu-bar-mode)
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-n") 'next-error)
(global-set-key (kbd "C-p") 'previous-error)
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "C-c t") 'balle-grep-todos-in-dir)

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
