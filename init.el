;;; init.el --- Main Emacs configuration -*- lexical-binding: t; -*-

;;; Code:

;; Custom file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Load path — lib/ takes precedence over same-named libraries
(let ((default-directory "~/.emacs.d/lib/"))
  (setq load-path
        (append
         (let ((load-path (copy-sequence load-path)))
           (append 
            (copy-sequence (normal-top-level-add-to-load-path '(".")))
            (normal-top-level-add-subdirs-to-load-path)))
         load-path)))

;; Package management
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(package-install-selected-packages t)

;; General settings
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(column-number-mode 1)
(setq truncate-partial-width-windows nil)
(set-default-coding-systems 'utf-8)
(fset 'yes-or-no-p 'y-or-n-p)
(setq grep-command "grep -nH -I -e ")

;; Confirm before exiting
(defun confirm-exit-emacs ()
  "Prompt for confirmation before exiting Emacs."
  (interactive)
  (if (yes-or-no-p "Are you sure you want to exit? ")
      (save-buffers-kill-emacs)))
(global-unset-key "\C-x\C-c")
(global-set-key "\C-x\C-c" 'confirm-exit-emacs)

;; Window navigation with super key
(windmove-default-keybindings 'super)
(setq windmove-allow-all-windows t)

;; Apply ANSI color codes to a region
(defun ansi-color-apply-on-region-int (beg end)
  "Apply ANSI color codes to the region from BEG to END."
  (interactive "r")
  (ansi-color-apply-on-region beg end))

;; tempbuf — automatically clean up unused buffers
(require 'tempbuf)
(add-hook 'dired-mode-hook 'turn-on-tempbuf-mode)
(add-hook 'custom-mode-hook 'turn-on-tempbuf-mode)
(add-hook 'Man-mode-hook 'turn-on-tempbuf-mode)
(add-hook 'view-mode-hook 'turn-on-tempbuf-mode)
(add-hook 'find-file-hook 'turn-on-tempbuf-mode)

;; ido
(require 'ido)
(ido-mode 1)
(setq ido-show-dot-for-dired 1)
(setq ido-enable-flex-matching t)

;; Git
(setenv "GIT_PAGER" "cat")
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;; Python
(require 'python)
(add-hook 'comint-output-filter-functions 'python-pdbtrack-comint-output-filter-function)
(setq python-indent-def-block-scale 1)
(fset 'pdb-set "breakpoint()")
(global-set-key "\M-t" 'pdb-set)

;; eglot — use ruff as Python language server
(add-hook 'python-mode-hook 'eglot-ensure)
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(python-mode . ("ruff" "server"))))

;; C/C++
(setq gdb-many-windows t
      gdb-show-main t)

;; Tramp
(require 'tramp)
(setq tramp-auto-save-directory "~/.emacs.d/tramp-autosave")

;; Tramp — open files in Docker containers: /docker:container:/path
(push
 (cons
  "docker"
  '((tramp-login-program "docker")
    (tramp-login-args (("exec" "-it") ("%h") ("/bin/bash")))
    (tramp-remote-shell "/bin/sh")
    (tramp-remote-shell-args ("-i") ("-c"))))
 tramp-methods)
(defadvice tramp-completion-handle-file-name-all-completions
  (around dotemacs-completion-docker activate)
  "Complete Docker container names for /docker: paths."
  (if (equal (ad-get-arg 1) "/docker:")
      (let* ((dockernames-raw (shell-command-to-string "docker ps | perl -we 'use strict; $_ = <>; m/^(.*)NAMES/ or die; my $offset = length($1); while(<>) {substr($_, 0, $offset, q()); chomp; for(split m/\\W+/) {print qq($_:\n)} }'"))
             (dockernames (cl-remove-if-not
                           #'(lambda (dockerline) (string-match ":$" dockerline))
                           (split-string dockernames-raw "\n"))))
        (setq ad-return-value dockernames))
    ad-do-it))

;; File associations
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))

;; Modules
(load "~/.emacs.d/eat-config.el")
(load "~/.emacs.d/theme-cleanup.el")

(if (and (display-graphic-p) (file-exists-p "~/.emacs.d/desktop.el"))
    (load "~/.emacs.d/desktop"))

(if (file-exists-p "~/.emacs.d/pi-config.el")
    (load "~/.emacs.d/pi-config.el"))

(if (file-exists-p "~/.emacs.d/my-menu.el")
    (load "~/.emacs.d/my-menu.el"))

;; PATH — mise shims and ~/bin
(let ((mise-shims (expand-file-name "~/.local/share/mise/shims")))
  (setenv "PATH" (concat (getenv "PATH") ":" mise-shims))
  (add-to-list 'exec-path mise-shims t))
(let ((home-bin (expand-file-name "~/bin")))
  (setenv "PATH" (concat home-bin ":" (getenv "PATH")))
  (add-to-list 'exec-path home-bin))

;;; init.el ends here
