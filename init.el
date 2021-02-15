;; custom.el
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Libraries in ~/emacs.d/lib should take precedence over other
;; libraries with same name
;; http://www.emacswiki.org/emacs/LoadPath
(let ((default-directory "~/.emacs.d/lib/"))
  (setq load-path
        (append
         (let ((load-path (copy-sequence load-path))) ;; Shadow
           (append 
            (copy-sequence (normal-top-level-add-to-load-path '(".")))
            (normal-top-level-add-subdirs-to-load-path)))
         load-path)))

;; package
(require 'package)
(add-to-list 'package-archives
             '("MELPA Stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(byte-recompile-directory (expand-file-name "~/.emacs.d") 0)

;; Misc customization
(tool-bar-mode -1)
(setq frame-title-format "%b - Emacs")
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(column-number-mode 1)
(setq truncate-partial-width-windows nil)
(defun confirm-exit-emacs ()
  "ask for confirmation before exiting emacs"
  (interactive)
  (if (yes-or-no-p "Are you sure you want to exit? ")
      (save-buffers-kill-emacs)))
(fset 'yes-or-no-p 'y-or-n-p)
(global-unset-key "\C-x\C-c")
(global-set-key "\C-x\C-c" 'confirm-exit-emacs)
(windmove-default-keybindings 'super) ;; easier window switching

;; http://unix.stackexchange.com/questions/19494/how-to-colorize-text-in-emacs
(defun ansi-color-apply-on-region-int (beg end)
  "interactive version of func"
  (interactive "r")
  (ansi-color-apply-on-region beg end))

;; tempbuf-mode
(require 'tempbuf)
(add-hook 'dired-mode-hook 'turn-on-tempbuf-mode)
(add-hook 'custom-mode-hook 'turn-on-tempbuf-mode)
(add-hook 'w3-mode-hook 'turn-on-tempbuf-mode)
(add-hook 'Man-mode-hook 'turn-on-tempbuf-mode)
(add-hook 'view-mode-hook 'turn-on-tempbuf-mode)
;; It may also be reasonable to activate it by default on any visited
;; file buffer (buffers with unsaved content will not get automatically
;; deleted, anyway):
(add-hook 'find-file-hooks 'turn-on-tempbuf-mode)

;; ido
(require 'ido)
(ido-mode 1)
(setq ido-show-dot-for-dired 1)
(setq ido-enable-flex-matching t)

;; Set environment variable for git
(setenv "GIT_PAGER" "cat")

;; Magit https://magit.vc/manual/magit/Getting-started.html#Getting-started
(global-set-key (kbd "C-x g") 'magit-status)
(global-magit-file-mode 1)

;; python-mode.el                                                               
(require 'python-mode)
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;; Python pdb.set_trace() macro
(fset 'pdb-set
      "import pdb; pdb.set_trace()")
(global-set-key "\M-t" 'pdb-set)

;; http://www.flycheck.org/en/latest/user/installation.html
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))
;;(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'python-mode-hook 'flycheck-mode)

;; Tramp
(require 'tramp)
(setq tramp-auto-save-directory "~/.emacs.d/tramp-autosave")
;; https://www.emacswiki.org/emacs/TrampAndDocker
;; Open files in Docker containers like so: /docker:drunk_bardeen:/etc/passwd
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
  "(tramp-completion-handle-file-name-all-completions \"\" \"/docker:\" returns
    a list of active Docker container names, followed by colons."
  (if (equal (ad-get-arg 1) "/docker:")
      (let* ((dockernames-raw (shell-command-to-string "docker ps | perl -we 'use strict; $_ = <>; m/^(.*)NAMES/ or die; my $offset = length($1); while(<>) {substr($_, 0, $offset, q()); chomp; for(split m/\\W+/) {print qq($_:\n)} }'"))
             (dockernames (cl-remove-if-not
                           #'(lambda (dockerline) (string-match ":$" dockerline))
                           (split-string dockernames-raw "\n"))))
        (setq ad-return-value dockernames))
    ad-do-it))

;; Open Matlab files in octave-mode
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))

;; http://orgmode.org/worg/org-tutorials/orgtutorial_dto.php
(require 'org-install)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
;; (setq org-log-done t)
;; (add-hook 'org-mode-hook #'visual-line-mode)
;; (add-hook 'org-mode-hook #'auto-fill-mode)
;; (setq org-startup-indented t)

(add-hook 'js2-mode-hook (lambda () (electric-layout-mode -1)))

(if (file-exists-p "~/.emacs.d/desktop.el")
    (load "~/.emacs.d/desktop"))

(setenv "PATH" (concat "~/bin:" (getenv "PATH")))
(setq exec-path (append '("/sw/bin") exec-path))

