;; Font -----------------------------------------------------------------------
; http://www.emacswiki.org/emacs/XftGnuEmacs#toc14
; also http://peadrop.com/blog/2007/01/06/pretty-emacs

(set-default-font "Monospace-10")
(set-default-font "Monospace-14")
(add-to-list 'default-frame-alist '(font . "Monospace-14"))

(defun small-fonts ()
  "Small fonts"
  (interactive)
  (set-default-font "Monospace-10"))


(defun large-fonts ()
  "Large fonts"
  (interactive)
  (set-default-font "Monospace-14"))

(defun super-large-fonts ()
  "Super large fonts"
  (interactive)
  (set-default-font "Monospace-25"))

(global-set-key [f5] 'small-fonts)
(global-set-key [f6] 'large-fonts)
(global-set-key [f7] 'super-large-fonts)

(global-set-key (read-kbd-macro "M-[ a") [M-up])
(global-set-key (read-kbd-macro "M-[ b") [M-down])
(global-set-key (read-kbd-macro "M-[ c") [M-right])
(global-set-key (read-kbd-macro "M-[ d") [M-left])

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; See also /etc/emacs21/site-start.d/99user-defined.el

;; Emacs Load Path
;; http://www.emacswiki.org/emacs/LoadPath
(let ((default-directory "~/.emacs.d/el/"))
  (setq load-path
        (append
         (let ((load-path (copy-sequence load-path))) ;; Shadow
           (append 
            (copy-sequence (normal-top-level-add-to-load-path '(".")))
            (normal-top-level-add-subdirs-to-load-path)))
         load-path)))

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  (package-initialize))

(setq frame-title-format "%b - Emacs")
;;(add-hook 'c-mode-common-hook
;;	  (lambda () (c-toggle-auto-hungry-state 1)))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; http://www.emacswiki.org/cgi-bin/wiki/frame-restore.el
;;(desktop-save-mode 1)

(column-number-mode 1)

;; (standard-display-european 1) ;; Umlaute

(defun confirm-exit-emacs ()
  "ask for confirmation before exiting emacs"
  (interactive)
  (if (yes-or-no-p "Are you sure you want to exit? ")
      (save-buffers-kill-emacs)))

;; Answer yes or no questions with y or n
(fset 'yes-or-no-p 'y-or-n-p)

(global-unset-key "\C-x\C-c")
(global-set-key "\C-x\C-c" 'confirm-exit-emacs)
(tool-bar-mode -1)

;; pdb.set_trace() macro
(fset 'pdb-set
      "import pdb; pdb.set_trace()")
(global-set-key "\M-t" 'pdb-set)


;; http://unix.stackexchange.com/questions/19494/how-to-colorize-text-in-emacs
(defun ansi-color-apply-on-region-int (beg end)
  "interactive version of func"
  (interactive "r")
  (ansi-color-apply-on-region beg end))

;; nxml-mode ------------------------------------------------------------------

;;(load "/usr/share/emacs/site-lisp/nxml-mode/rng-auto.el")
(setq auto-mode-alist
      (cons '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\|html\\|pt\\|cpt\\|zcml\\|kid\\)\\'" . nxml-mode)
	    auto-mode-alist))

;; Disable silly magic-mode-alist which otherwise automatically
;; invokes html-helper-mode
(setq magic-mode-alist ())

;; CSS mode: Different indent -------------------------------------------------
(setq cssm-indent-function #'cssm-c-style-indenter)
(setq cssm-indent-level 4)

;; Truncate lines, see http://www.emacswiki.org/cgi-bin/wiki/TruncateLines
(setq truncate-partial-width-windows nil)

;; http://www.emacsblog.org/2007/04/24/package-faves-iswitchb/ ----------------
;;============================================================
;; iswitchb
;;============================================================
(require 'iswitchb)  
;(iswitchb-mode 1)
;;============================================================
;; iswitchb ignores
;;============================================================
(add-to-list 'iswitchb-buffer-ignore "^ ")
;; (add-to-list 'iswitchb-buffer-ignore "*Messages*")
;; (add-to-list 'iswitchb-buffer-ignore "*Buffer")
(add-to-list 'iswitchb-buffer-ignore "*Completions")

(setq iswitchb-default-method  'samewindow)


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


;; ivy, swiper
;; (ivy-mode 1)
;; (global-set-key "\C-s" 'swiper)

;; ido
(require 'ido)
(ido-mode 1)
(setq ido-show-dot-for-dired 1)


;; Set environment variable for git
(setenv "GIT_PAGER" "cat")


;; Magit https://magit.vc/manual/magit/Getting-started.html#Getting-started
(global-set-key (kbd "C-x g") 'magit-status)
(global-magit-file-mode 1)


;; Easier window switching
(windmove-default-keybindings 'super)


;; python-mode.el                                                               
(require 'python-mode)
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;; http://www.flycheck.org/en/latest/user/installation.html
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))
;;(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'python-mode-hook 'flycheck-mode)

;; http://www.illusori.co.uk/blog/2011/07/25/perl_php_static_analysis_with_emacs_flymake.html
;; Disable in-place checking, and tell it to use /tmp/ for the temp files.
;; (setq temporary-file-directory "/tmp/")
;; (setq flymake-run-in-place nil)


;; http://hustoknow.blogspot.de/2010/09/emacs-and-pyflakes-using-tmp-directory.html

;; (defun flymake-create-temp-in-system-tempdir (filename prefix)
;;   (make-temp-file (or prefix "flymake")))

;; (when (load "flymake" t)
;;   (defun flymake-pep8-init ()
;;     (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                        'flymake-create-temp-in-system-tempdir))
;;            (local-file (file-relative-name
;;                         temp-file
;;                         (file-name-directory buffer-file-name))))
;;       (list "flake8.py" (list local-file))))
;;   (add-to-list 'flymake-allowed-file-name-masks
;;                '("\\.py\\'" flymake-pep8-init)))
;; (add-hook 'python-mode-hook 'flymake-mode)

;; (require 'flymake-cursor)

;; tramp

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



;; http://www.emacswiki.org/emacs/FlyMake#toc17
(custom-set-faces
 '(flymake-errline ((((class color)) (:foreground "red"))))
 '(flymake-warnline ((((class color)) (:underline "green")))))


(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))


;; https://www.emacswiki.org/emacs/RainbowDelimiters
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)


;; http://orgmode.org/worg/org-tutorials/orgtutorial_dto.php
(require 'org-install)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-log-done t)

;; (add-hook 'org-mode-hook #'visual-line-mode)
;; (add-hook 'org-mode-hook #'auto-fill-mode)

(setq org-startup-indented t)



(if (file-exists-p "~/.emacs.d/desktop.el")
    (load "~/.emacs.d/desktop"))

(setenv "PATH" (concat "~/bin:" (getenv "PATH")))
(setq exec-path (append '("/sw/bin") exec-path))
