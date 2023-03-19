(set-scroll-bar-mode 'left)

;; Theme
(load-theme 'doom-pine t)

;; Fonts
(set-frame-font "Monospace-10")
(set-frame-font "Monospace-14")
(add-to-list 'default-frame-alist '(font . "Monospace-14"))
(defun medium-fonts1 ()
  "Medium fonts"
  (interactive)
  (set-frame-font "Cascadia Mono-13"))
(defun large-fonts1 ()
  "Large fonts"
  (interactive)
  (set-frame-font "Cascadia Mono-14"))
(defun super-large-fonts1 ()
  "Super large fonts"
  (interactive)
  (set-frame-font "Cascadia Mono-22"))

(global-set-key [s-f7] 'medium-fonts1)
(global-set-key [s-f8] 'large-fonts1)
(global-set-key [s-f9] 'super-large-fonts1)

(defun medium-fonts2 ()
  "Medium fonts"
  (interactive)
  (set-frame-font "Comic Code Daniel-12"))
(defun large-fonts2 ()
  "Large fonts"
  (interactive)
  (set-frame-font "Comic Code Daniel-13"))
(defun super-large-fonts2 ()
  "Super large fonts"
  (interactive)
  (set-frame-font "Comic Code Daniel-22"))

(global-set-key [f7] 'medium-fonts2)
(global-set-key [f8] 'large-fonts2)
(global-set-key [f9] 'super-large-fonts2)

;; shell mode
(setq ansi-color-names-vector ; better contrast colors
      ["black" "red4" "green4" "yellow4"
       "blue3" "magenta4" "cyan4" "white"])
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; hl-line mode and colour
(global-hl-line-mode 1)
;; (set-face-background 'hl-line "#FFE")

;; ;; http://stackoverflow.com/questions/704616
;; (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
;; (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; http://www.emacswiki.org/emacs/DynamicAbbreviations
(global-set-key (kbd "C-<tab>") 'dabbrev-expand)
(define-key minibuffer-local-map (kbd "C-<tab>") 'dabbrev-expand)

;; Emacs Client / Server ------------------------------------------------------
;; http://www.emacswiki.org/cgi-bin/wiki.pl?EmacsClient
;; https://kanis.fr/svn/trunk/wk/lisp/emacs.d/ivan-server.el
(defun ivan-server-start-filter-function (process output)
  "Filter function for `ivan-server-start', which checks for an
 accessible Emacs process acting as a server by calling
 `emacsclient --eval t' as an external asynchronous
 process. Process output is filtered by this function which only
 calls `server-start' when no server is running"
  (if (equal output "t\n")
      (message "Not starting server, one instance already running...")
    (server-start)))
(defun ivan-server-start ()
  "Call `server-start' only if no other accessible Emacs process
 is already acting as a server for client processes. Adapted from
 Stefan Monnier's code"
  (let ((process-connection-type nil))
    (set-process-filter (start-process "ivan-process" nil "emacsclient"
                                       "--eval" "t")
                        'ivan-server-start-filter-function)))
(ivan-server-start)
;;(add-hook 'after-init-hook 'server-start)
(add-hook 'server-switch-hook
	  (lambda nil
	    (let ((server-buf (current-buffer)))
	      (bury-buffer)
	      (switch-to-buffer-other-frame server-buf))))
(add-hook 'server-done-hook 'delete-frame)

(if (file-exists-p "~/.emacs.d/mu4e.el")
    (load "~/.emacs.d/mu4e"))
