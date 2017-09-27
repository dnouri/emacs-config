(set-scroll-bar-mode 'left)


;; shell mode -----------------------------------------------------------------
(setq ansi-color-names-vector ; better contrast colors
      ["black" "red4" "green4" "yellow4"
       "blue3" "magenta4" "cyan4" "white"])
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)


;; hl-line mode and colour
(global-hl-line-mode 1)
(set-face-background 'hl-line "#FFE")


;; http://stackoverflow.com/questions/704616
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)


;; Coloring for diff 
(custom-set-faces
 '(diff-added ((t (:foreground "Dark Green"))) 'now)
 '(diff-removed ((t (:foreground "Dark Red"))) 'now)
 )


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


;; Customized Key Bindings ----------------------------------------------------
;; Euro: C-M-e
(defun insert-euro-sign ()
  (interactive)
  (insert (make-char 'latin-iso8859-15 #xa4))
)
(global-set-key (kbd "C-M-e") 'insert-euro-sign)


;; SuperCollider, see http://space.k-hornz.de:8888/space/uploads/scel-tut.txt
;; (require 'sclang)

;; (eval-after-load "w3m"
;;  '(progn
;;  (define-key w3m-mode-map [left] 'backward-char)
;;  (define-key w3m-mode-map [right] 'forward-char)
;;  (define-key w3m-mode-map [up] 'previous-line)
;;  (define-key w3m-mode-map [down] 'next-line)))

;; (add-hook 'sclang-mode-hook
;;     (lambda () (setq indent-tabs-mode nil))
;;     )


;; ERC, http://www.emacswiki.org/cgi-bin/wiki/ErcSound
(add-hook 'erc-text-matched-hook 'erc-beep-on-match)
(setq erc-beep-match-types '(current-nick keyword))
(setq erc-auto-query 'window-noselect)
(setq erc-autojoin-channels-alist
      '(("freenode.net"
         "#kotti" "#pyramid" "##machinelearning" "#pydata"
         )))
(require 'erc-match)
(setq erc-keywords '("Kotti" "kotti"))
(erc-match-mode t)
(setq erc-server-send-ping-timeout nil)


;; http://www.emacswiki.org/emacs/ErcPageMe
(require 'notifications)
(defun erc-global-notify (match-type nick message)
  "Notify when a message is recieved."
  (notifications-notify
   :title nick
   :body message
   :app-icon "/usr/share/notify-osd/icons/gnome/scalable/status/notification-message-im.svg"
   :urgency 'low))

(add-hook 'erc-text-matched-hook 'erc-global-notify)



;; http://hexmode.openweblog.com/472367.html
(defvar mah/erc-nick-notify-last '(0 0 0))
(defvar mah/erc-nick-ntify-delay '(0 5 0))
(defvar mah/erc-nick-notify-cmd "notify-send")
(defvar mah/erc-nick-notify-icon
  "/usr/share/icons/default.kde/48x48/apps/edu_languages.png")
(defvar mah/erc-nick-notify-timeout 10000)
(defvar mah/erc-nick-notify-urgency "low");
(defvar mah/erc-nick-notify-category "im.received");

(defun mah/erc-nick-notify ()
  "Notify me when my nick shows up.  This function should be in
the insert-post-hook."
  (let ((now (current-time)))
    (when (time-less-p mah/erc-nick-notify-delay
                       (time-since mah/erc-nick-notify-last))
      (setq mah/erc-nick-notify-last now)
      (goto-char (point-min))
      (when (re-search-forward
             (concat "^\\("
                     "\\(<\\([^>]*\\)\>\\)" ; <someone>
                     "\\|"
                     ;; Don't match if we're saying something
                     "\\(\\* " (regexp-quote (erc-current-nick)) "\\)"
                     "\\)"
                     "\\(.*"
                     (regexp-quote (erc-current-nick)) ".*\\)")
             nil t)
        (let ((msg (concat 
                    (when (> (length (match-string-no-properties 2)) 0)
                      (concat "<b>&lt;" (match-string-no-properties 3)
                              "&gt;</b> "))
                    (match-string-no-properties 5))))
          (shell-command (concat mah/erc-nick-notify-cmd
                                 " -i " mah/erc-nick-notify-icon
                                 " -t " (int-to-string
                                         mah/erc-nick-notify-timeout)
                                 " -u " mah/erc-nick-notify-urgency
                                 " -c " mah/erc-nick-notify-category
                                 " -- "
                                 "'" (buffer-name) "'"
                                 " '" msg "'")))))))

;;(add-hook 'erc-insert-pre-hook 'mah/erc-nick-notify)

;; jabber.el: http://www.emacswiki.org/emacs/JabberEl
;; Message alert hooks
;; (require 'jabber)
;; (define-jabber-alert echo "Show a message in the echo area"
;;   (lambda (msg)
;;     ()))



(load "~/.emacs.d/mu4e")
