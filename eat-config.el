;;; eat-config.el --- Eat terminal emulator configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Performance tuning, display optimizations, and utility functions
;; for the Eat terminal emulator.
;;
;; Keybindings:
;;   C-c e p — toggle performance mode
;;   C-c e c — cleanup dead buffers
;;   C-c e r — performance report

;;; Code:

;; ---------------------------------------------------------------------------
;; General performance
;; ---------------------------------------------------------------------------

(setq read-process-output-max (* 1024 1024)
      gc-cons-threshold (* 50 1024 1024))

(run-with-idle-timer 5 t #'garbage-collect)

(set-face-attribute 'nobreak-space nil :underline nil)

(setq scroll-conservatively 10000
      fast-but-imprecise-scrolling t)

;; ---------------------------------------------------------------------------
;; Eat core settings
;; ---------------------------------------------------------------------------

(with-eval-after-load 'eat
  (setq eat-minimum-latency 0.033
        eat-maximum-latency 0.2
        eat-term-scrollback-size 50000
        eat-enable-sixel-display nil
        eat-enable-blinking-text nil
        eat-very-visible-cursor-type nil))

;; ---------------------------------------------------------------------------
;; Font — use Cascadia Mono NF in eat buffers
;; ---------------------------------------------------------------------------

(add-hook 'eat-mode-hook
          (lambda ()
            (set-frame-font "-SAJA-Cascadia Mono NF-regular-normal-normal-*-*-*-*-*-m-0-iso10646-1" nil t)))

;; ---------------------------------------------------------------------------
;; Echo area management — lock height while eat buffers are visible
;; ---------------------------------------------------------------------------

(defvar eat--original-echo-area-settings nil
  "Original echo area settings, saved for restoration.")

(defvar eat--echo-area-locked nil
  "Non-nil when echo area height is locked for eat.")

(defun eat--any-eat-window-visible-p (&optional frame)
  "Return non-nil if any eat buffer is visible in FRAME."
  (catch 'found
    (walk-windows 
     (lambda (window)
       (when (eq (buffer-local-value 'major-mode (window-buffer window))
                 'eat-mode)
         (throw 'found t)))
     'no-minibuf
     frame)))

(defun eat--manage-echo-area (&optional _frame)
  "Lock echo area height when eat buffers are visible."
  (let ((eat-visible (eat--any-eat-window-visible-p)))
    (cond
     ((and eat-visible (not eat--echo-area-locked))
      (setq eat--original-echo-area-settings
            `((max-mini-window-height . ,max-mini-window-height)
              (resize-mini-windows . ,resize-mini-windows)))
      (setq max-mini-window-height 1
            resize-mini-windows nil
            eat--echo-area-locked t))
     ((and (not eat-visible) eat--echo-area-locked)
      (setq max-mini-window-height 
            (alist-get 'max-mini-window-height eat--original-echo-area-settings 0.25)
            resize-mini-windows 
            (alist-get 'resize-mini-windows eat--original-echo-area-settings 'grow-only))
      (setq eat--echo-area-locked nil
            eat--original-echo-area-settings nil)))))

(defvar eat-window-update-timer nil
  "Timer for deferred echo area updates.")

(defvar eat-window-update-pending nil
  "Non-nil when an echo area update is pending.")

(defun eat--manage-echo-area-deferred (&optional _frame)
  "Defer echo area management to batch window configuration changes."
  (unless eat-window-update-pending
    (setq eat-window-update-pending t)
    (when eat-window-update-timer
      (cancel-timer eat-window-update-timer))
    (setq eat-window-update-timer
          (run-with-idle-timer 0.1 nil
                               (lambda ()
                                 (eat--manage-echo-area)
                                 (setq eat-window-update-pending nil))))))

(add-hook 'window-configuration-change-hook 'eat--manage-echo-area-deferred)
(add-hook 'window-buffer-change-functions 'eat--manage-echo-area-deferred)

(add-hook 'kill-emacs-hook
          (lambda ()
            (when eat--echo-area-locked
              (setq eat--echo-area-locked nil)
              (eat--manage-echo-area))))

;; ---------------------------------------------------------------------------
;; Display optimizations
;; ---------------------------------------------------------------------------

(defun eat-optimize-redisplay ()
  "Configure buffer-local display settings for terminal performance."
  (setq-local bidi-display-reordering nil
              bidi-paragraph-direction 'left-to-right
              bidi-inhibit-bpa t)
  (setq-local auto-hscroll-mode nil
              show-trailing-whitespace nil
              line-move-visual nil
              truncate-lines nil
              display-line-numbers nil)
  (setq-local font-lock-maximum-decoration 1
              jit-lock-chunk-size 500
              jit-lock-defer-time 0.05))

(add-hook 'eat-mode-hook 'eat-optimize-redisplay)

;; ---------------------------------------------------------------------------
;; Output processing
;; ---------------------------------------------------------------------------

(defun eat-optimize-output-processing ()
  "Reduce output processing overhead in eat buffers."
  (setq-local inhibit-modification-hooks t
              inhibit-point-motion-hooks t))

(add-hook 'eat-mode-hook 'eat-optimize-output-processing)

;; ---------------------------------------------------------------------------
;; Concurrent output throttling
;; ---------------------------------------------------------------------------

(defvar eat-active-output-buffers 0
  "Number of eat buffers currently processing output.")

(defvar eat-max-concurrent-outputs 2
  "Maximum concurrent output processing operations.")

(defvar eat-enable-output-throttling t
  "Non-nil to throttle concurrent output processing.")

(defun eat-throttle-output-advice (orig-fun &rest args)
  "Advice to limit concurrent output processing in ORIG-FUN with ARGS."
  (if (>= eat-active-output-buffers eat-max-concurrent-outputs)
      (run-with-idle-timer 0.05 nil
                          (lambda () (apply orig-fun args)))
    (let ((eat-active-output-buffers (1+ eat-active-output-buffers)))
      (unwind-protect
          (apply orig-fun args)
        (setq eat-active-output-buffers (1- eat-active-output-buffers))))))

(with-eval-after-load 'eat
  (when (and eat-enable-output-throttling
             (fboundp 'eat--process-output-queue))
    (advice-add 'eat--process-output-queue :around #'eat-throttle-output-advice)))

;; ---------------------------------------------------------------------------
;; Output batching
;; ---------------------------------------------------------------------------

(defvar eat-output-batch-size (* 64 1024)
  "Maximum bytes to process per batch.")

(defvar eat-enable-custom-batching nil
  "Non-nil to enable custom output batching (eat has its own).")

(defun eat-process-output-in-batches (orig-fun process string)
  "Process large output STRING from PROCESS in chunks via ORIG-FUN."
  (if (> (length string) eat-output-batch-size)
      (progn
        (funcall orig-fun process 
                 (substring string 0 eat-output-batch-size))
        (run-with-idle-timer 
         0.01 nil
         (lambda ()
           (when (buffer-live-p (process-buffer process))
             (with-current-buffer (process-buffer process)
               (eat-process-output-in-batches 
                orig-fun process
                (substring string eat-output-batch-size)))))))
    (funcall orig-fun process string)))

(with-eval-after-load 'eat
  (when (and eat-enable-custom-batching
             (fboundp 'eat-term-process-output))
    (advice-add 'eat-term-process-output :around #'eat-process-output-in-batches)))

;; ---------------------------------------------------------------------------
;; GC tuning for eat buffers
;; ---------------------------------------------------------------------------

(defun eat-optimize-gc ()
  "Increase GC threshold and schedule idle collection for eat buffers."
  (setq-local gc-cons-threshold (* 50 1024 1024))
  (run-with-idle-timer 
   2 nil 
   (lambda ()
     (when (and (buffer-live-p (current-buffer))
                (eq major-mode 'eat-mode))
       (garbage-collect)))))

(add-hook 'eat-mode-hook 'eat-optimize-gc)

;; ---------------------------------------------------------------------------
;; Window management
;; ---------------------------------------------------------------------------

(defun eat-disable-window-adjustments ()
  "Set minimum window height for eat buffers without fixing size."
  (setq-local window-min-height 5))

(add-hook 'eat-mode-hook 'eat-disable-window-adjustments)

;; ---------------------------------------------------------------------------
;; Utility functions
;; ---------------------------------------------------------------------------

(defun eat-cleanup-dead-buffers ()
  "Kill eat buffers whose processes have terminated."
  (interactive)
  (let ((cleaned 0))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (eq major-mode 'eat-mode)
                   (not (process-live-p (get-buffer-process buffer))))
          (kill-buffer buffer)
          (setq cleaned (1+ cleaned)))))
    (message "Cleaned up %d dead eat buffers" cleaned)))

(defun eat-performance-report ()
  "Display buffer count, memory usage, and latency for eat buffers."
  (interactive)
  (let* ((eat-buffers (seq-filter 
                      (lambda (b) 
                        (with-current-buffer b (eq major-mode 'eat-mode)))
                      (buffer-list)))
         (total-size 0)
         (live-count 0))
    (message "=== EAT Performance Report ===")
    (dolist (buf eat-buffers)
      (with-current-buffer buf
        (let ((size (buffer-size))
              (live (process-live-p (get-buffer-process buf))))
          (setq total-size (+ total-size size))
          (when live (setq live-count (1+ live-count)))
          (message "  %s: %dk %s" 
                  (buffer-name)
                  (/ size 1024)
                  (if live "LIVE" "dead")))))
    (message "Total: %d buffers (%d live), %dMB memory"
            (length eat-buffers)
            live-count
            (/ total-size (* 1024 1024)))
    (message "Current latency: min=%s max=%s"
            eat-minimum-latency
            eat-maximum-latency)))

(defvar eat-performance-mode nil
  "Non-nil when high-performance mode is active.")

(defun eat-toggle-performance-mode ()
  "Toggle between normal and high-performance modes."
  (interactive)
  (setq eat-performance-mode (not eat-performance-mode))
  (if eat-performance-mode
      (progn
        (setq eat-minimum-latency 0.1
              eat-maximum-latency 0.5
              eat-term-scrollback-size 30000
              eat-output-batch-size (* 32 1024))
        (message "EAT: high-performance mode ON"))
    (setq eat-minimum-latency 0.033
          eat-maximum-latency 0.2
          eat-term-scrollback-size 30000
          eat-output-batch-size (* 64 1024))
    (message "EAT: high-performance mode OFF")))

;; ---------------------------------------------------------------------------
;; Keybindings
;; ---------------------------------------------------------------------------

(global-set-key (kbd "C-c e p") 'eat-toggle-performance-mode)
(global-set-key (kbd "C-c e c") 'eat-cleanup-dead-buffers)
(global-set-key (kbd "C-c e r") 'eat-performance-report)

(provide 'eat-config)
;;; eat-config.el ends here
