;;; theme-cleanup.el --- Auto-disable themes before loading new ones -*- lexical-binding: t -*-

;;; Commentary:
;; Advises `load-theme' to disable all active themes first,
;; preventing conflicts and visual artifacts from stacked themes.

;;; Code:

(defun my/disable-all-themes-before-load (&rest _args)
  "Disable all active themes before loading a new one."
  (mapc #'disable-theme custom-enabled-themes))

(advice-add 'load-theme :before #'my/disable-all-themes-before-load)

(with-eval-after-load 'counsel
  (advice-add 'counsel-load-theme-action :before #'my/disable-all-themes-before-load))

(with-eval-after-load 'consult
  (advice-add 'consult-theme :before #'my/disable-all-themes-before-load))

(defun my/list-enabled-themes ()
  "Display all currently enabled themes."
  (interactive)
  (if custom-enabled-themes
      (message "Enabled themes: %s" 
               (mapconcat #'symbol-name custom-enabled-themes ", "))
    (message "No themes currently enabled")))

(defun my/disable-all-themes ()
  "Disable all currently enabled themes."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes)
  (message "All themes disabled"))

(provide 'theme-cleanup)
;;; theme-cleanup.el ends here
