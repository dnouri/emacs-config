;;; my-menu.el --- Personal configuration menu -*- lexical-binding: t; -*-

;;; Commentary:
;; A menu bar entry grouping font, theme, terminal, and pi agent controls.

;;; Code:

(easy-menu-define my-config-menu nil
  "Personal configuration menu."
  '("My Menu"
    ("Fonts"
     ["Cascadia Mono (coding)" my/font-switch-to-primary
      :style radio
      :selected (string= (face-attribute 'default :family) my/font-primary-family)]
     ["Comic Code (prose)" my/font-switch-to-secondary
      :style radio
      :selected (string= (face-attribute 'default :family) my/font-secondary-family)]
     "---"
     ["Increase Size" my/font-increase-size :keys "F7"]
     ["Decrease Size" my/font-decrease-size :keys "F6"])
    ("Themes"
     ["Load Theme..." load-theme]
     ["Disable All Themes" my/disable-all-themes]
     ["List Enabled Themes" my/list-enabled-themes])
    ("Terminal (EAT)"
     ["Toggle Performance Mode" eat-toggle-performance-mode
      :style toggle
      :selected eat-performance-mode]
     ["Cleanup Dead Buffers" eat-cleanup-dead-buffers]
     ["Performance Report" eat-performance-report])
    ("Pi Agent"
     ["Reload" pi-reload]
     ["Full Reload" pi-reload-full])))

(define-key-after global-map [menu-bar my-menu]
  (cons "My Menu" my-config-menu)
  'tools)

(provide 'my-menu)
;;; my-menu.el ends here
