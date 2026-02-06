(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes t)
 '(electric-indent-mode nil)
 '(grep-find-command
   "find -L . -not -path \"*.venv*\" -not -path \"*venv*\" -not -path \"build/\" -type f -print0 | xargs -0 -e grep -n -e ")
 '(make-backup-files nil)
 '(menu-bar-mode t)
 '(package-selected-packages
   '(csharp-mode csv-mode doom-themes dracula-theme eat eglot
                 gdscript-mode gruvbox-theme lua-mode magit
                 markdown-mode org-modern phscroll rg solarized-theme
                 spacemacs-theme transient typescript-mode yaml-mode
                 zenburn-theme zig-mode))
 '(package-vc-selected-packages
   '((phscroll :vc-backend Git :url
               "https://github.com/misohena/phscroll")))
 '(shell-mode-hook '(compilation-shell-minor-mode))
 '(show-paren-mode t nil (paren))
 '(tempbuf-life-extension-ratio 4)
 '(tempbuf-minimum-timeout 86400)
 '(tramp-encoding-shell "/usr/bin/bash")
 '(uniquify-buffer-name-style 'forward nil (uniquify)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
