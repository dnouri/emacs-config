;;; pi-config.el --- Pi coding agent integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Load pi-coding-agent from development directory with reload helpers.

;;; Code:

;; Ensure mise shims are in PATH for the pi binary
(let ((mise-shims (expand-file-name "~/.local/share/mise/shims")))
  (unless (member mise-shims exec-path)
    (add-to-list 'exec-path mise-shims)
    (setenv "PATH" (concat mise-shims ":" (getenv "PATH")))))

(defvar pi-config-source-dir (expand-file-name "~/co/pi-coding-agent")
  "Directory containing the pi-coding-agent source.")

(add-to-list 'load-path pi-config-source-dir)
(require 'pi-coding-agent)
(defalias 'pi 'pi-coding-agent)

(defun pi-reload ()
  "Reload pi-coding-agent from development directory."
  (interactive)
  (load (expand-file-name "pi-coding-agent-core.el" pi-config-source-dir) nil t)
  (load (expand-file-name "pi-coding-agent.el" pi-config-source-dir) nil t)
  (message "pi-coding-agent reloaded"))

(defun pi-reload-full ()
  "Fully unload and reload pi-coding-agent, resetting defcustoms."
  (interactive)
  (when (featurep 'pi-coding-agent)
    (unload-feature 'pi-coding-agent t))
  (when (featurep 'pi-coding-agent-core)
    (unload-feature 'pi-coding-agent-core t))
  (require 'pi-coding-agent)
  (message "pi-coding-agent fully reloaded"))

(provide 'pi-config)
;;; pi-config.el ends here
