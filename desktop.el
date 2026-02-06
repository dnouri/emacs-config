;;; desktop.el --- GUI display settings -*- lexical-binding: t; -*-

;;; Commentary:
;; Loaded only in graphical mode.  Configures frame appearance,
;; fonts, theme, and display-specific keybindings.

;;; Code:

;; Frame
(tool-bar-mode -1)
(setq frame-title-format "%b - Emacs")
(set-scroll-bar-mode 'left)

;; Theme
(load-theme 'leuven-dark t)

;; Fonts — F6/F7 adjust size, F8/F9 switch family
(defun font-available-p (name)
  "Check if font NAME is available to Emacs."
  (not (null (x-list-fonts name))))

(defvar my/font-primary-family
  (cond ((font-available-p "Cascadia Mono") "Cascadia Mono")
        ((font-available-p "Noto Sans Mono") "Noto Sans Mono")
        (t "monospace"))
  "Font family for programming and technical work.")

(defvar my/font-secondary-family
  (cond ((font-available-p "Comic Code") "Comic Code")
        ((font-available-p "Comic Mono") "Comic Mono")
        (t "monospace"))
  "Font family for prose and creative writing.")

(defvar my/font-default-size 14
  "Default font size in points.")

(set-face-attribute 'default nil
                    :family my/font-primary-family
                    :weight 'bold
                    :height (* 10 my/font-default-size))

(defun my/font-current-height ()
  "Return the current font height in tenths of points."
  (face-attribute 'default :height))

(defun my/font-adjust-size (delta)
  "Adjust font size by DELTA points, minimum 8pt."
  (interactive)
  (let* ((current-height (my/font-current-height))
         (new-height (max 80 (+ current-height (* 10 delta)))))
    (set-face-attribute 'default nil :height new-height)))

(defun my/font-set-family (family)
  "Set font FAMILY, preserving current size and weight."
  (interactive)
  (let ((height (face-attribute 'default :height))
        (weight (face-attribute 'default :weight)))
    (set-face-attribute 'default nil :family family :weight weight :height height)))

(defun my/font-decrease-size ()
  "Decrease font size by 1 point."
  (interactive)
  (my/font-adjust-size -1))

(defun my/font-increase-size ()
  "Increase font size by 1 point."
  (interactive)
  (my/font-adjust-size +1))

(defun my/font-switch-to-primary ()
  "Switch to the primary font family."
  (interactive)
  (my/font-set-family my/font-primary-family))

(defun my/font-switch-to-secondary ()
  "Switch to the secondary font family."
  (interactive)
  (my/font-set-family my/font-secondary-family))

(global-set-key [f6] #'my/font-decrease-size)
(global-set-key [f7] #'my/font-increase-size)
(global-set-key [f8] #'my/font-switch-to-primary)
(global-set-key [f9] #'my/font-switch-to-secondary)

;; Shell — better ANSI color contrast
(setq ansi-color-names-vector
      ["black" "red4" "green4" "yellow4"
       "blue3" "magenta4" "cyan4" "white"])
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Highlight current line, adapting to the active theme.
;; Slightly lighten for dark themes, slightly darken for light themes.
;; Keep foreground nil so syntax highlighting is preserved.
(global-hl-line-mode 1)
(defun my/set-hl-line-color ()
  "Set hl-line background as a subtle shift from the theme's default.
Lightens by 20% for dark themes, darkens by 20% for light themes."
  (require 'color)
  (let ((bg (face-attribute 'default :background nil 'default)))
    (when (and bg (not (eq bg 'unspecified)))
      (let ((adjusted (if (eq (frame-parameter nil 'background-mode) 'dark)
                          (color-lighten-name bg 20)
                        (color-darken-name bg 20))))
        (set-face-attribute 'hl-line nil
                            :background adjusted
                            :foreground nil)))))
(add-hook 'after-load-theme-hook #'my/set-hl-line-color)
(my/set-hl-line-color)

;; Dynamic abbreviation expansion
(global-set-key (kbd "C-<tab>") 'dabbrev-expand)
(define-key minibuffer-local-map (kbd "C-<tab>") 'dabbrev-expand)

;; Email (private, not tracked in git)
(if (file-exists-p "~/.emacs.d/mu4e.el")
    (load "~/.emacs.d/mu4e"))

;;; desktop.el ends here
