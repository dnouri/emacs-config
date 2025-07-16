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
  (set-frame-font "Cascadia Mono-14"))
(defun large-fonts1 ()
  "Large fonts"
  (interactive)
  (set-frame-font "Cascadia Mono-16"))
(defun super-large-fonts1 ()
  "Super large fonts"
  (interactive)
  (set-frame-font "Cascadia Mono-22"))

(global-set-key [M-f7] 'medium-fonts1)
(global-set-key [M-f8] 'large-fonts1)
(global-set-key [M-f9] 'super-large-fonts1)

(defun medium-fonts2 ()
  "Medium fonts"
  (interactive)
  (set-frame-font "Comic Code Daniel-14"))
(defun large-fonts2 ()
  "Large fonts"
  (interactive)
  (set-frame-font "Comic Code Daniel-16"))
(defun super-large-fonts2 ()
  "Super large fonts"
  (interactive)
  (set-frame-font "Comic Code Daniel-22"))

(global-set-key [f7] 'medium-fonts2)
(global-set-key [f8] 'large-fonts2)
(global-set-key [f9] 'super-large-fonts2)

(defun medium-fonts2 ()
  "Medium fonts"
  (interactive)
  (set-frame-font "Comic Code Daniel-14"))
(defun large-fonts2 ()
  "Large fonts"
  (interactive)
  (set-frame-font "Comic Code Daniel-16"))
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

(if (file-exists-p "~/.emacs.d/mu4e.el")
    (load "~/.emacs.d/mu4e"))
