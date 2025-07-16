(set-scroll-bar-mode 'left)

;; Theme
(load-theme 'doom-pine t)

;; Fonts
;; This configuration provides a flexible way to manage fonts.
;;
;;  - F6: Decrease font size
;;  - F7: Increase font size
;;  - F8: Switch to the primary font (e.g., for coding)
;;  - F9: Switch to the secondary font (e.g., for writing prose)
;;
;; The font size is preserved when switching between families.

;; --- 1. Font Availability Check ---

(defun font-available-p (name)
  "Check if font NAME is available to Emacs."
  (not (null (x-list-fonts name))))

;; --- 2. Define Your Fonts ---

(defvar my/font-primary-family
  (cond ((font-available-p "Cascadia Mono") "Cascadia Mono")
        ((font-available-p "Noto Sans Mono") "Noto Sans Mono")
        (t "monospace"))
  "The font family to use for programming and technical work.")

(defvar my/font-secondary-family
  (cond ((font-available-p "Comic Code") "Comic Code")
        ((font-available-p "Comic Mono") "Comic Mono")
        (t "monospace"))
  "The font family to use for prose and creative writing.")

(defvar my/font-default-size 14
  "The default font size to start with (in points).")

;; --- 3. Set the Default Font on Startup ---

(set-face-attribute 'default nil
                    :family my/font-primary-family
                    :weight 'bold
                    :height (* 10 my/font-default-size))

;; --- 4. Core Font Control Functions ---

(defun my/font-current-height ()
  "Return the current font height in tenths of points."
  (face-attribute 'default :height))

(defun my/font-adjust-size (delta)
  "Adjust the font size of the current frame by DELTA points.
Preserves the current font family and weight."
  (interactive)
  (let* ((current-height (my/font-current-height))
         (new-height (max 80 (+ current-height (* 10 delta))))) ;; prevent size < 8pt
    (set-face-attribute 'default nil :height new-height)))

(defun my/font-set-family (family)
  "Set the font family for the current frame to FAMILY.
Preserves the current font size and weight."
  (interactive)
  (let ((height (face-attribute 'default :height))
        (weight (face-attribute 'default :weight)))
    (set-face-attribute 'default nil :family family :weight weight :height height)))

;; --- 5. Interactive Functions with Keybindings ---

(defun my/font-decrease-size ()
  "Decrease the current font size by 1 point."
  (interactive)
  (my/font-adjust-size -1))

(defun my/font-increase-size ()
  "Increase the current font size by 1 point."
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

;; --- 6. Assign Keys ---

(global-set-key [f6] #'my/font-decrease-size)
(global-set-key [f7] #'my/font-increase-size)
(global-set-key [f8] #'my/font-switch-to-primary)
(global-set-key [f9] #'my/font-switch-to-secondary)

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
