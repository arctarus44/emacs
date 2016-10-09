;;  Graphical stuffs
(defun gui-configuration()
  (load-theme 'wombat)
  (tool-bar-mode -1) ;; disable toolbar
  (menu-bar-mode -1) ;; disable menubar
  (toggle-scroll-bar -1) ;; disable scrollbar
  (blink-cursor-mode 0)
  (setq-default cursor-type 'bar)
  (set-cursor-color "deepskyblue") 

  (defun toggle-fullscreen ()
    (defun zoom-out ()
      (interactive)
      (text-scale-adjust -1))

    (toggle-frame-maximized)
    (global-set-key (kbd "M-RET") 'toggle-frame-maximized)
    (global-set-key (kbd "<C-mouse-4>") 'zoom-in)
    (global-set-key (kbd "<C-mouse-5>") 'zoom-out)
    (global-set-key (kbd "C-+") 'zoom-in)

    (interactive)
    (set-frame-parameter nil 'fullscreen
                         (if (frame-parameter nil 'fullscreen)
                             nil 'fullboth)))

  (defun zoom-in ()
    (interactive)
    (text-scale-adjust +1))

  (defun zoom-out ()
    (interactive)
    (text-scale-adjust -1))

  (toggle-frame-maximized)
  (global-set-key (kbd "M-RET") 'toggle-frame-maximized)
  (global-set-key (kbd "<C-mouse-4>") 'zoom-in)
  (global-set-key (kbd "<C-mouse-5>") 'zoom-out)
  (global-set-key (kbd "C-+") 'zoom-in)
  (global-set-key (kbd "C--") 'zoom-out)
  )

;; Terminal stuffs
(defun term-configuration()
  )

(if (display-graphic-p)
    (gui-configuration)
  (term-configuration)
  )


;; Path
(add-to-list 'load-path "/home/arctarus/.emacs.d/elisp")

;; Packages
(package-initialize)
(require 'package)
(add-to-list 'package-archives '(("melpa" . "https://melpa.org/packages/")
				 ("gnu" . "https://elpa.gnu.org/packages/")))

;; Linum-mode
(setq linum-format "%d ")
(global-linum-mode 1)

;; highligth current line
(global-hl-line-mode +1)
(setq-default fill-column 80)

;; Fill Column Indicator
(require 'fill-column-indicator)
(add-hook 'after-change-major-mode-hook 'fci-mode) ; use fci for every file
(setq fci-rule-width 1)
(setq fci-rule-color "darkorange4")

;; Mode-line configuration
(display-battery-mode 1)

;; Rainbow delimiters
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Undo tree mode
(global-undo-tree-mode)

;; Whitespace mode
(require 'whitespace) ;; activate

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:background "gray19"))))
 '(linum ((t (:inherit (default shadow default) :foreground "gray40"))))
 '(minimap-active-region-background ((t (:background "gray17"))))
 '(mode-line ((t (:background "#444444" :foreground "orange"))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "cyan"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "magenta"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "blue"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "yellow"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "green"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "cyan" :weight bold))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "magenta" :weight bold))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "blue" :weight bold))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "yellow" :weight bold))))
 '(rainbow-delimiters-unmatched-face ((t (:foreground "red" :weight bold))))
 '(whitespace-empty ((t nil)))
 '(whitespace-hspace ((t (:foreground "DarkOrange4"))))
 '(whitespace-indentation ((t (:foreground "gray50"))))
 '(whitespace-line ((t (:underline (:color "DarkOrange4" :style wave)))))
 '(whitespace-newline ((t (:foreground "gray25"))))
 '(whitespace-space ((t (:foreground "DarkOrange4"))))
 '(whitespace-space-after-tab ((t (:background "gray14" :foreground "DarkOrange4"))))
 '(whitespace-space-before-tab ((t (:background "gray17" :foreground "DarkOrange4"))))
 '(whitespace-tab ((t (:foreground "DarkOrange4"))))
 '(whitespace-trailing ((t (:foreground "DarkOrange4")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(battery-mode-line-format " [%b%p%%]")
 '(column-number-mode t)
 '(display-time-24hr-format t)
 '(display-time-default-load-average nil)
 '(display-time-mode t)
 '(package-selected-packages (quote (undo-tree minimap rainbow-delimiters))))

;; Usefull functions
; Delete trailling whitespace and save
(defun delete-trailing-whitespace-and-save ()
  (interactive)
  (delete-trailing-whitespace)
(save-buffer))

;; Shortcut
(global-set-key (kbd "C-x C-s") 'delete-trailing-whitespace-and-save)
(global-set-key (kbd "M-s M-t") 'term)
(global-set-key [f5] 'tabbar-backward-tab)
(global-set-key [f6] 'tabbar-forward-tab)
(global-set-key [f7] 'delete-window)
(global-set-key [f8] 'split-window-vertically)
(global-set-key [f9] 'split-window-horizontally)
(global-set-key [f10] 'other-window)
(global-set-key [f11] 'other-frame)
(global-set-key [M-up] 'move-text-up)
(global-set-key [M-down] 'move-text-down)
(global-set-key [S-f4] 'kill-this-buffer)
(global-set-key [backtab] 'auto-complete)
(global-set-key (kbd "M-s t") 'shell)
(global-set-key (kbd "M-s d") 'desktop-clear)
(global-set-key (kbd "M-s c") 'comment-dwim)
(global-set-key (kbd "M-s l") 'line-comment-banner)
(global-set-key (kbd "M-s b") 'comment-box)
(global-set-key (kbd "M-s j") 'downcase-region)
(global-set-key (kbd "M-s u") 'upcase-region)
(global-set-key (kbd "M-s r") 'read-only-mode)
(global-set-key (kbd "C-d") 'kill-whole-line)
(global-set-key (kbd "M-s g") 'goto-line)
(global-set-key [(meta up)]  'move-line-up)
(global-set-key [(meta down)]  'move-line-down)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
