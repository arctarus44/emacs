;;Theme
(load-theme 'wombat)

;; Path management
; elpa
(add-to-list 'load-path "/home/arctarus/.emacs.d/elpa/")
; Path to emhacks
(add-to-list 'load-path "/usr/share/emacs/site-lisp/emhacks")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Windowed Conf ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when window-system
  (defun toggle-fullscreen ()
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

;; Compile
(byte-recompile-directory (expand-file-name "~/.emacs.d") 0)

;; Require
(require 'package)
(require 'whitespace)
(require 'cl) ; for line-comment-banner


;; Autoload
(autoload 'line-comment-banner "line-comment-banner" nil t)
(autoload 'jedi:setup "jedi" nil t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(battery-mode-line-format " [%b%p%%]")
 '(blink-cursor-blinks 1)
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(delete-selection-mode t)
 '(desktop-save t)
 '(display-battery-mode t)
 '(display-time-24hr-format t)
 '(display-time-default-load-average nil)
 '(display-time-mode t)
 '(fill-column 80)
 '(flymake-gui-warnings-enabled nil)
 '(flymake-no-changes-timeout 0.75)
 '(flymake-start-syntax-check-on-newline nil)
 '(global-linum-mode t)
 '(global-whitespace-mode t)
 '(indent-tabs-mode t)
 '(inhibit-startup-screen t)
 '(make-pointer-invisible t)
 '(menu-bar-mode nil)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tabbar-mode t nil (tabbar))
 '(tool-bar-mode nil)
 '(whitespace-line-column 80))

;; fci-mode
(add-hook 'after-change-major-mode-hook 'fci-mode) ; use fci for every file
(setq fci-rule-width 1)
(setq fci-rule-color "orange")

;; Jedi
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)
(setq jedi:tooltip-method '(pos-tip))

;; Tabbar
(setq tabbar-buffer-list-function
	  (lambda ()
		(remove-if
		 (lambda(buffer)
		   (find (aref (buffer-name buffer) 0) " *"))
		 (buffer-list))))

(setq tabbar-buffer-groups-function
	  (lambda ()
		(list "All")))
(defadvice tabbar-buffer-tab-label
	(after fixup_tab_label_space_and_flag activate)
  (setq ad-return-value
		(if (and (buffer-modified-p (tabbar-tab-value tab))
				 (buffer-file-name (tabbar-tab-value tab)))
			(concat "+ " (concat ad-return-value ""))
		  (concat " " (concat ad-return-value " ")))))

;; Called each time the modification state of the buffer changed.
(defun ztl-modification-state-change ()
  (tabbar-set-template tabbar-current-tabset nil)
  (tabbar-display-update))

;; First-change-hook is called BEFORE the change is made.
(defun ztl-on-buffer-modification ()
  (set-buffer-modified-p t)
  (ztl-modification-state-change))
(add-hook 'after-save-hook 'ztl-modification-state-change)

;; This doesn't work for revert, I don't know.
(add-hook 'after-revert-hook 'ztl-modification-state-change)
(add-hook 'first-change-hook 'ztl-on-buffer-modification)

;; Hook
(add-hook 'after-save-hook 'ztl-modification-state-change)
(add-hook 'after-revert-hook 'ztl-modification-state-change)
(add-hook 'first-change-hook 'ztl-on-buffer-modification)

; Prog-mode
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

; Python hook
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook 'auto-complete-mode)
(add-hook 'python-mode-hook 'jedi:ac-setup)
(add-hook 'python-mode-hook
		  (lambda ()
			(setq indent-tabs-mode t)
			(setq tab-width 4)
			(setq python-indent 4)))

; C-mode hook
(add-hook 'c-mode-common-hook 'rainbow-delimiters-mode)
(add-hook 'c-mode-common-hook
			 (lambda () (make-local-variable 'comment-fill)
               (setq comment-fill "*")))
(add-hook 'c-mode-common-hook
		  '(lambda ()
			 (add-to-list 'ac-omni-completion-sources
						  (cons "\\." '(ac-source-semantic)))
			 (add-to-list 'ac-omni-completion-sources
						  (cons "->" '(ac-source-semantic)))
			 (setq ac-sources '(ac-source-semantic ac-source-yasnippet))
			 ))

(custom-set-faces
 '(default ((t (:family "Inconsolata" :foundry "unknown" :slant normal :weight normal :height 128 :width normal))))
 '(flymake-errline ((t (:inherit nil :underline (:color "red" :style wave)))))
 '(hl-line ((t (:background "gray15"))))
 '(linum ((t (:inherit (default shadow default)))))
 '(minibuffer-prompt ((t (:foreground "blue"))))
 '(mode-line ((t (:foreground "orange"))))
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
 '(scroll-bar ((t nil)))
 '(tabbar-button ((t (:inherit tabbar-default :foreground "black"))))
 '(tabbar-default ((t (:inherit variable-pitch :background "gray14" :foreground "dark gray" :height 0.8))))
 '(tabbar-highlight ((t nil)))
 '(tabbar-selected ((t (:inherit tabbar-default :foreground "orange red" :weight bold))))
 '(tabbar-separator ((t (:inherit tabbar-default :background "gray14" :height 0.2))))
 '(tabbar-unselected ((t (:inherit tabbar-default :foreground "dark magenta" :weight semi-bold))))
 '(whitespace-empty ((t nil)))
 '(whitespace-hspace ((t (:foreground "DarkOrange4"))))
 '(whitespace-indentation ((t (:foreground "gray50"))))
 '(whitespace-line ((t (:underline (:color "DarkOrange4" :style wave)))))
 '(whitespace-newline ((t (:foreground "gray25"))))
 '(whitespace-space ((t (:foreground "DarkOrange4"))))
 '(whitespace-space-after-tab ((t (:background "gray17" :foreground "DarkOrange4"))))
 '(whitespace-space-before-tab ((t (:background "gray17" :foreground "DarkOrange4"))))
 '(whitespace-tab ((t (:foreground "DarkOrange4"))))
 '(whitespace-trailing ((t (:background "gray20" :foreground "gray5")))))

;; Shortcut
(global-set-key (kbd "C-x C-s") 'delete-trailing-whitespace-and-save)
(global-set-key (kbd "C-c C-l") 'line-comment-banner)
(global-set-key (kbd "M-s M-t") 'term)
(global-set-key [f5] 'tabbar-backward-tab)
(global-set-key [f6] 'tabbar-forward-tab)
(global-set-key [f7] 'delete-window)
(global-set-key [f8] 'split-window-vertically)
(global-set-key [f9] 'split-window-horizontally)
(global-set-key [f10] 'other-window)
(global-set-key [M-up] 'move-text-up)
(global-set-key [M-down] 'move-text-down)
(global-set-key [S-f4] 'kill-this-buffer)
(global-set-key [backtab] 'auto-complete)
(global-set-key (kbd "M-s t") 'shell)
(global-set-key (kbd "M-s d") 'desktop-clear)
(add-hook 'server-switch-hook
		  (lambda ()
			(local-set-key [S-f4] 'exit-buffer)))
(global-set-key (kbd "M-s l") 'line-comment-banner)
(global-set-key (kbd "M-s b") 'comment-box)
(global-set-key (kbd "M-s j") 'downcase-region)
(global-set-key (kbd "M-s u") 'upcase-region)
(global-set-key (kbd "M-s r") 'read-only-mode)
(global-set-key (kbd "C-d") 'kill-whole-line)
(global-set-key (kbd "M-s g") 'goto-line)

;; System configuration
(fset 'yes-or-no-p 'y-or-n-p)
(prefer-coding-system 'utf-8)
(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)

;; Repository
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade.repos.org/packages/") t)

;; Function
; Delete trailling whitespace and save
(defun delete-trailing-whitespace-and-save ()
  (interactive)
  (delete-trailing-whitespace)
  (save-buffer))

(global-set-key (kbd "DEL") 'backward-delete-char)
(setq c-backspace-function 'backward-delete-char)

;; Mode config
; Python mode

; C -mode
(setq-default c-basic-offset 4
tab-width 4
indent-tabs-mode t)
