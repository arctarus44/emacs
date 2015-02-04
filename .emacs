;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-default-font "Inconsolata")
(load-theme 'wombat)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Windowed Conf ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when window-system
  (defun toggle-fullscreen ()
    (interactive)
    (set-frame-parameter nil 'fullscreen
                         (if (frame-parameter nil 'fullscreen)
                             nil
                           'fullboth)))
  (defun zoom-in ()
    (interactive)
    (text-scale-adjust +1))
  (defun zoom-out ()
    (interactive)
    (text-scale-adjust -1))
  ;; '(set-face-attribute 'default nil :font "incosolata-5")
  ;; (toggle-fullscreen)
  (toggle-frame-maximized)
  (global-set-key (kbd "M-RET") 'toggle-frame-maximized)
  (global-set-key (kbd "<C-mouse-4>") 'zoom-in)
  (global-set-key (kbd "<C-mouse-5>") 'zoom-out)
  (global-set-key (kbd "C-+") 'zoom-in)
  (global-set-key (kbd "C--") 'zoom-out)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Path ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "/home/arctarus/.emacs.d/elpa/")
(byte-recompile-directory (expand-file-name "~/.emacs.d") 0)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; System conf ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(fset 'yes-or-no-p 'y-or-n-p)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-tab-width 4)
(setq python-indent 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Init ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade.repos.org/packages/") t)
(require 'cl)
(require 'whitespace)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(battery-mode-line-format " [%b%p%%]")
 '(blink-cursor-mode 1)
 '(column-number-mode t)
 '(delete-selection-mode t)
 '(desktop-save-mode t)
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

(setq fci-rule-width 1)
(setq fci-rule-color "darkblue")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Autoload ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'line-comment-banner "line-comment-banner" nil t)
(autoload 'jedi:setup "jedi" nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; hook ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;After-init-hook
;+      o     +              o
;    +             o     +       +
;o          +
;    o  +           +        +
;+        o     o       +        o
;-_-_-_-_-_-_-_,------,      o
;_-_-_-_-_-_-_-|   /\_/\
;-_-_-_-_-_-_-~|__( ^ .^)  +     +
;_-_-_-_-_-_-_-""  ""
;+      o         o   +       o
;    +         +
;o        o         o      o     +
;    o           +
;+      +     o        o      +
;(add-hook 'after-init-hook 'nyan-mode);Nya! =^..^=


(defun aft-fun ()
  ;;   (require 'centered-cursor-mode)
  ;;   (global-centered-cursor-mode +1)
  )

(add-hook 'after-init-hook 'aft-fun)
(add-hook 'after-init-hook 'whitespace-mode)
(add-hook 'after-init-hook 'hl-line-mode)


;;Prog-mode-hook
(add-hook 'prog-mode-hook 'fci-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)


(setq-default c-basic-offset 4
			  tab-width 4
			  indent-tabs-mode t)
;;Python-mode-hook
										;Auto indentation
(add-hook 'python-mode-hook '(lambda () (define-key python-mode-map "\C-m" 'newline-and-indent)))
(add-hook 'python-mode-hook
		  (lambda ()
			(setq indent-tabs-mode t)
			(setq tab-width 4)
			(setq python-indent 4)))
(add-hook 'python-mode-hook 'jedi:setup)
(autoload 'python-mode "python-mode.el" "Python mode." t)
(setq auto-mode-alist (append '(("/*.\.py$" . python-mode)) auto-mode-alist))

;;jedi
;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:complete-on-dot t)                 ; optional

(add-hook 'c-mode-common-hook
		  '(lambda ()

			 ;; ac-omni-completion-sources is made buffer local so
			 ;; you need to add it to a mode hook to activate on
			 ;; whatever buffer you want to use it with.  This
			 ;; example uses C mode (as you probably surmised).

			 ;; auto-complete.el expects ac-omni-completion-sources to be
			 ;; a list of cons cells where each cell's car is a regex
			 ;; that describes the syntactical bits you want AutoComplete
			 ;; to be aware of. The cdr of each cell is the source that will
			 ;; supply the completion data.  The following tells autocomplete
			 ;; to begin completion when you type in a . or a ->

			 (add-to-list 'ac-omni-completion-sources
						  (cons "\\." '(ac-source-semantic)))
			 (add-to-list 'ac-omni-completion-sources
						  (cons "->" '(ac-source-semantic)))

			 ;; ac-sources was also made buffer local in new versions of
			 ;; autocomplete.  In my case, I want AutoComplete to use
			 ;; semantic and yasnippet (order matters, if reversed snippets
			 ;; will appear before semantic tag completions).

			 (setq ac-sources '(ac-source-semantic ac-source-yasnippet))
			 ))

(add-hook 'c-mode-common-hook 'rainbow-delimiters-mode)

(add-hook 'c-mode-common-hook
             (lambda () (make-local-variable 'comment-fill)
                        (setq comment-fill "*")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Mode configuration ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Fci-mode
(setq fci-rule-width 1)
(setq fci-rule-color "orange")

;;Tabbar
										;Hide all specials buffer
(setq tabbar-buffer-list-function
	  (lambda ()
		(remove-if
		 (lambda(buffer)
		   (find (aref (buffer-name buffer) 0) " *"))
		 (buffer-list))))
										;List all buffer
(setq tabbar-buffer-groups-function
	  (lambda ()
		(list "All")))
;; Add a buffer modification state indicator in the tab label, and place a
;; space around the label to make it looks less crowd.
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

;;Jedi (python autocompletion)
(setq jedi:setup-keys t)                      ; optional
(setq jedi:complete-on-dot t)                 ; optional

;;Syslog-mode
;; (require 'syslog-mode)
;; (add-to-list 'auto-mode-alist '("/var/log.*\\'" . syslog-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Function ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
										;Delete trailling whitespace and save
(defun delete-trailing-whitespace-and-save ()
  (interactive)
  (delete-trailing-whitespace)
  (save-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Face ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
										;Usefull doc : http://raebear.net/comp/emacscolors.html
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "red"))))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Key bindings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(global-set-key (kbd "M-s c") 'comment-dwim)
(global-set-key (kbd "M-s r") 'read-only-mode)
;Kill the whole line
(global-set-key (kbd "C-d") 'kill-whole-line)
(global-set-key (kbd "M-s g") 'goto-line)
;;Active la commande upcase-region
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;; Smart-tab insinuate
;;(smart-tabs-insinuate 'c 'javascript)


;; Test autocompletion
(autoload 'python-mode "python-mode.el" "Python mode." t)
(setq auto-mode-alist (append '(("/.*\.py\'" . python-mode)) auto-mode-alist))
