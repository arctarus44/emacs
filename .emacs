(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(battery-mode-line-format " [%b%p%%]")
 '(column-number-mode t)
 '(create-lockfiles nil)
 '(custom-safe-themes
   (quote
	("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(default-frame-alist (quote ((fullscreen . maximized))))
 '(display-time-24hr-format t)
 '(display-time-default-load-average nil)
 '(display-time-mode t)
 '(electric-pair-mode 1)
 '(package-selected-packages
   (quote
	(neotree restclient-test vterm docker-compose-mode impatient-mode volatile-highlights projectile docker-tramp php-mode plantuml-mode typescript-mode helm which-key pyenv-mode-auto vue-mode company-tern rjsx-mode indium xref-js2 js2-refactor js2-mode dashboard all-the-icons centaur-tabs fireplace go-eldoc flymake-google-cpplint go-autocomplete go-mode go-snippets godoctor zone-nyan idomenu imenu-list imenu+ exec-path-from-shell dockerfile-mode yaml-mode json-mode restclient smart-mode-line diff-hl langtool flycheck-pyflakes elpy py-autopep8 irony company-irony-c-headers flycheck-irony irony-eldoc ## company-irony company rainbow-identifiers aggressive-indent markdown-mode multiple-cursors undo-tree rainbow-delimiters smart-tabs-mode)))
 '(plantuml-default-exec-mode (quote jar))
 '(plantuml-indent-level 4)
 '(plantuml-jar-path "/home/passiflorae/bin/plantuml.jar"))

;; System configuration
(fset 'yes-or-no-p 'y-or-n-p)
(prefer-coding-system 'utf-8)
(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)
(delete-selection-mode 1)
(define-coding-system-alias 'UTF-8 'utf-8)

(set-face-attribute 'default nil :height 110)

;; Packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("elpy" . "https://jorgenschaefer.github.io/packages/"))

;; Path
(add-to-list 'load-path "~/.emacs.d/elisp")

(desktop-save-mode 1)



(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(package-install-selected-packages)

;; Import env var
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(load-theme 'wombat)
(require 'all-the-icons)


;; dashboard
(require 'dashboard)
(dashboard-setup-startup-hook)
  (tool-bar-mode -1) ;; disable toolbar
  (menu-bar-mode -1) ;; disable menubar

;;  Graphical stuffs
(defun gui-configuration()
  ;; Rainbow indentifier
  (add-hook 'prog-mode-hook 'rainbow-identifiers-mode)
  (tool-bar-mode -1) ;; disable toolbar
  (menu-bar-mode -1) ;; disable menubar
  (scroll-bar-mode -1) ;; disable scrollbar
  (blink-cursor-mode 0)
  (setq-default cursor-type 'bar)

  (defun zoom-in ()
    (interactive)
    (text-scale-adjust +1))

  (defun zoom-out ()
    (interactive)
    (text-scale-adjust -1))

  (global-set-key (kbd "M-RET") 'toggle-frame-maximized)
  (global-set-key (kbd "<C-mouse-4>") 'zoom-in)
  (global-set-key (kbd "<C-mouse-5>") 'zoom-out)
  (global-set-key (kbd "C-+") 'zoom-in)
  (global-set-key (kbd "C--") 'zoom-out)
  (custom-set-faces
    '(linum ((t (:inherit (default shadow default) :foreground "gray40"))))
   )
  )

;; Store Backup file in one directory
(setq backup-directory-alist '(("." . "~/.emacs.d/backupfiles")))

;; which key
(which-key-mode)
(which-key-setup-minibuffer)

;; Terminal stuffs
(defun term-configuration()
  (custom-set-faces
   '(linum ((t (:inherit (default shadow default) :foreground "yellow"))))
   )
  )

;; centaur-tabs
(require 'centaur-tabs)
(centaur-tabs-mode t)
(setq centaur-tabs-set-icons t)

(setq centaur-tabs-set-bar 'left)
(global-set-key [f5]  'centaur-tabs-backward)
(global-set-key [f6] 'centaur-tabs-forward)

(setq centaur-tabs-gray-out-icons 'buffer)
(setq centaur-tabs-set-modified-marker "*")

;; projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

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
(global-whitespace-mode nil) ;; activate

;; C mode
(setq-default c-basic-offset 4
			  tab-width 4
			  indent-tabs-mode t)
(setq c-default-style "java")

;; Smart tabs
(smart-tabs-insinuate 'c )

;; ido mode
(require 'ido)
(ido-mode t)


;; LanguageTool
(require 'langtool)
(setq langtool-language-tool-jar "~/bin/languagetool/languagetool-commandline.jar")
(setq langtool-default-language "en-GB")
(setq langtool-mother-tongue "fr")

;; Python
(elpy-enable)
(require 'py-autopep8)
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))
;; Fixing a key binding bug in elpy
(define-key yas-minor-mode-map (kbd "C-c k") 'yas-expand)
(add-hook 'python-mode-hook (lambda () (auto-complete-mode -1)))
(define-key elpy-mode-map (kbd "<C-tab>") 'elpy-company-backend)

;; pyenv
(require 'pyenv-mode-auto)
(setq exec-path (cons "/home/passiflorae/.pyenv/shims" exec-path))

(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; Golang
(require 'go-autocomplete)
(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-modes 'go-mode)

(defun my-go-mode-hook ()
  ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
)
(add-hook 'go-mode-hook 'my-go-mode-hook)
(add-hook 'go-mode-hook 'go-eldoc-setup)


;; Highlight matching brackets.
(show-paren-mode 1)
(setq show-paren-style 'parenthesis)

;; Elasticmode
(autoload 'es-mode "es-mode.el"
  "Major mode for editing Elasticsearch queries" t)
(add-to-list 'auto-mode-alist '("\\.es$" . es-mode))

;; Irony mode

;; Markdown mode
;(use-package markdown-mode
;  :ensure t
;  :commands (markdown-mode gfm-mode)
;  :mode (("README\\.md\\'" . gfm-mode)
;         ("\\.md\\'" . markdown-mode)
;         ("\\.markdown\\'" . markdown-mode))
;  :init (setq markdown-command "multimarkdown"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "deep sky blue"))))
 '(hl-line ((t (:background "gray19"))))
 '(linum ((t (:inherit (default shadow default) :foreground "gray40"))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 2.0))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.7))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.4))))
 '(markdown-header-face-4 ((t (:inherit markdown-header-face :height 1.1))))
 '(markdown-link-face ((t (:inherit link))))
 '(minimap-active-region-background ((t (:background "gray17"))))
 '(mode-line ((t (:background "#444444" :foreground "orange"))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "cyan"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "magenta"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "OrangeRed2"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "yellow"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "green"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "cyan" :weight bold))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "magenta" :weight bold))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "blue" :weight bold))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "yellow" :weight bold))))
 '(rainbow-delimiters-unmatched-face ((t (:foreground "red" :weight bold))))
 '(whitespace-empty ((t nil)))
 '(whitespace-hspace ((t (:foreground "DarkOrange4"))))
 '(whitespace-indentation ((t (:foreground "gray25"))))
 '(whitespace-line ((t (:underline (:color "DarkOrange4" :style wave)))))
 '(whitespace-newline ((t (:foreground "gray25"))))
 '(whitespace-space ((t (:foreground "gray25"))))
 '(whitespace-space-after-tab ((t (:foreground "DarkOrange4"))))
 '(whitespace-space-before-tab ((t (:background "DarkOrange4" :foreground "DarkOrange4"))))
 '(whitespace-tab ((t (:foreground "gray25"))))
 '(whitespace-trailing ((t (:foreground "DarkOrange4")))))

;; Usefull functions
										; Delete trailling whitespace and save
(defun delete-trailing-whitespace-and-save ()
  (interactive)
  (delete-trailing-whitespace)
  (save-buffer))

(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
(indent-according-to-mode))

;; Shortcut
(global-set-key (kbd "C-x C-s") 'delete-trailing-whitespace-and-save)
(global-set-key (kbd "M-s M-t") 'term)
(global-set-key [f7] 'delete-window)
(global-set-key [f8] 'split-window-vertically)
(global-set-key [f9] 'split-window-horizontally)
(global-set-key [f10] 'other-window)
(global-set-key [f11] 'other-frame)
(global-set-key [M-up] 'move-text-up)
(global-set-key [M-down] 'move-text-down)
(global-set-key [S-f4] 'kill-this-buffer)
(global-set-key (kbd "M-s t") 'shell)
(global-set-key (kbd "M-s t") 'goto-line)
(global-set-key (kbd "M-s d") 'desktop-clear)
(global-set-key (kbd "M-s c") 'comment-dwim)
(global-set-key (kbd "M-s l") 'line-comment-banner)
(global-set-key (kbd "M-s b") 'comment-box)
(global-set-key (kbd "M-s j") 'downcase-region)
(global-set-key (kbd "M-s u") 'upcase-region)
(global-set-key (kbd "M-s r") 'read-only-mode)
(global-set-key (kbd "C-d") 'kill-whole-line)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key [(meta up)]  'move-line-up)
(global-set-key [(meta down)]  'move-line-down)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(global-set-key (kbd "DEL") 'backward-delete-char)
(setq c-backspace-function 'backward-delete-char)
(global-set-key (kbd "C-x O") 'previous-multiframe-window)
(global-set-key [C-c r] 'revert-buffer)

;; fix me add this inside a python hook
;; (global-set-key (kbd "<C-tab>") 'elpy-company-backend)

;; =============
;; irony-mode
;; =============
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
;; =============
;; company mode
;; =============
(add-hook 'c++-mode-hook 'company-mode)
(add-hook 'c-mode-hook 'company-mode)
;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
(define-key irony-mode-map [remap completion-at-point]
  'irony-completion-at-point-async)
(define-key irony-mode-map [remap complete-symbol]
  'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
(eval-after-load 'company
'(add-to-list 'company-backends 'company-irony))
;; (optional) adds CC special commands to `company-begin-commands' in order to
;; trigger completion at interesting places, such as after scope operator
;;     std::|
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
;; =============
;; flycheck-mode
;; =============
(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'c-mode-hook 'flycheck-mode)
(eval-after-load 'flycheck
'(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
;; =============
;; eldoc-mode
;; =============
(add-hook 'irony-mode-hook 'irony-eldoc)
;; ==========================================
;; (optional) bind TAB for indent-or-complete
;; ==========================================
(defun irony--check-expansion ()
(save-excursion
  (if (looking-at "\\_>") t
    (backward-char 1)
    (if (looking-at "\\.") t
      (backward-char 1)
      (if (looking-at "->") t nil)))))
(defun irony--indent-or-complete ()
"Indent or Complete"
(interactive)
(cond ((and (not (use-region-p))
            (irony--check-expansion))
       (message "complete")
       (company-complete-common))
      (t
       (message "indent")
       (call-interactively 'c-indent-line-or-region))))
(defun irony-mode-keys ()
"Modify keymaps used by `irony-mode'."
(local-set-key (kbd "TAB") 'irony--indent-or-complete)
(local-set-key [tab] 'irony--indent-or-complete))
(add-hook 'c-mode-common-hook 'irony-mode-keys)




(if (display-graphic-p)
    (gui-configuration)
  (term-configuration)
  )


;; Smart mode line
(setq sml/theme 'dark)
(sml/setup)
(set-cursor-color "deepskyblue")


  (defun zone-choose (pgm)
    "Choose a PGM to run for `zone'."
    (interactive
     (list
      (completing-read
       "Program: "
       (mapcar 'symbol-name zone-programs))))
    (let ((zone-programs (list (intern pgm))))
      (zone)))


;;;;;;;;;;;;;;;;
;; javascript ;;
;;;;;;;;;;;;;;;;
(require 'js2-mode)
(require 'js2-refactor)
(require 'xref-js2)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; Better imenu
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)


(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
;; unbind it.
(define-key js-mode-map (kbd "M-.") nil)

(add-hook 'js2-mode-hook (lambda ()
						   (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
(add-to-list 'company-backends 'company-tern)

(setq company-minimum-prefix-length 1)
(setq js2-strict-missing-semi-warning nil)

;; vuejs
(setq mmm-js-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))
(setq mmm-typescript-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))

;; php
(require 'flymake-php)
(add-hook 'php-mode-hook 'flymake-php-load)
