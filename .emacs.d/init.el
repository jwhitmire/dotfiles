;;
;; Initialize packages via Cask/pallet
;;
(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)

;;
;; setup basic info
;;
(defvar HOME (expand-file-name "~"))
(setq HOSTNAME (system-name))
(setq USERNAME (getenv "USER"))

(setq debug-on-error t)

;;(setq user-full-name "Jeff Whitmire"
;;      user-mail-address "jeff.whitmire@phishme.com")

;;
;; setup load path/directories
;;
(setq dotfiles-dir (file-name-directory
        (or (buffer-file-name) load-file-name)))

(setq init-dir (expand-file-name "init" dotfiles-dir)
      my-site-lisp-dir (expand-file-name "site-lisp" dotfiles-dir)
      custom-file (expand-file-name "custom.el" dotfiles-dir))

(add-to-list 'load-path init-dir)
(add-to-list 'load-path my-site-lisp-dir)

;;
;; init loading helper functions
;;
(defun require-init (feature-name)
  (let ((feature (intern (format "%s" feature-name))))
    (message (format ">>> Loading %s..." feature))
    (require feature)
    (message (format ">>> Finished loading %s" feature))))


;; Check if system is Darwin/Mac OS X
(defun system-type-is-darwin ()
  (interactive)
  "Return true if system is darwin-based (Mac OS X)"
  (string-equal system-type "darwin"))

;; Check if system is GNU/Linux
(defun system-type-is-gnu ()
  (interactive)
  "Return true if system is GNU/Linux-based"
  (string-equal system-type "gnu/linux"))

;; Check for triage VM
(defun system-is-triage-vm ()
  (interactive)
  "Return true if system is a triage virtual machine"
  (string-equal system-name "triage.vm"))

;;
;; OSX specific settings
;;
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'super
      ns-function-modifier 'hyper
      x-select-enable-clipboard t)

(require 'exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;;
;; whitespace configuration
;;
(require 'whitespace)
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Load these instead of autoloading since they will get used
;; by almost everything
(require 'cl)
(require 'saveplace)
(require 'ffap)
(require 'ansi-color)
(require 'recentf)
(require 'uniquify)
(require 'redo+)

;; Lazy people like me never want to type "yes" when "y" will suffice.
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'auto-tail-revert-mode 'tail-mode)

(windmove-default-keybindings 'hyper)

;; Instruct Emacs to use emacs term-info not system term info
;; http://stackoverflow.com/questions/8918910/weird-character-zsh-in-emacs-terminal
(setq system-uses-terminfo nil)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(random t)
(pending-delete-mode t)

;;
;; load emacs customizations
;;
(load custom-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; term setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (require 'multi-term nil t)
  (global-set-key (kbd "<f5>") 'multi-term)
  (global-set-key (kbd "C-c t") 'multi-term-next)
  (global-set-key (kbd "C-c T") 'multi-term-prev)
  (setq multi-term-buffer-name "term"
        multi-term-program "/usr/local/bin/zsh"))

(when (require 'term nil t) ; only if term can be loaded
  (defun term-handle-ansi-terminal-messages (message)
    (while (string-match "\eAnSiT.+\n" message)
      ;; Extract the command code and the argument
      (let* ((start (match-beginning 0))
       (command-code (aref message (+ start 6)))
       (argument
        (save-match-data
    (substring message
         (+ start 8)
         (string-match "\r?\n" message
           (+ start 8))))))
  ;; Delete command from MESSAGE
  (setq message (replace-match "" t t message))

  (cond ((= command-code ?c)
         (setq term-ansi-at-dir argument))
        ((= command-code ?h)
         (setq term-ansi-at-host argument))
        ((= command-code ?u)
         (setq term-ansi-at-user argument))
        ((= command-code ?e)
         (save-excursion
     (find-file-other-window argument)))
        ((= command-code ?x)
         (save-excursion
     (find-file argument))))))

    (when (and term-ansi-at-host term-ansi-at-dir term-ansi-at-user)
      (setq buffer-file-name
      (format "%s@%s:%s" term-ansi-at-user term-ansi-at-host term-ansi-at-dir))
      (set-buffer-modified-p nil)
      (setq default-directory (if (string= term-ansi-at-host (system-name))
          (concatenate 'string term-ansi-at-dir "/")
        (format "/%s@%s:%s/" term-ansi-at-user term-ansi-at-host term-ansi-at-dir))))
    message)

  (setq term-bind-key-alist
        (list (cons "C-c C-c" 'term-interrupt-subjob)
              (cons "C-p" 'previous-line)
              (cons "C-n" 'next-line)
              (cons "M-f" 'term-send-forward-word)
              (cons "M-b" 'term-send-backward-word)
              (cons "M-o" 'term-send-backspace)
              (cons "C-c C-j" 'term-line-mode)
              (cons "C-c C-k" 'term-char-mode)
              (cons "M-DEL" 'term-send-backward-kill-word)
              (cons "M-d" 'term-send-forward-kill-word)
              (cons "M-b" 'term-send-backward-word)
              (cons "M-f" 'term-send-forward-word)
              (cons "C-s" 'isearch-forward)
              (cons "C-r" 'iserach-backward)
              (cons "C-m" 'term-send-raw)
              (cons "M-p" 'term-send-raw-meta)
              (cons "M-y" 'term-send-raw-meta))))

(add-hook 'term-mode-hook
    (lambda ()
      (setq term-buffer-maximum-size 10000
            autopair-dont-activate t
            autopair-mode -1
            show-trailing-whitespace nil)
      (define-key term-raw-map (kbd "C-y") 'term-paste)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-complete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ac-config-default)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")

(setq ac-ignore-case nil)
(add-to-list 'ac-modes 'ruby-mode)
(add-to-list 'ac-modes 'web-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flyspell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq flyspell-issue-message-flg nil)
(add-hook 'ruby-mode
    (lambda () (flyspell-prog-mode)))

(add-hook 'web-mode-hook
    (lambda () (flyspell-prog-mode)))

;; flyspell breaks auto-complete without this
(ac-flyspell-workaround)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helm setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'helm-config)
(require 'helm-css-scss)
(require 'helm-eshell)
(require 'helm-files)
(require 'helm-grep)
(require 'helm-themes)

(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-:") 'ac-complete-with-helm)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") 'helm-select-action)

(define-key helm-grep-mode-map (kbd "<return") 'helm-grep-mode-jump-other-window)
(define-key helm-grep-mode-map (kbd "n") 'helm-grep-mode-jump-other-window-forward)
(define-key helm-grep-mode-map (kbd "p") 'helm-grep-mode-jump-other-window-backward)

(define-key helm-css-scss-map (kbd "s-i") 'helm-css-scss-multi-from-helm-css-scss)
(define-key isearch-mode-map (kbd "s-i") 'helm-css-scss-from-isearch)
(define-key ac-complete-mode-map (kbd "C-:") 'ac-complete-with-helm)

(add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)
(add-hook 'eshell-mode-hook
    #'(lambda ()
        (define-key eshell-mode-map (kbd "M-l") 'helm-eshell-history)))

(dolist ($hook '(css-mode-hook scss-mode-hook less-css-mode-hook))
  (add-hook
   $hook (lambda ()
     (local-set-key (kbd "s-i") 'helm-css-scss)
     (local-set-key (kbd "s-I") 'helm-css-scss-back-to-last-point))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; my custom stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; stolen from http://tuxicity.se/emacs/elisp/2010/03/11/duplicate-current-line-or-region-in-emacs.html
(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated.  However, if
there's a region all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
  (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
  (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
  (goto-char end)
  (newline)
  (insert region)
  (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))
(global-set-key (kbd "C-D") 'duplicate-current-line-or-region)

(defun join-next-line ()
  "Join the current line with the line beneath it."
  (interactive)
  (delete-indentation 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; helm
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
;;(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
(global-set-key (kbd "C-c h x") 'helm-register)
(global-set-key (kbd "C-c h g") 'helm-google-suggest)
(global-set-key (kbd "C-c h M-:") 'helm-eval-expression-with-eldoc)

;; isearch
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "\C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

(global-unset-key (kbd "C-y"))
(global-unset-key (kbd "C-w"))
(global-unset-key (kbd "M-w"))
(global-set-key (kbd "M-c") 'kill-ring-save)
(global-set-key (kbd "M-C") 'kill-region)
(global-set-key (kbd "M-v") 'yank)

(global-set-key (kbd "M-z") 'undo)
(global-set-key (kbd "M-Z") 'redo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; development related customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-pretty-lambda-mode t)

(require 'smartparens-config)
(require 'smartparens-ruby)
(smartparens-global-mode)
(show-smartparens-global-mode t)

(defun sp-web-mode-is-code-context (id action context)
  (when (and (eq action 'insert)
       (not (or (get-text-property (point) 'part-side)
          (get-text-property (point) 'block-side))))
    t))
(sp-local-pair 'web-mode "<" nil :when '(sp-web-mode-is-code-context))

(sp-with-modes '(rhtml-mode)
  (sp-local-pair "<" ">")
  (sp-local-pair "<%" "%>"))

(setq ag-highlight-search t)

(add-hook 'projectile-mode-hook 'projectile-rails-on)

(require 'grizzl)
(projectile-global-mode)
(setq projectile-rails-expand-snippet nil
      projectile-enable-caching t
      projectile-completion-system 'grizzl)

;; Cmd-p for finding in project
(global-set-key (kbd "s-p") 'projectile-find-file)
;; Cmd-b for fuzzy switch buffer
(global-set-key (kbd "s-b") 'projectile-switch-to-buffer)

;; Git mode
(eval-after-load 'magit
  (progn '(global-set-key (kbd "M-C-g") 'magit-status)))

(global-set-key (kbd "C-x g") 'magit-status)

(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)
(global-set-key (kbd "M-j") 'join-next-line)
(global-set-key (kbd "C-M-j") 'join-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lisp mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
                   '(add-to-list 'ac-modes 'slime-repl-mode))

;;(setq inferior-lisp-program "/usr/local/bin/sbcl")
(setq slime-contribs '(slime-fancy))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ruby mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(chruby "ruby-2.1.2")

(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)
(add-hook 'after-init-hook 'inf-ruby-switch-setup)

(define-key inf-ruby-minor-mode-map (kbd "C-c C-z") 'run-ruby)
(when (executable-find "pry")
  (add-to-list 'inf-ruby-implementations '("pry" . "pry"))
  (setq inf-ruby-default-implementation "pry"))

;; .rb, .gemspec, and Rakefile already added by autoloads
(add-to-list 'auto-mode-alist '("\\.(?:rake\\|ru\\|gemspec\\|thor\\|jbuilder)$" . ruby-mode))
(add-to-list 'auto-mode-alist '("(?:Gem\|Guard\\|Cap\\|Thor\\|Vagrant)file(?:\\.lock)?$" . ruby-mode))

(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

(defun* get-closest-gemfile-root (&optional (file "Gemfile"))
  "Determine the pathname of the first instance of FILE starting from the current directory
towards root. This may not do the correct thing in the presence of links. If it does not find
FILE, then it shall return the [sic] of FILE in the current directory, suitable for creation"
  (let ((root (expand-file-name "/")))
    (loop
     for d = default-directory then (expand-file-name ".." d)
     if (file-exists-p (expand-file-name file d))
     return d
     if (equal d root)
     return nil)))

(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'robe-mode-hook 'ac-robe-setup)

(add-hook 'ruby-mode 'ruby-tools-mode)
(add-hook 'ruby-mode 'ruby-refactor-mode-launch)
(add-hook 'ruby-mode 'turn-on-ruby-dev)

(eval-after-load 'rspec-mode
  '(rspec-install-snippets))

(defadvice rspec-compile (around rspec-compile-around)
  "Use BASH shell for running specs due to ZSH issues."
  (let ((shell-file-name "/usr/local/bin/bash")
        (default-directory (projectile-rails-root)))
    (message "------------- DEBUG ------------------")
    (message "default-directory: %s" default-directory)
    (message "projectile-rails-root: %s" (projectile-rails-root))
    (message "------------- DEBUG ------------------")
    ad-do-it))

(ad-activate 'rspec-compile)

(defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
  (rvm-activate-corresponding-ruby))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; web-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))

(defun jw-web-mode-hook ()
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-comment-style 2
        web-mode-style-padding 1
        web-mode-script-padding 1
        web-mode-block-padding 0
        web-mode-enable-auto-pairing t
        web-mode-enable-css-colorization t
        web-mode-enable-comment-keywords t
        web-mode-enable-heredoc-fontification t
        web-mode-enable-current-element-highlight t)
  (local-set-key (kbd "RET") 'newline-and-indent))
(add-hook 'web-mode-hook 'jw-web-mode-hook)

(define-key web-mode-map (kbd "C-n") 'web-mode-tag-match)

(setq web-mode-extra-snippets
      '(("erb" . (("name" . ("beg" . "end"))))
        ("php" . (("name" . ("beg" . "end"))
                  ("name" . ("beg" . "end"))))))

(setq web-mode-extra-auto-pairs
      '(("erb" . (("open" "close")))
        ("php" . (("open" "close")
                  ("open" "close")))))

(setq web-mode-ac-sources-alist
      '(("css" . (ac-source-css-property))
        ("html" . (ac-source-words-in-buffer ac-source-abbrev))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; snippets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'yasnippet)
(yas-load-directory (concat dotfiles-dir "snippets"))
(yas-global-mode 1)

;; dash documentation
(global-set-key (kbd "C-c d") 'dash-at-point)
(global-set-key (kbd "C-c D") 'helm-dash-at-point)
(global-set-key (kbd "C-h D") 'helm-dash)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; theme/window setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initial theme/font/etc
(load-theme 'calmer-forest t)

;; https://www.mozilla.org/en-US/styleguide/products/firefox-os/typeface/#download-primary
(set-frame-font "Fira Mono OT-12" nil t)

; enable syntax highlighting for dash functions
(eval-after-load "dash" '(dash-enable-font-lock))

(add-hook 'dired-mode-hook 'turn-on-stripe-buffer-mode)
(add-hook 'org-mode-hook 'turn-on-stripe-table-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; highlight stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(hes-mode)
(require 'highlight-indentation)
(add-hook 'ruby-mode-hook
    (lambda () (highlight-indentation-current-column-mode)))
(add-hook 'coffee-mode-hook
    (lambda () (highlight-indentation-current-column-mode)))

;;(global-rainbow-delimiters-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mode-line setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(sml/setup)
(sml/apply-theme 'dark)

(add-to-list 'sml/replacer-regexp-list '("^~/work/vm/triage" ":Triage:") t)
(add-to-list 'sml/replacer-regexp-list '("^~/work/vm/" ":VM:") t)

(require 'octicons)
(make-face 'octicons-mode-line)
(set-face-attribute 'octicons-mode-line nil
        :inherit 'mode-line
        :inherit 'octicons)
