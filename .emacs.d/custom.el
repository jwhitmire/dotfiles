(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/auto-save-list/" t))))
 '(backup-directory-alist (quote (("." . "~/.emacs.d/backups"))))
 '(blink-cursor-mode nil)
 '(column-nmber-mode t)
 '(custom-safe-themes
   (quote
    ("756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" "3a727bdc09a7a141e58925258b6e873c65ccf393b2240c51553098ca93957723" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "2affb26fb9a1b9325f05f4233d08ccbba7ec6e0c99c64681895219f964aac7af" default)))
 '(delete-old-versions -1)
 '(display-time-mode nil)
 '(global-linum-mode t)
 '(global-pretty-lambda-mode t)
 '(global-whitespace-mode t)
 '(guide-key-mode t)
 '(guide-key/guide-key-sequence (quote ("C-x r" "C-x 4" "C-c")))
 '(helm-adaptive-mode t nil (helm-adaptive))
 '(helm-boring-file-regexp-list
   (quote
    ("\\.git$" "\\.hg$" "\\.svn$" "\\.CSV$" "\\._darcs$" "\\.la$" "\\.o$" "\\.i$")))
 '(helm-buffers-favorite-modes
   (quote
    (lisp-interaction-mode emacs-lisp-mode text-mode org-mode picture-mode artist-mode)))
 '(helm-buffers-fuzzy-matching t)
 '(helm-candidate-number-limit 200)
 '(helm-command-prefix-key "C-c h")
 '(helm-css-scss-insert-close-comment-depth 2)
 '(helm-ff-file-name-history-use-recentf t)
 '(helm-ff-search-library-in-sexp t)
 '(helm-google-suggest-use-curl-p t)
 '(helm-idle-delay 0.01)
 '(helm-match-plugin-mode t nil (helm-match-plugin))
 '(helm-mode t)
 '(helm-move-to-line-cycle-in-source t)
 '(helm-quick-update t)
 '(helm-scroll-amount 4)
 '(helm-split-window-default-side (quote other))
 '(helm-split-window-in-side-p t)
 '(history-delete-duplicates t)
 '(history-length t)
 '(ido-use-virtual-buffers t)
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message
   ";; This is your chance to scratch where it itches... in a manner of speaking
;;
")
 '(menu-bar-mode t)
 '(multi-term-buffer-name "multi-term")
 '(multi-term-dedicated-max-window-height 40)
 '(multi-term-dedicated-window-height 40)
 '(multi-term-program "/usr/local/bin/zsh")
 '(multi-term-scroll-show-maximum-output t)
 '(multi-term-scroll-to-bottom-on-output t)
 '(org-CUA-compatible nil)
 '(org-replace-disputed-keys t)
 '(ruby-align-to-stmt-keywords (quote (def if case unless while)))
 '(safe-local-variable-values
   (quote
    ((eval ignore-errors "Write-contents-functions is a buffer-local alternative to before-save-hook"
           (add-hook
            (quote write-contents-functions)
            (lambda nil
              (delete-trailing-whitespace)
              nil))
           (require
            (quote whitespace))
           "Sometimes the mode needs to be toggled off and on."
           (whitespace-mode 0)
           (whitespace-mode 1))
     (whitespace-line-column . 80)
     (whitespace-style face trailing lines-tail))))
 '(savehist-additional-variables (quote (kill-ring search-ring regexp-search-ring)))
 '(savehist-file "~/.emacs.d/savehist")
 '(savehist-mode t)
 '(savehist-save-minibuffer-history 1)
 '(scroll-bar-mode (quote right))
 '(scroll-conservatively 10000)
 '(scroll-margin 1)
 '(scroll-preserve-screen-position 1)
 '(scroll-step 1)
 '(shift-select-mode t)
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(size-indication-mode t)
 '(standard-indent 2)
 '(tab-width 2)
 '(term-buffer-maximum-size 100000)
 '(tool-bar-mode nil)
 '(track-eol t)
 '(transient-mark-mode t)
 '(truncate-lines nil)
 '(truncate-partial-width-windows nil)
 '(undo-tree-visualizer-diff t)
 '(undo-tree-visualizer-timestamps t)
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(uniquify-ignore-buffers-re "^\\*")
 '(vc-follow-symlinks t)
 '(vc-make-backup-files t)
 '(visible-bell t)
 '(whitespace-action (quote (cleanup auto-cleanup report-on-bogus)))
 '(whitespace-line-column 120)
 '(whitespace-style
   (quote
    (face trailing tabs indentation::space lines-tail space-before-tab::space space-after-tab::space tab-mark))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
