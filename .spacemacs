;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     python
     (version-control :variables
                      version-control-diff-tool 'diff-hl
                      version-control-global-margin t)

     sql
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     ;; helm
     (auto-completion :variables
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-sort-by-usage t)
     chrome
     common-lisp
     dash
     emacs-lisp
     erlang
     elixir
     git
     github
     (html :variables
           css-indent-offset 2
           web-mode-markup-indent-offset 2
           web-mode-css-indent-offset 2
           web-mode-code-indent-offset 2)
     (javascript :variables
                 js2-basic-offset 2
                 js-indent-level 2
                 javascript-indent-level 2)
     markdown

     (org :variables
          org-enable-github-support t
          org-enable-bootstrap-support t
          org-enable-reveal-js-support t
          org-projectile-file "TODO.org")
     pandoc


     osx
     (restclient :variables
                 restclient-use-org t)
     (ruby :variables
           ruby-version-manager 'rbenv
           ruby-test-runner 'rspec)
     ruby-on-rails
     (shell :variables
            shell-default-shell 'multi-term
            shell-default-height 30
            shell-default-position 'bottom
            shell-default-term-shell "/usr/local/bin/zsh")
     spell-checking
     shell-scripts
     syntax-checking
     yaml

     (geolocation :variables
                  geolocation-enable-location-services t
                  geolocation-enable-weather-forecast t)

     (mu4e :variables
           mu4e-account-alist t)
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(magithub)
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'emacs
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'random
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 5))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-light
                         spacemacs-dark
                         solarized-light
                         solarized-dark
                         misterioso)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 14
                               :weight normal
                               :width normal
                               :powerline-scale 1.5)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers 't
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis 'all
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."

  (let ((gls "/usr/local/bin/gls"))
    (if (file-exists-p gls) (setq insert-directory-program gls)))
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

  ;; setup magithub
  (use-package magithub
    :after magit
    :config (magithub-feature-autoinject t))

  ;; Change font size
  (define-key global-map (kbd "C-+") 'text-scale-increase)
  (define-key global-map (kbd "C--") 'text-scale-decrease)

  ;; rework copy/paste functions
  (global-unset-key (kbd "C-y"))
  (global-unset-key (kbd "C-w"))
  (global-unset-key (kbd "M-w"))
  (global-set-key (kbd "M-c") 'kill-ring-save)
  (global-set-key (kbd "M-C") 'kill-region)
  (global-set-key (kbd "M-v") 'yank)

  ;; turn on GUI elements
  (menu-bar-mode t)
  (scroll-bar-mode t)
  (tooltip-mode t)

  ;; setup linenum to work well with gutter fringe
  (progn
    (when (eq dotspacemacs-line-numbers 'relative)
      (add-hook 'prog-mode-hook 'linum-relative-mode)
      (add-hook 'text-mode-hook 'linum-relative-mode))
    (setq-default lineum-format "%4d \u2502"
                  lineum-relative-format "%4s \u2502"))
  (progn
    ;; gutter faces
    (set-face-attribute 'git-gutter:added nil :background nil :foreground "green")
    (set-face-attribute 'git-gutter:deleted nil :background nil :foreground "red")
    (set-face-attribute 'git-gutter:modified nil :background nil :foreground "blue")

    (setq-default git-gutter:modified-sign "!"))


  ;; Adjust the window margin as the scale changes
  (defun linum-update-window-scale-fix (win)
    "fix linum for scaled text"
    (set-window-margins win
                        (ceiling (* (if (boundp 'text-scale-mode-step)
                                        (expt text-scale-mode-step
                                              text-scale-mode-amount) 1)
                                    (if (car (window-margins))
                                        (car (window-margins)) 1)
                                    ))))
  (advice-add #'linum-update-window :after #'linum-update-window-scale-fix)

  ;; highlight trailing whitespace
  (setq show-trailing-whitespace t)

  ;; delete selected text when typing
  (delete-selection-mode t)
  ;; turn on whitespace cleanup
  (add-hook 'before-save-hook 'whitespace-cleanup)

  ;; Lazy people like 'y' over 'yes'
  (defalias 'yes-or-no-p 'y-or-n-p)

  ;; org mode customizations.  This is necessary so the ELPA org doesn't conflict with the one shipped with emacs
  (with-eval-after-load 'org
    (lambda()
      (require 'ob-emacs-lisp)
      (require 'ob-sql)
      (require 'ob-shell)
      (require 'ob-org)
      (require 'ob-css)
      (require 'ob-js)
      (require 'ob-http)
      (setq org-export-babel-evaluate nil)
      (setq org-startup-indented 1)
      ;; increase imenu depth to third level headings
      (setq org-imenu-depth 3)
      ;; set sensible default for dot files
      (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))
      ;; update images from code blocks
      (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
      (setq org-src-fontify-natively t)
      (setq org-src-tab-acts-natively t)
      (setq org-confirm-bable-evaluate nil)
      ))
  (with-eval-after-load 'org-agenda
    (require 'org-projectile)
    (push (org-projectile:todo-files) org-agenda-files)
    )

  ;; geolocation configuration
  (setq sunshine-appid "e5ffad27455174a25985049d0919e6d7"
        sunshine-location "14580,USA"
        sunshine-show-icons t)

  ;; mu4e configuration
  (setq mu4e-installation-path "/usr/share/emacs/site-lisp"
        mu4e-update-interval 180
        mu4e-maildir "~/.Mail"
        mu4e-drafts-filder "/[Gmail].Drafts"
        mu4e-sent-folder "/[Gmail].Sent Mail"
        mu4e-attachment-dir "~/Downloads"
        mu4e-sent-messages-behavior 'delete
        mu4e-get-mail-command "offlineimap")

  (setq mu4e-view-show-images t
        mu4e-compose-signature-auto-include 't
        message-citation-line-format "%N @ %Y-%m-%d %H:%M %Z:\n"
        message-citation-line-function 'message-insert-formatted-citation-line
        mu4e-view-show-addresses 't
        mu4e-view-prefer-html t
        mail-user-agent 'mu4e-user-agent)

  (setq mu4e-headers-fields
        '( (:date . 25)
           (:flags . 6)
           (:from . 22)
           (:subject . nil)))

  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-stream-type 'starttls)

  (setq mu4e-account-alist
        '(("StitchFix"
           (mu4e-sent-messages-behavior delete)
           (user-mail-address "jeff.whitmire@stitchfix.com")
           (user-full-name "Jeff Whitmire")
           (mu4e-compose-signature "Principal Engineer -- Styling")
           (smtpmail-stream-type starttls)
           (smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil)))
           (smtpmail-auth-credentials '(("smtp.gmail.com" 587 "jeff.whitmire@stitchfix.com" nil)))
           (smtpmail-default-smtp-server "smtp.gmail.com")
           (smtpmail-smtp-server "smtp.gmail.com")
           (smtpmail-smtp-service 587))
          ("Personal"
           (mu4e-sent-messages-behavior delete)
           (user-mail-address "jeff@jwhitmire.com")
           (user-full-name "Jeff Whitmire")
           (mu4e-compose-signature "Jeff.")
           (smtpmail-stream-type starttls)
           (smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil)))
           (smtpmail-auth-credentials '(("smtp.gmail.com" 587 "jeff@jwhitmire.com" nil)))
           (smtpmail-default-smtp-server "smtp.gmail.com")
           (smtpmail-smtp-server "smtp.gmail.com")
           (smtpmail-smtp-service 587))))
  (mu4e/mail-account-reset)

  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" default)))
 '(evil-want-Y-yank-to-eol nil)
 '(gud-gdb-command-name "gdb --annotate=1")
 '(large-file-warning-threshold nil)
 '(org-todo-keywords
   (quote
    ((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "DONE(d)" "CANCELED(c)"))))
 '(package-selected-packages
   (quote
    (mu4e-maildirs-extension mu4e-alert yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode helm-pydoc cython-mode company-anaconda anaconda-mode pythonic theme-changer sunshine rase osx-location magit-gh-pulls gmail-message-mode ham-mode html-to-markdown github-search github-clone github-browse-file gist gh marshal logito pcache flymd edit-server erlang winum restclient-helm ob-restclient fuzzy flycheck-credo company-restclient know-your-http-well enh-ruby-mode pandoc-mode ox-pandoc ht ox-twbs ox-reveal ox-gfm ob-elixir flycheck-mix alchemist elixir-mode magithub sql-indent yaml-mode xterm-color web-mode web-beautify tagedit smeargle slime-company slime slim-mode shell-pop scss-mode sass-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe reveal-in-osx-finder restclient rbenv pug-mode projectile-rails rake inflections pbcopy osx-trash osx-dictionary orgit org-projectile org-present org org-pomodoro alert log4e gntp org-download ob-http multi-term mmm-mode minitest markdown-toc markdown-mode magit-gitflow livid-mode skewer-mode simple-httpd less-css-mode launchctl json-mode json-snatcher json-reformat js2-refactor multiple-cursors js2-mode js-doc insert-shebang htmlize helm-gitignore helm-dash helm-css-scss helm-company helm-c-yasnippet haml-mode gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter gh-md flyspell-correct-helm flyspell-correct flycheck-pos-tip flycheck fish-mode feature-mode evil-magit magit magit-popup git-commit with-editor eshell-z eshell-prompt-extras esh-help emmet-mode diff-hl dash-at-point company-web web-completion-data company-tern dash-functional tern company-statistics company-shell company-quickhelp pos-tip company common-lisp-snippets coffee-mode chruby bundler inf-ruby auto-yasnippet yasnippet auto-dictionary ac-ispell auto-complete ws-butler window-numbering which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint info+ indent-guide ido-vertical-mode hydra hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make projectile pkg-info epl helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight elisp-slime-nav dumb-jump f s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed dash aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core popup async quelpa package-build spacemacs-theme)))
 '(scss-sass-command "/Users/jeffwhitmire/.rbenv/shims/sass")
 '(table-html-th-rows 1))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-block ((t (:background "light blue" :foreground "#655370"))))
 '(org-block-begin-line ((t (:background "#ddd8eb" :foreground "#9380b2"))))
 '(org-document-title ((t (:inherit bold :foreground "#6c3163" :underline nil :height 1.2))))
 '(org-drawer ((t (:foreground "Blue1"))))
 '(org-level-1 ((t (:inherit bold :foreground "#3a81c3" :height 1.1))))
 '(org-level-2 ((t (:inherit bold :foreground "#2d9574" :height 1.0))))
 '(org-level-3 ((t (:foreground "#67b11d" :weight normal))))
 '(org-meta-line ((t (:foreground "#da8b55" :slant italic))))
 '(org-quote ((t (:inherit org-block :slant italic)))))
