;; Define the init file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
      (load custom-file))

;; Define and initialise package repositories
(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("ox-odt" . "https://kjambunathan.github.io/elpa/"))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(package-initialize)

;; use-package to simplify the config file
(unless (package-installed-p 'use-package)
        (package-refresh-contents)
        (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure 't)

;; Keyboard-centric user interface
(setq inhibit-splash-screen t
  initial-scratch-message nil
  initial-major-mode 'text-mode)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)

(setq make-backup-files nil)
(setq blink-cursor-interval 0.6)
(require 'wc-mode)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; Suggested setting
(global-set-key "\C-cw" 'wc-mode)

(save-place-mode 1)

;; deft
(require 'deft)
(setq deft-extensions '("txt" "tex" "org" "md"))
(setq deft-directory "~/Dokumente/arbeit")
(setq deft-recursive t)
(setq deft-use-filename-as-title t)
(setq deft-recursive-ignore-dir-regexp
  (concat "\\(?:"
          "\\."
          "\\|\\.\\."
          "\\\|org"
          "\\)$"))

(global-set-key [f2] 'deft)

;; quelpa
(quelpa
  '(quelpa-use-package
    :fetcher git
    :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)

;;gcmh
(use-package gcmh
  :diminish gcmh-mode
  :config
  (setq gcmh-idle-delay 5
        gcmh-high-cons-threshold (* 16 1024 1024)) ; 16mb
  (gcmh-mode 1))

(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-percentage 0.1))) ;; Default value for `gc-cons-percentage'

(add-hook 'emacs-startup-hook
  (lambda ()
    (message "Emacs ready in %s with %d garbage collections."
             (format "%.2f seconds"
                     (float-time
                      (time-subtract after-init-time before-init-time)))
             gcs-done)))

;;markdown mode
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

;; olivetti
(require 'olivetti)

;; Autosave on focus change
(add-hook 'focus-out-hook 'save-buffer)

;; all the icons
(require 'all-the-icons)

;; flycheck
(package-install 'flycheck)

(global-flycheck-mode)
(package-install 'exec-path-from-shell)
(exec-path-from-shell-initialize)
(add-hook 'after-init-hook #'global-flycheck-mode)

;;dumb jump
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
(setq xref-show-definitions-function #'xref-show-definitions-completing-read)


;; go-mode
; "company" is auto-completion
(require 'company)
(require 'go-mode)
(require 'company-go)
(require 'go-autocomplete)
(require 'auto-complete-config)
(ac-config-default)

;;lspmode
;; if you want to change prefix for lsp-mode keybindings.
(setq lsp-keymap-prefix "s-l")
(require 'lsp-mode)

(use-package lsp-mode
  :config
  (setq lsp-idle-delay 0.5
        lsp-enable-symbol-highlighting t
        lsp-enable-snippet nil ;; Not supported by company capf, which is the recommended company backend
        lsp-pyls-plugins-flake8-enabled t)
  (lsp-register-custom-settings
   '(("pyls.plugins.pyls_mypy.enabled" t t)
     ("pyls.plugins.pyls_mypy.live_mode" nil t)
     ("pyls.plugins.pyls_black.enabled" t t)
     ("pyls.plugins.pyls_isort.enabled" t t)

     ;; Disable these as they're duplicated by flake8
     ("pyls.plugins.pycodestyle.enabled" nil t)
     ("pyls.plugins.mccabe.enabled" nil t)
     ("pyls.plugins.pyflakes.enabled" nil t)))
  :hook
  ((python-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration)))

(use-package lsp-ui
  :config (setq lsp-ui-sideline-show-hover t
                lsp-ui-sideline-delay 0.5
                lsp-ui-doc-delay 5
                lsp-ui-sideline-ignore-duplicates t
                lsp-ui-doc-position 'bottom
                lsp-ui-doc-alignment 'frame
                lsp-ui-doc-header nil
                lsp-ui-doc-include-signature t
                lsp-ui-doc-use-childframe t)
  :commands lsp-ui-mode
  :bind (:map evil-normal-state-map
              ("gd" . lsp-ui-peek-find-definitions)
              ("gr" . lsp-ui-peek-find-references)
              :map md/leader-map
              ("Ni" . lsp-ui-imenu)))

;;python
(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode t)

  ;; Set correct Python interpreter
  (setq pyvenv-post-activate-hooks
        (list (lambda ()
                (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python3")))))
  (setq pyvenv-post-deactivate-hooks
        (list (lambda ()
                (setq python-shell-interpreter "python3")))))

(add-hook 'javascript-mode-hook #'lsp)

(lsp-treemacs-sync-mode 1)

(setq lsp-diagnostics-provider :none)

(define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol)

;;yasnippet
(require 'yasnippet)
(yas-global-mode 1)


;; Discover my major
(global-set-key (kbd "C-h C-m") 'discover-my-major)
(global-set-key (kbd "C-h M-m") 'discover-my-mode)

;; set mouse cursor
;;;; Mouse scrolling in terminal emacs
(unless (display-graphic-p)
        ;; activate mouse-based scrolling
        (xterm-mouse-mode 1)
        (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
        (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

;; orgami
(require 'origami)
(global-origami-mode)
(global-set-key (kbd "C-c f") 'origami-toggle-node)

;;whichkey
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; json-mode
(add-hook 'json-mode-hook
  (lambda ()
    (make-local-variable 'js-indent-level)
    (setq js-indent-level 2)))

;;prettier
(add-hook 'web-mode-hook 'prettier-js-mode)

(defun enable-minor-mode (my-pair)
  "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
          (funcall (cdr my-pair)))))

(add-hook 'web-mode-hook #'(lambda ()
                            (enable-minor-mode
                             '("\\.jsx?\\'" . prettier-js-mode))))


;; neotree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

;;auto-complete
(use-package auto-complete
  :ensure t)

;;web-mode
(use-package web-mode
  :ensure t
  :config
  (progn
   (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
   (setq web-mode-engines-alist
         '(("django"    . "\\.html\\'")))
   (setq web-mode-ac-sources-alist
         '(("css" . (ac-source-css-property))
           ("html" . (ac-source-words-in-buffer ac-source-abbrev))))
   (setq web-mode-markup-indent-offset 2)
   (setq web-mode-code-indent-offset 2)
   (setq web-mode-css-indent-offset 2)
   (setq web-mode-enable-current-element-highlight t)
   (setq web-mode-enable-auto-closing t)
   (setq web-mode-enable-auto-quoting t)
   (setq web-mode-enable-auto-pairing t)
   (setq web-mode-enable-auto-expanding t)
   (setq web-mode-enable-css-colorization t)))

;; undotree
(global-undo-tree-mode)

;;projectile
(use-package projectile
  :ensure t)

;;org-mode
;; orgmode config
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-log-done 'time)

(add-hook 'org-mode-hook 'org-appear-mode)
(setq org-ellipsis "⤵")

(setq org-default-notes-file "~/Dokumente/arbeit/org/notizen.org")

;;org-ql
(use-package org-ql
  :quelpa (org-ql :fetcher github :repo "alphapapa/org-ql"
                  :files (:defaults (:exclude "helm-org-ql.el"))))

;; Improve org mode looks
(setq org-startup-indented t
  org-pretty-entities t
  org-hide-emphasis-markers t
  org-startup-with-inline-images t
  org-image-actual-width '(300))

;; org-mode agendafiles
(setq org-agenda-files (list "/home/tjunk/Dokumente/arbeit/org/slimlist.org"
                             "/home/tjunk/Dokumente/arbeit/org/upcoming.org"
                             "/home/tjunk/Dokumente/arbeit/org/lkos.org"
                             "/home/tjunk/Dokumente/arbeit/org/bfz2.org"
                             "/home/tjunk/Dokumente/arbeit/org/migdb.org"
                             "/home/tjunk/Dokumente/arbeit/org/intkibe.org"
                             "/home/tjunk/Dokumente/arbeit/org/ganztag.org"
                             "/home/tjunk/Dokumente/arbeit/org/wamos.org"
                             "/home/tjunk/Dokumente/arbeit/org/jmd.org"))

(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;;ox-pandoc
;; default options for all output formats
(setq org-pandoc-options '((standalone . t)))
;; cancel above settings only for 'docx' format
(setq org-pandoc-options-for-docx '((standalone . nil)))
;; special settings for beamer-pdf and latex-pdf exporters
(setq org-pandoc-options-for-beamer-pdf '((pdf-engine . "xelatex")))
(setq org-pandoc-options-for-latex-pdf '((pdf-engine . "pdflatex")))
;; special extensions for markdown_github output
(setq org-pandoc-format-extensions '(markdown_github+pipe_tables+raw_html))

;;org roam
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/Dokumente/arbeit/zettelkasten"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))


;; org-download
(require 'org-download)

;; Drag-and-drop to `dired`
(add-hook 'dired-mode-hook 'org-download-enable)

;;ox-odt
(require 'ox-odt)
(setq org-odt-preferred-output-format "docx")

;; Fast TODO Selection
(setq org-use-fast-todo-selection t)

(setq org-log-done 'time)

(require 'org-journal)

(require 'org-superstar)
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
(setq
  org-superstar-headline-bullets-list '("❀" "⁕" "★" "☆" "✦"))

;;org-download
(use-package org-download
  :ensure t)

;;magit
(use-package magit
  :ensure t
  :config (setq magit-display-buffer-function ;; Make Magit Fullscreen
                (lambda (buffer)
                  (if magit-display-buffer-noselect
                      ;; the code that called `magit-display-buffer-function'
                      ;; expects the original window to stay alive, we can't go
                      ;; fullscreen
                      (magit-display-buffer-traditional buffer)
                      (delete-other-windows)
                      ;; make sure the window isn't dedicated, otherwise
                      ;; `set-window-buffer' throws an error
                      (set-window-dedicated-p nil nil)
                      (set-window-buffer nil buffer)
                      ;; return buffer's window
                      (get-buffer-window buffer)))))

(global-set-key "\C-xg" 'magit-status)

;;doom-modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; How tall the mode-line should be. It's only respected in GUI.
;; If the actual char height is larger, it respects the actual height.
(setq doom-modeline-height 25)

;; How wide the mode-line bar should be. It's only respected in GUI.
(setq doom-modeline-bar-width 4)

;; Whether to use hud instead of default bar. It's only respected in GUI.
(setq doom-modeline-hud nil)

;; The limit of the window width.
;; If `window-width' is smaller than the limit, some information won't be displayed.
(setq doom-modeline-window-width-limit fill-column)

;; How to detect the project root.
;; nil means to use `default-directory'.
;; The project management packages have some issues on detecting project root.
;; e.g. `projectile' doesn't handle symlink folders well, while `project' is unable
;; to hanle sub-projects.
;; You can specify one if you encounter the issue.
(setq doom-modeline-project-detection 'auto)

;; Determines the style used by `doom-modeline-buffer-file-name'.
;;
;; Given ~/Projects/FOSS/emacs/lisp/comint.el
;;   auto => emacs/lisp/comint.el (in a project) or comint.el
;;   truncate-upto-project => ~/P/F/emacs/lisp/comint.el
;;   truncate-from-project => ~/Projects/FOSS/emacs/l/comint.el
;;   truncate-with-project => emacs/l/comint.el
;;   truncate-except-project => ~/P/F/emacs/l/comint.el
;;   truncate-upto-root => ~/P/F/e/lisp/comint.el
;;   truncate-all => ~/P/F/e/l/comint.el
;;   truncate-nil => ~/Projects/FOSS/emacs/lisp/comint.el
;;   relative-from-project => emacs/lisp/comint.el
;;   relative-to-project => lisp/comint.el
;;   file-name => comint.el
;;   buffer-name => comint.el<2> (uniquify buffer name)
;;
;; If you are experiencing the laggy issue, especially while editing remote files
;; with tramp, please try `file-name' style.
;; Please refer to https://github.com/bbatsov/projectile/issues/657.
(setq doom-modeline-buffer-file-name-style 'auto)

;; Whether display icons in the mode-line.
;; While using the server mode in GUI, should set the value explicitly.
(setq doom-modeline-icon (display-graphic-p))

;; Whether display the icon for `major-mode'. It respects `doom-modeline-icon'.
(setq doom-modeline-major-mode-icon t)

;; Whether display the colorful icon for `major-mode'.
;; It respects `all-the-icons-color-icons'.
(setq doom-modeline-major-mode-color-icon t)

;; Whether display the icon for the buffer state. It respects `doom-modeline-icon'.
(setq doom-modeline-buffer-state-icon t)

;; Whether display the modification icon for the buffer.
;; It respects `doom-modeline-icon' and `doom-modeline-buffer-state-icon'.
(setq doom-modeline-buffer-modification-icon t)

;; Whether to use unicode as a fallback (instead of ASCII) when not using icons.
(setq doom-modeline-unicode-fallback nil)

;; Whether display the minor modes in the mode-line.
(setq doom-modeline-minor-modes nil)

;; If non-nil, a word count will be added to the selection-info modeline segment.
(setq doom-modeline-enable-word-count nil)

;; Major modes in which to display word count continuously.
;; Also applies to any derived modes. Respects `doom-modeline-enable-word-count'.
;; If it brings the sluggish issue, disable `doom-modeline-enable-word-count' or
;; remove the modes from `doom-modeline-continuous-word-count-modes'.
(setq doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))

;; Whether display the buffer encoding.
(setq doom-modeline-buffer-encoding t)

;; Whether display the indentation information.
(setq doom-modeline-indent-info nil)

;; If non-nil, only display one number for checker information if applicable.
(setq doom-modeline-checker-simple-format t)

;; The maximum number displayed for notifications.
(setq doom-modeline-number-limit 99)

;; The maximum displayed length of the branch name of version control.
(setq doom-modeline-vcs-max-length 12)

;; Whether display the workspace name. Non-nil to display in the mode-line.
(setq doom-modeline-workspace-name t)

;; Whether display the perspective name. Non-nil to display in the mode-line.
(setq doom-modeline-persp-name t)

;; If non nil the default perspective name is displayed in the mode-line.
(setq doom-modeline-display-default-persp-name nil)

;; If non nil the perspective name is displayed alongside a folder icon.
(setq doom-modeline-persp-icon t)

;; Whether display the `lsp' state. Non-nil to display in the mode-line.
(setq doom-modeline-lsp t)

;; Whether display the GitHub notifications. It requires `ghub' package.
(setq doom-modeline-github nil)

;; The interval of checking GitHub.
(setq doom-modeline-github-interval (* 30 60))

;; Whether display the modal state icon.
;; Including `evil', `overwrite', `god', `ryo' and `xah-fly-keys', etc.
(setq doom-modeline-modal-icon t)

;; Whether display the gnus notifications.
(setq doom-modeline-gnus t)

;; Whether gnus should automatically be updated and how often (set to 0 or smaller than 0 to disable)
(setq doom-modeline-gnus-timer 2)

;; Wheter groups should be excludede when gnus automatically being updated.
(setq doom-modeline-gnus-excluded-groups '("dummy.group"))

;; Whether display the IRC notifications. It requires `circe' or `erc' package.
(setq doom-modeline-irc t)

;; Function to stylize the irc buffer names.
(setq doom-modeline-irc-stylize 'identity)

;; Whether display the environment version.
(setq doom-modeline-env-version t)
;; Or for individual languages
(setq doom-modeline-env-enable-python t)
(setq doom-modeline-env-enable-ruby t)
(setq doom-modeline-env-enable-perl t)
(setq doom-modeline-env-enable-go t)
(setq doom-modeline-env-enable-elixir t)
(setq doom-modeline-env-enable-rust t)

;; Change the executables to use for the language version string
(setq doom-modeline-env-python-executable "python") ; or `python-shell-interpreter'
(setq doom-modeline-env-ruby-executable "ruby")
(setq doom-modeline-env-perl-executable "perl")
(setq doom-modeline-env-go-executable "go")
(setq doom-modeline-env-elixir-executable "iex")
(setq doom-modeline-env-rust-executable "rustc")

;; What to display as the version while a new one is being loaded
(setq doom-modeline-env-load-string "...")

;; Hooks that run before/after the modeline version string is updated
(setq doom-modeline-before-update-env-hook nil)
(setq doom-modeline-after-update-env-hook nil)

(custom-set-faces
  '(mode-line ((t (:family "Noto Sans" :height 0.9))))
  '(mode-line-inactive ((t (:family "Noto Sans" :height 0.9)))))


;;helm
(require 'helm)
(require 'helm-config)
;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-M-x-fuzzy-match t)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(setq helm-buffers-fuzzy-matching t
  helm-recentf-fuzzy-match t)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-z") 'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
      (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p t ; open helm buffer inside current window, not occupy whole other window
  helm-move-to-line-cycle-in-source t ; move to end or beginning of source when reaching top or bottom of source.
  helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp.
  helm-scroll-amount 8 ; scroll 8 lines other window using M-<next>/M-<prior>
  helm-ff-file-name-history-use-recentf t
  helm-echo-input-in-header-line t)

(defun spacemacs//helm-hide-minibuffer-maybe ()
  "Hide minibuffer in Helm session if we use the header line as input field."
  (when (with-helm-buffer helm-echo-input-in-header-line)
        (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
          (overlay-put ov 'window (selected-window))
          (overlay-put ov 'face
                       (let ((bg-color (face-background 'default nil)))
                         `(:background ,bg-color :foreground ,bg-color)))
          (setq-local cursor-type nil))))

(add-hook 'helm-minibuffer-set-up-hook
  'spacemacs//helm-hide-minibuffer-maybe)

(setq helm-autoresize-max-height 0)
(setq helm-autoresize-min-height 30)
(helm-autoresize-mode 1)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z") 'helm-select-action) ; list actions using C-z

(setq helm-locate-fuzzy-match t)
(global-set-key (kbd "C-c h o") 'helm-occur)
(setq helm-apropos-fuzzy-match t)
(setq helm-lisp-fuzzy-completion t)
(global-set-key (kbd "C-c h x") 'helm-register)
(global-set-key (kbd "C-c h g") 'helm-google-suggest)
(define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)

(helm-mode 1)

;;helm-tramp
(setq tramp-default-method "ssh")
(define-key global-map (kbd "C-c s") 'helm-tramp)
(add-hook 'helm-tramp-pre-command-hook '(lambda () (global-aggressive-indent-mode 0)
         (projectile-mode 0)
         (editorconfig-mode 0)))
(add-hook 'helm-tramp-quit-hook '(lambda () (global-aggressive-indent-mode 1)
         (projectile-mode 1)
         (editorconfig-mode 1)))
(setq make-backup-files nil)
(setq create-lockfiles nil)

;;tramp
(setq remote-file-name-inhibit-cache nil)
(setq vc-ignore-dir-regexp
  (format "%s\\|%s"
          vc-ignore-dir-regexp
          tramp-file-name-regexp))
(setq tramp-verbose 1)

;;helm-gopackage
(autoload 'helm-go-package "helm-go-package") ;; Not necessary if using ELPA package
(eval-after-load 'go-mode
  '(substitute-key-definition 'go-import-add 'helm-go-package go-mode-map))

;;helm-flycheck
(require 'helm-flycheck) ;; Not necessary if using ELPA package
(eval-after-load 'flycheck
  '(define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))

;;helm descbinds
(require 'helm-descbinds)
(helm-descbinds-mode)

;;helm-swoop
;; Locate the helm-swoop folder to your path
(add-to-list 'load-path "~/.emacs.d/elisp/helm-swoop")
(require 'helm-swoop)

;; Change the keybinds to whatever you like :)
(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)

;; When doing isearch, hand the word over to helm-swoop
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
;; From helm-swoop to helm-multi-swoop-all
(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
;; When doing evil-search, hand the word over to helm-swoop
;; (define-key evil-motion-state-map (kbd "M-i") 'helm-swoop-from-evil-search)

;; Instead of helm-multi-swoop-all, you can also use helm-multi-swoop-current-mode
(define-key helm-swoop-map (kbd "M-m") 'helm-multi-swoop-current-mode-from-helm-swoop)

;; Move up and down like isearch
(define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
(define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)

;; Save buffer when helm-multi-swoop-edit complete
(setq helm-multi-swoop-edit-save t)

;; If this value is t, split window inside the current window
(setq helm-swoop-split-with-multiple-windows nil)

;; Split direcion. 'split-window-vertically or 'split-window-horizontally
(setq helm-swoop-split-direction 'split-window-vertically)

;; If nil, you can slightly boost invoke speed in exchange for text color
(setq helm-swoop-speed-or-color nil)

;; ;; Go to the opposite side of line from the end or beginning of line
(setq helm-swoop-move-to-line-cycle t)

;; Optional face for line numbers
;; Face name is `helm-swoop-line-number-face`
(setq helm-swoop-use-line-number-face t)

;; If you prefer fuzzy matching
(setq helm-swoop-use-fuzzy-match t)

;; Use search query at the cursor  (default)
(setq helm-swoop-pre-input-function
  (lambda () (thing-at-point 'symbol)))

;; Disable pre-input
(setq helm-swoop-pre-input-function
  (lambda () ""))
;; Or, just use M-x helm-swoop-without-pre-input

;; Match only for symbol
(setq helm-swoop-pre-input-function
  (lambda () (format "\\_<%s\\_> " (thing-at-point 'symbol))))

;; Always use the previous search for helm. Remember C-<backspace> will delete entire line
(setq helm-swoop-pre-input-function
  (lambda ()
    (if (boundp 'helm-swoop-pattern)
        helm-swoop-pattern
        "")))

;; If there is no symbol at the cursor, use the last used words instead.
(setq helm-swoop-pre-input-function
  (lambda ()
    (let (($pre-input (thing-at-point 'symbol)))
      (if (eq (length $pre-input) 0)
          helm-swoop-pattern
          ;; this variable keeps the last used words
          $pre-input))))

;; If a symbol or phrase is selected, use it as the initial query.
(setq helm-swoop-pre-input-function
  (lambda ()
    (if mark-active
        (buffer-substring-no-properties (mark) (point))
        "")))

;; helm projectile
(require 'helm-projectile)
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(setq projectile-enable-caching t)
(helm-projectile-on)

(setq helm-semantic-fuzzy-match t
  helm-imenu-fuzzy-match t)

;;helm ripgrep
(setq helm-grep-ag-command (concat "rg"
                                   " --color=never"
                                   " --smart-case"
                                   " --no-heading"
                                   " --line-number %s %s %s")
  helm-grep-file-path-style 'relative)
(defun mu-helm-rg (directory &optional with-types)
  "Search in DIRECTORY with RG.
With WITH-TYPES, ask for file types to search in."
  (interactive "P")
  (require 'helm-adaptive)
  (helm-grep-ag-1 (expand-file-name directory)
                  (helm-aif (and with-types
                                 (helm-grep-ag-get-types))
                            (helm-comp-read
                             "RG type: " it
                             :must-match t
                             :marked-candidates t
                             :fc-transformer 'helm-adaptive-sort
                             :buffer "*helm rg types*"))))

(defun mu-helm-project-search (&optional with-types)
  "Search in current project with RG.
With WITH-TYPES, ask for file types to search in."
  (interactive "P")
  (mu-helm-rg (mu--project-root) with-types))

(defun mu-helm-file-search (&optional with-types)
  "Search in `default-directory' with RG.
With WITH-TYPES, ask for file types to search in."
  (interactive "P")
  (mu-helm-rg default-directory with-types))

(defun mu--project-root ()
  "Return the project root directory or `helm-current-directory'."
  (require 'helm-ls-git)
  (if-let (dir (helm-ls-git-root-dir))
          dir
          (helm-current-directory)))

;; company mode
(add-hook 'after-init-hook 'global-company-mode)

(eval-after-load 'company
  '(add-to-list 'company-backends 'company-ghc))

(setq company-dabbrev-downcase 0)
(setq company-idle-delay 0)

(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)

;; Go - lsp-mode
;; Set up before-save hooks to format buffer and add/delete imports.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; Start LSP Mode and YASnippet mode
(add-hook 'go-mode-hook #'lsp-deferred)
(add-hook 'go-mode-hook #'yas-minor-mode)

;;helm company
(autoload 'helm-company "helm-company") ;; Not necessary if using ELPA package
(eval-after-load 'company
  '(progn
     (define-key company-mode-map (kbd "C-:") 'helm-company)
     (define-key company-active-map (kbd "C-:") 'helm-company)))



;;spacemacs theme
(load-theme 'spacemacs-dark t)

;; line numbers
(global-display-line-numbers-mode t)

;; line highlighting
(global-hl-line-mode +1)

;; autosave
(setq backup-directory-alist
  `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
  `((".*" ,temporary-file-directory t)))

;; column numbers
(setq column-number-mode t)

;; init.el ends here
