;;; package --- emacs_config

;;; Commentary:
;;; My own configurations for making Emacs MY editor

;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
;; add org packages too
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

;; This is only needed once, near the top of the file
(eval-when-compile
  (require 'use-package))

;; variables used in this file
(defvar xzr:plantuml-jar-path
  "~/projects/programming/plantuml/plantuml.jar")
(defvar xzr:reveal-js-root
  "file:///home/xzr/projects/programming/reveal.js")

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(add-to-list 'default-frame-alist '(fullscreen . fullboth))
(add-to-list 'default-frame-alist '(font . "Jetbrains Mono-10"))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

(add-to-list 'auto-mode-alist '("\\.ipp\\'" . c++-mode))

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; set keys for apple keyboard, for emacs in osx
;; make cmd key do meta
(setq mac-command-modifier 'meta)
;; make alt key do super
(setq mac-option-modifier 'super)
;; the right option modifier shall stay as it is
;; for characters like @, ~, ...
(setq mac-right-option-modifier 'none)
;; make control key do control
(setq mac-control-modifier 'control)
;; make fn key do hyper
(setq ns-function-modifier 'hyper)

;; for emacs running on windows
;; left windows key is super
(setq w32-pass-lwindow-to-system nil)
(setq w32-lwindow-modifier 'super)
;; right windows key too
(setq w32-pass-rwindow-to-system nil)
(setq w32-rwindow-modifier 'super)
;; menue key is hyper
(setq w32-pass-apps-to-system nil)
(setq w32-apps-modifier 'hyper)

;; C-h shall be delete char backwards
;; C-h is replaced with M-?
(global-set-key (kbd "M-?") 'help-command)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)

;; tab-bar-mode
(tab-bar-mode t)
;; make highlighted tab stand out a bit more
(set-face-attribute 'tab-bar-tab nil
                    :inherit 'doom-modeline-panel
                    :foreground nil
                    :background nil)
;; take projectile project name as tab name
(defun xzr:tab-bar-tab-name-function ()
  "Name tab by project name from projectile as default."
  (projectile-project-name))
(setq tab-bar-tab-name-function #'xzr:tab-bar-tab-name-function)

(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (C . t)
   (dot . t)
   (plantuml . t)
   )
 )

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (setq doom-modeline-icon t))))

(use-package hydra
  :ensure t
  )

(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  )

(use-package helm
  :ensure t
  :demand t
  :bind
  (
   ("M-y"                               . helm-show-kill-ring)
   ("C-x b"                             . helm-mini)
   ([remap find-file]                   . helm-find-files)
   ([remap occur]                       . helm-occur)
   ([remap list-buffers]                . helm-buffers-list)
   ([remap dabbrev-expand]              . helm-dabbrev)
   ([remap execute-extended-command]    . helm-M-x)
   ([remap apropos-command]             . helm-apropos)
   )
  )

(use-package avy
  :ensure t
  :config
  (defhydra xzr:hydra-avy (:color teal :columns 4)
    "avy"
    ("c" avy-goto-char "avy-goto-char")
    ("t" avy-goto-char-timer "avy-goto-char-timer")
    ("l" avy-goto-line "avy-goto-line")
    ("w" avy-goto-word-1 "avy-goto-word-1")
    )

  (global-set-key (kbd "C-c j") 'xzr:hydra-avy/body)
  )

(use-package ace-window
  :ensure t
  :bind
  ("C-c a" . ace-select-window)
  )

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode)
  )

(use-package multiple-cursors
  :ensure t
  :config
    (defhydra xzr:hydra-multiple-cursors (:color teal :columns 4)
    "multiple-cursors"
    ("a" mc/mark-next-like-this "Adds a cursor and region at the next part of the buffer forwards that matches the current region.")
    )

  (global-set-key (kbd "C-c m") 'xzr:hydra-multiple-cursors/body)
  )

(use-package yasnippet
  :ensure t
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  )

(use-package yasnippet-snippets
  :ensure t
  )

(use-package helm-c-yasnippet
  :ensure t
  :config
  (setq helm-yas-space-match-any-greedy t)
  (defhydra xzr:hydra-helm-yas (:color teal)
    "helm-yas"
    ("c" helm-yas-complete "helm-yas-complete")
    ("v" helm-yas-visit-snippet-file "helm-yas-visit-snippet-file")
    ("n" helm-yas-create-snippet-on-region "helm-yas-create-snippet-on-region")
    )

  (global-set-key (kbd "C-c y") 'xzr:hydra-helm-yas/body)
  )

(use-package yaml-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  )

(use-package json-mode
  :ensure t
  )

(use-package magit
  :ensure t
  )

(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode t)
  (setq git-gutter:lighter " GG")
  (setq git-gutter:update-interval 2)
  (git-gutter:start-update-timer)
  (defhydra xzr:hydra-git-gutter (:body-pre (git-gutter-mode 1) :hint nil)
    "
Git gutter:
  _n_: next hunk        _s_tage hunk    s_t_atistics
  _p_: previous hunk    _r_evert hunk
  _a_: first hunk       p_o_pup hunk    _q_uit
  _e_: last hunk        _m_ark hunk     _Q_uit and deactivate git-gutter

  set start _R_evision
"
    ("n" git-gutter:next-hunk)
    ("p" git-gutter:previous-hunk)
    ("a" (progn (goto-char (point-min))
                (git-gutter:next-hunk 1)))
    ("e" (progn (goto-char (point-min))
                (git-gutter:previous-hunk 1)))
    ("s" git-gutter:stage-hunk)
    ("r" git-gutter:revert-hunk)
    ("o" git-gutter:popup-hunk)
    ("m" git-gutter:mark-hunk)
    ("t" git-gutter:statistic)
    ("R" git-gutter:set-start-revision)
    ("q" nil :color blue)
    ("Q" (git-gutter-mode -1) :color blue))
  (global-set-key (kbd "C-c g") 'xzr:hydra-git-gutter/body)
  )

(use-package clang-format+
  :ensure t
  :config
  (fset 'c-indent-region 'clang-format-region)
  :hook
  (c-mode . clang-format+-mode)
  (c++-mode . clang-format+-mode)
  )

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'helm)
  (defhydra xzr:hydra-projectile (:color teal :columns 4)
    "projectile"
    ("ESC" projectile-project-buffers-other-buffer           "project-buffers-other-buffer")
    ("!"   projectile-run-shell-command-in-root              "run-shell-command-in-root")
    ("&"   projectile-run-async-shell-command-in-root        "run-async-shell-command-in-root")
    ("D"   projectile-dired                                  "dired")
    ("E"   projectile-edit-dir-locals                        "edit-dir-locals")
    ("F"   projectile-find-in-known-projects                 "find-file-in-known-projects")
    ("I"   projectile-ibuffer                                "ibuffer")
    ("P"   projectile-test-project                           "test-project")
    ("R"   projectile-regenerate-tags                        "regenerate-tags")
    ("S"   projectile-save-project-buffers                   "save-project-buffers")
    ("T"   projectile-find-test-file                         "find-test-file")
    ("V"   projectile-browse-dirty-projects                  "browse-dirty-projects")
    ("a"   projectile-find-other-file                        "find-other-file")
    ("b"   projectile-switch-to-buffer                       "switch-to-buffer")
    ("c"   projectile-compile-project                        "compile-project")
    ("d"   projectile-find-dir                               "find-dir")
    ("e"   projectile-recentf                                "recentf")
    ("f"   projectile-find-file                              "find-file")
    ("g"   projectile-find-file-dwim                         "find-file-dwim")
    ("i"   projectile-invalidate-cache                       "invalidate-cache")
    ("j"   projectile-find-tag                               "find-tag")
    ("k"   projectile-kill-buffers                           "kill-buffers")
    ("l"   projectile-find-file-in-directory                 "find-file-in-directory")
    ("m"   projectile-commander                              "commander")
    ("o"   projectile-multi-occur                            "multi-occur")
    ("p"   projectile-switch-project                         "switch-project")
    ("q"   projectile-switch-open-project                    "switch-open-project")
    ("r"   projectile-replace                                "replace")
    ("t"   projectile-toggle-between-implementation-and-test "toggle-between-implementation-and-test")
    ("u"   projectile-run-project                            "run-project")
    ("v"   projectile-vc                                     "vc")
    ("z"   projectile-cache-current-file                     "cache-current-file")

    ("xe"  projectile-run-eshell "run-eshell")
    ("xs"  projectile-run-shell  "run-shell")
    ("xt"  projectile-run-term   "run-term")

    ("sg"  projectile-grep "grep")
    ("ss"  projectile-ag   "ag")

    ("Oo"  projectile-display-buffer                           "display-buffer")
    ("Oa"  projectile-find-other-file-other-window             "find-other-file-other-window")
    ("Ob"  projectile-switch-to-buffer-other-window            "switch-to-buffer-other-window")
    ("Od"  projectile-find-dir-other-window                    "find-dir-other-window")
    ("Of"  projectile-find-file-other-window                   "find-file-other-window")
    ("Og"  projectile-find-file-dwim-other-window              "find-file-dwim-other-window")
    ("Ot"  projectile-find-implementation-or-test-other-window "find-implementation-or-test-other-window")
    )

  (global-set-key (kbd "C-c p") 'xzr:hydra-projectile/body)
  )

(use-package rust-mode
  :ensure t
  )

(use-package clojure-mode
  :ensure t
  )

(use-package cmake-mode
  :ensure t
  )

(setq lsp-keymap-prefix (kbd "C-c l"))
(use-package lsp-mode
  :ensure t
  :config
  (setq lsp-headerline-breadcrumb-enable nil)
  :hook (
         (c++-mode . lsp)
         (c-mode . lsp)
         (cmake-mode . lsp)
         (clojure-mode . lsp)
         (clojurec-mode . lsp)
         (clojurescript-mode . lsp)
         (rust-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration)
         )
  :commands lsp
  )

(use-package lsp-ui
    :ensure t
    :commands
    lsp-ui-mode
    )

(use-package company-lsp
  :ensure t
  :commands
  company-lsp
  )

(use-package helm-lsp
  :ensure t
  :commands
  helm-lsp-workspace-symbol
  )

(use-package lsp-treemacs
  :ensure t
  :commands
  lsp-treemacs-errors-list
  :bind
  (:map global-map
        ("C-c t t"   . treemacs)
        ("C-c t 0"   . treemacs-select-window)
        )
  )

(use-package dap-mode
  :ensure t
  )

(use-package company
  :ensure t
  :after lsp-mode
  :hook (prog-mode . company-mode)
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection)
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.2))

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode)
  )

(use-package ox-reveal
  :ensure t
  :config
  (setq org-reveal-root
        xzr:reveal-js-root)
  )

(use-package plantuml-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist
               '("\\.puml\\'" . plantuml-mode)
               )
  (setq plantuml-jar-path
        (expand-file-name xzr:plantuml-jar-path))
  (setq plantuml-default-exec-mode
        'jar)
  )

(use-package graphviz-dot-mode
  :ensure t
  :config
  (setq graphviz-dot-indent-width 4)
  )

(use-package company-graphviz-dot
  )

(use-package all-the-icons
  :ensure t
  )

(use-package doom-themes
  :ensure t
  :config
  (setq doom-monokai-classic-brighter-comments t)
  (setq doom-monokai-classic-comment-bg t)
  ; dark
  ;(load-theme 'doom-monokai-classic t)
  ;(load-theme 'doom-one t)
  (load-theme 'doom-peacock t)
  ;(load-theme 'wheatgrass t)
  ; light
  ;(load-theme 'doom-one-light t)
  ;(load-theme 'doom-acario-light t)
  ;(load-theme 'leuven t)
  )

(use-package doom-modeline
  :ensure t
  :config
  (doom-modeline-mode t)
  )

;;; .emacs ends here
