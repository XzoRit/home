;;; package --- emacs_config

;;; Commentary:
;;; My own configurations for making Emacs MY editor

;;; Code:

;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; use-package
(straight-use-package 'use-package)

;; variables used in this file
(defvar xzr:plantuml-jar-path
  "~/a/b/c/plantuml/plantuml.jar")
(defvar xzr:reveal-js-root
  "file:///a/b/c/reveal.js")

;; org
;; has to be high on top, see here:
;; https://github.com/org-roam/org-roam/issues/1916
(straight-use-package 'org)
(setq org-plantuml-jar-path xzr:plantuml-jar-path)

;; nano
(straight-use-package
 '(nano-emacs :type git :host github :repo "rougier/nano-emacs"))

(setq nano-font-family-monospaced "JetBrains Mono")
(setq nano-font-size 10)

(require 'nano-layout)
(require 'nano-theme-light)
;(require 'nano-theme-dark)
(require 'nano-faces)
(nano-faces)
(require 'nano-theme)
(nano-theme)
(require 'nano-defaults)
(require 'nano-modeline)

(tab-bar-mode t)
(show-paren-mode t)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; C-h shall be delete char backwards
;; C-h is replaced with M-?
(global-set-key (kbd "M-?") 'help-command)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)

;; center cursor when scrolling
(setq scroll-preserve-screen-position t
      scroll-conservatively 0
      maximum-scroll-margin 0.5
      scroll-margin 99999)

(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (C . t)
   (dot . t)
   (plantuml . t)
   )
 )

(defun xzr:colorize-compilation ()
  "Colorize from `compilation-filter-start' to `point'."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region
     compilation-filter-start (point))))

(add-hook 'compilation-filter-hook
          #'xzr:colorize-compilation)

(use-package hydra
  :straight t
  )

(use-package mini-frame
  :straight t
  :config
  (mini-frame-mode)
  (setq mini-frame-show-parameters
        `((top . 0)
          (left . 0.25)
          (width . 1.0)
          (height . 12)
          (left-fringe . 12)
          (right-fringe .12)
          (child-frame-border-width . 0)
          (internal-border-width . 0)))
  )

(use-package which-key
  :straight t
  :custom
  (which-key-popup-type 'side-window)
  (which-key-side-window-location 'top)
  (which-key-side-window-max-width 1.0)
  (which-key-side-window-max-height 0.25)
  (which-key-idle-delay 1.0)
  (which-key-idle-secondary-delay 0.1)
  (which-key-separator ": " )
  (which-key-unicode-correction 2)
  :config
  (which-key-mode)
  )

(use-package avy
  :straight t
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
  :straight t
  :custom
  (aw-background nil)
  (aw-dispatch-always t)
  :bind
  ("C-c a" . ace-select-window)
  )

(use-package undo-tree
  :straight t
  :config
  (global-undo-tree-mode)
  )

(use-package yaml-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  )

(use-package json-mode
  :straight t
  )

(use-package rust-mode
  :straight t
  )

(use-package clojure-mode
  :straight t
  )

(use-package cmake-mode
  :straight t
  )

(use-package plantuml-mode
  :straight t
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
  :straight t
  :config
  (setq graphviz-dot-indent-width 4)
  )

(setq lsp-keymap-prefix (kbd "C-c l"))
(use-package lsp-mode
  :straight t
  :after
  (which-key)
  :custom
  (lsp-headerline-breadcrumb-enable nil)
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

(use-package ox-reveal
  :straight t
  :custom
  (org-reveal-root xzr:reveal-js-root)
  (org-reveal-highlight-css "%r/plugin/highlight/tomorrow-night-eighties.css")
  )

(use-package flycheck
  :straight t
  :config
  (global-flycheck-mode)
  )

(use-package magit
  :straight t
  )

(use-package git-gutter
  :straight t
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
  :straight t
  :config
  (fset 'c-indent-region 'clang-format-region)
  :hook
  (c-mode . clang-format+-mode)
  (c++-mode . clang-format+-mode)
  )

(use-package projectile
  :straight t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  )

(use-package window-purpose
  :straight t
  :init
  :config
  (purpose-mode)
  (purpose-add-user-purposes :modes '(
                                        ; edit
                                      (c++-mode . cpp_edit)
                                        ; terminal
                                      (ansi-term-mode . terminal)
                                      (bat-mode . terminal)
                                      (compilation-mode . terminal)
                                      (shell-mode . terminal)
                                      (term-mode . terminal)
                                        ; navigation
                                      (dired-mode . navigation)
                                      ))

  (defhydra xzr:hydra-purpose (:color blue :hint nil)
    "
purpose:
    _l_: purpose-load-window-layout       _d_: purpose-toggle-window-purpose-dedicated
    _s_: purpose-save-window-layout       _D_: purpose-toggle-window-buffer-dedicated
    _r_: purpose-reset-window-layout      _p_: purpose-set-window-purpose
    _L_: purpose-load-window-layout-file  _k_: purpose-delete-non-dedicated-windows
    _S_: purpose-save-window-layout-file  _m_: purpose-mode

    C-x b purpose-friendly-switch-buffer
    C-u C-x b switch-buffer-without-purpose
    C-u C-u C-x b purpose-switch-buffer-with-purpose
"
    ("m" purpose-mode)

    ("l" purpose-load-window-layout)
    ("s" purpose-save-window-layout)
    ("r" purpose-reset-window-layout)
    ("L" purpose-load-window-layout-file)
    ("S" purpose-save-window-layout-file)

    ("d" purpose-toggle-window-purpose-dedicated)
    ("D" purpose-toggle-window-buffer-dedicated)
    ("p" purpose-set-window-purpose)

    ("k" purpose-delete-non-dedicated-windows)
    )

  (global-set-key (kbd "C-c v") 'xzr:hydra-purpose/body)
  )

(require 'window-purpose-x)
(purpose-x-magit-single-on)
(purpose-x-kill-setup)

(straight-use-package '( vertico :files (:defaults "extensions/*")
                         :includes (vertico-buffer
                                    vertico-directory
                                    vertico-flat
                                    vertico-grid
                                    vertico-indexed
                                    vertico-mouse
                                    vertico-multiform
                                    vertico-quick
                                    vertico-repeat
                                    vertico-reverse
                                    vertico-unobtrusive)))
(use-package vertico
  :straight t
  :config
  (vertico-mode)
  (vertico-mouse-mode)
  (vertico-reverse-mode)
  (vertico-indexed-mode)
  )

(use-package orderless
  :straight t
  :custom
  (completion-styles
   '(orderless)
   )
  )

(use-package embark
  :straight t
  :bind
  (("C-." . embark-act))
  )

(use-package marginalia
  :straight t
  :bind
  (("C-c m" . marginalia-cycle))
  :init
  (marginalia-mode)
  )

(use-package consult
  :straight t
  :bind
  (
   ("M-y" . consult-yank-pop)
   )
  :config
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.5 any)
   )
  )

(use-package corfu
  :straight t
  :init
  (corfu-global-mode)
  )

(use-package cape
  :straight t
  :init
  (defhydra xzr:hydra-cape (:color red :hint nil)
    "
cape:
    _k_: cape-keyword _p_: completion-at-point _t_: complete-tag
    _a_: cape-abbrev  _d_: cape-dabbrev
    _l_: cape-line    _f_: cape-file
    _r_: cape-rfc1345 _s_: cape-symbol
    _w_: cape-dict    _i_: cape-ispell
    _&_: cape-sgml    _\\_: cape-tex
"
    ("p" completion-at-point)
    ("t" complete-tag)
    ("d" cape-dabbrev)
    ("f" cape-file)
    ("k" cape-keyword)
    ("s" cape-symbol)
    ("a" cape-abbrev)
    ("i" cape-ispell)
    ("l" cape-line)
    ("w" cape-dict)
    ("\\" cape-tex)
    ("&" cape-sgml)
    ("r" cape-rfc1345)
    )
  (global-set-key (kbd "C-c i") 'xzr:hydra-cape/body)
  )

(use-package dashboard
  :straight t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "Dashboard")
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-center-content t)
  (setq dashboard-icon-type 'all-the-icons)
  (setq dashboard-items '((recents  . 5) (projects . 5)))
)

;;; .emacs ends here
