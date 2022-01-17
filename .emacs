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

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(add-to-list 'default-frame-alist '(fullscreen . fullboth))
(add-to-list 'default-frame-alist '(font . "Jetbrains Mono-10"))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
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

(show-paren-mode t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

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

(use-package all-the-icons
  :ensure t
  )

(use-package doom-themes
  :ensure t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  (doom-gruvbox-brighter-comments t)
  (doom-gruvbox-dark-variant "hard")
  :config
  (doom-themes-org-config)
  (load-theme 'doom-gruvbox t)
  )

(use-package doom-modeline
  :ensure t
  :config
  (doom-modeline-mode t)
  )

(use-package which-key
  :ensure t
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
  :init
  (setq aw-background nil)
  :bind
  ("C-c a" . ace-select-window)
  )

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode)
  )

(use-package yaml-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  )

(use-package json-mode
  :ensure t
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

(setq lsp-keymap-prefix (kbd "C-c l"))
(use-package lsp-mode
  :ensure t
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
  :ensure t
  :config
  (setq org-reveal-root
        xzr:reveal-js-root)
  )

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode)
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
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  )

(use-package window-purpose
  :ensure t
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

(use-package vertico
  :ensure t
  :config
  (vertico-mode)
  (vertico-mouse-mode)
  (vertico-reverse-mode)
  )

(use-package orderless
  :ensure t
  :custom
  (completion-styles
   '(orderless)
   )
  )

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act))
  )

(use-package marginalia
  :ensure t
  :bind
  (("C-c m" . marginalia-cycle))
  :init
  (marginalia-mode)
  )

(use-package consult
  :ensure t
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
  :ensure t
  :init
  (corfu-global-mode)
  )

(use-package cape
  :ensure t
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

(use-package popper
  :ensure t
  :init
  (defhydra xzr:hydra-popper (:color blue :hint nil)
    "
popper:
    _l_: popper-toggle-latest     _t_: popper-toggle-type
    _k_: popper-kill-latest-popup _m_: popper-mode
    _c_: popper-cycle             _e_: popper-echo-mode
"
    ("c" popper-cycle :color red)
    ("l" popper-toggle-latest)
    ("t" popper-toggle-type)
    ("k" popper-kill-latest-popup)
    ("m" popper-mode)
    ("e" popper-echo-mode)
    )

  (global-set-key (kbd "C-c o") 'xzr:hydra-popper/body)

  (setq popper-reference-buffers
        '(
          "\\*Async Shell Command\\*"
          "\\*Backtrace\\*"
          "\\*Flycheck Errors\\*"
          "\\*Messages\\*"
          "\\*Warnings\\*"
          "\\*undo-tree\\*"
          "\\*xref\\*"

          "Apropos\\*$"
          "Output\\*$"

          "\\*git-gutter"
          help-mode))
  (setq popper-group-function #'popper-group-by-projectile)
  (popper-mode +1)
  (popper-echo-mode +1)
  )

;; EXPERIMENTS
;; mini-frame, shackle, straight

;;; .emacs ends here
