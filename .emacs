;;; package --- Summary

;;; Commentary:
;;; My own configurations for making Emacs MY editor

;;; Code:

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tab-bar-mode t)
(show-paren-mode t)
(repeat-mode t)

(set-default-coding-systems 'utf-8)
(setq delete-old-versions t)
(setq history-delete-duplicates t)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.ipp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.tpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.dox\\'" . c++-mode))

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; C-h shall be delete char backwards
;; C-h is replaced with M-?
(global-set-key (kbd "M-?") 'help-command)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)

;; window movement
(defvar-keymap xzr:windmove
  :repeat t
  "n" #'windmove-left
  "r" #'windmove-up
  "t" #'windmove-down
  "d" #'windmove-right
  
  "h" #'windmove-swap-states-left
  "g" #'windmove-swap-states-up
  "f" #'windmove-swap-states-down
  "q" #'windmove-swap-states-right
 )
(keymap-global-set "C-c w" xzr:windmove)

;; search shall be case sensitive
(setq case-fold-search nil)

;; center cursor when scrolling
(setq scroll-preserve-screen-position t
      scroll-conservatively 0
      maximum-scroll-margin 0.5
      scroll-margin 99999)

;; dired
;; see here https://protesilaos.com/codelog/2023-06-26-emacs-file-dired-basics/
(file-name-shadow-mode 1)
(setq delete-by-moving-to-trash t)
(setq dired-dwim-target t)
(add-hook 'dired-mode-hook #'dired-hide-details-mode)

;; compile
(require 'compile)
(setq compilation-scroll-output t)
;; mark assert errors from gcc
(push 'gcc-assert compilation-error-regexp-alist)
(push '(gcc-assert "^[a-z_A-Z]+:\\ \\([^:]+\\):\\([0-9]+\\):\\ \\(.*\\)$" 1 2 nil nil 1) compilation-error-regexp-alist-alist)

;; mark errors detected by ctest in compilation buffer
(push 'ctest compilation-error-regexp-alist)
(push '(ctest "^[0-9]+:\\ \\(/[^(]+\\)(\\([^)]+\\)):\\ \\([^:]+\\):\\ \\(.*\\)" 1 2 nil nil 1) compilation-error-regexp-alist-alist)

;; ansi-color
(require 'ansi-color)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

;; Initialize package sources
(require 'package)
;; from straight doc
;; "prevent package.el loading packages prior to their init-file loading"
(setq package-enable-at-startup nil)
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
              "https://radian-software.github.io/straight.el/install.el"
          'silent
              'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; variables used in this file
(defvar xzr:plantuml-jar-path
  "/usr/share/plantuml/plantuml.jar")
(defvar xzr:reveal-js-root
  "file:///<path-to>/reveal.js/")

; org
; has to be high on top, see here:
; https://github.com/org-roam/org-roam/issues/1916
(straight-use-package 'org)
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (C . t)
   (dot . t)
   (plantuml . t)
   )
 )

(setq org-plantuml-jar-path xzr:plantuml-jar-path)

;; use-package
(straight-use-package 'use-package)

;; nano
(straight-use-package
 '(nano-emacs :type git :host github :repo "rougier/nano-emacs"))

(require 'nano-faces)
(require 'nano-theme-light)
(require 'nano-theme-dark)
(require 'nano-theme)
(require 'nano-modeline)
(require 'nano-layout)
(require 'nano-defaults)

(setq nano-font-family-monospaced "UbuntuMono Nerd Font")
(setq nano-font-size 11)
(setq nano-theme-var 'dark)

(nano-faces)
(nano-theme)
(nano-modeline)
(nano-toggle-theme)

(use-package nerd-icons
  :straight t
  )

;; flyspell
(add-hook 'prog-mode-hook #'flyspell-prog-mode)

(use-package hydra
  :straight t
  )

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
;; This works with `file-name-shadow-mode' enabled.  When you are in
;; a sub-directory and use, say, `find-file' to go to your home '~/'
;; or root '/' directory, Vertico will clear the old path to keep
;; only your current input.
(add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

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

(use-package embark-consult
  :straight t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)
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
  :custom
  (corfu-auto t)
  :init
  (global-corfu-mode)
  )

(use-package cape
  :straight t
  :init
  (defhydra xzr:hydra-cape (:color blue :hint nil)
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
  (aw-scope 'frame)
  (aw-minibuffer-flag t)
  :bind
  ("C-c a" . ace-select-window)
  )

; https://github.com/casouri/vundo
(use-package vundo
   :straight t
  :bind
  ("C-x u" . vundo)
)

(straight-use-package 'tree-sitter)
(straight-use-package 'tree-sitter-langs)
(add-hook 'c++-mode-hook #'tree-sitter-mode)
(add-hook 'c++-mode-hook #'tree-sitter-hl-mode)

(use-package ts-fold
  :straight (ts-fold :type git :host github :repo "emacs-tree-sitter/ts-fold")
  :init
  (defhydra xzr:hydra-ts-fold (:color blue :hint nil)
    "
ts fold:
    _c_: ts-fold-close
    _o_: ts-fold-open
    _r_: ts-fold-open-recursively
    _C_: ts-fold-close-all
    _O_: ts-fold-open-all
    _t_: ts-fold-toggle
"
    ("c" ts-fold-close)
    ("o" ts-fold-open)
    ("r" ts-fold-open-recursively)
    ("C" ts-fold-close-all)
    ("O" ts-fold-open-all)
    ("t" ts-fold-toggle)
  )
  (global-set-key (kbd "C-c t") 'xzr:hydra-ts-fold/body)
  (global-ts-fold-mode)
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

(use-package d-mode
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

(use-package jenkinsfile-mode
  :straight t
  )

(setq lsp-keymap-prefix (kbd "C-c l"))
(setq lsp-clients-clangd-args '("--header-insertion-decorators=0" "--clang-tidy"))
(when (eq system-type 'windows-nt)
  (setq lsp-clients-clangd-args '("-j=4" "--clang-tidy" "--background-index" "--log=error"))
  )
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
         (lsp-mode . lsp-enable-which-key-integration)
         )
  :commands lsp
  )

;; seems to be getting in the way with nano
; (use-package lsp-ui
;   :straight t
; )

(use-package lsp-treemacs
  :straight t
)

(use-package ox-reveal
  :straight t
  :custom
  (org-reveal-root xzr:reveal-js-root)
  (org-reveal-highlight-css "%r/plugin/highlight/zenburn.css")
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

;; god-mode is not working in magit and dired
;; buffers since they are modal too.
;; maybe I should look for another modal package:
;; https://systemcrafters.net/live-streams/april-21-2023/
; (use-package god-mode
;   :straight t
;   :init
;   (global-set-key (kbd "<escape>") #'god-mode-all)
;   (define-key god-local-mode-map (kbd ".") #'repeat)
;   :config
;   ; (god-mode)
; )
;
;(defun xzr:update-cursor-in-god-mode ()
;  "Change cursor style when god-mode is active."
;  (setq cursor-type (if (or god-local-mode buffer-read-only) 'hollow 'box)))
;(add-hook 'post-command-hook #'xzr:update-cursor-in-god-mode)

(use-package dashboard
  :straight t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "Dashboard")
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-center-content t)
  (setq dashboard-display-icons-p t)
  (setq dashboard-icon-type 'nerd-icons)
  (setq dashboard-items '((recents  . 5) (projects . 5)))
)

;; my very own little helper
(defun xzr:flush-blank-lines (start end)
  "Removes blank lines from region"
  (interactive "r")
  (flush-lines "^\\s-*$" start end nil))

(defun xzr:collapse-blank-lines (start end)
  "Colapses empty lines to one empty line"
  (interactive "r")
  (replace-regexp "^\n\\{2,\\}" "\n" nil start end))

(defun xzr:show-file-name ()
  "Show the full path file name in the minibuffer and add it to kill-ring."
  (interactive)
  (message (buffer-file-name))
  (kill-new (file-truename buffer-file-name)))
(global-set-key "\C-cz" 'xzr:show-file-name)

;; https://www.masteringemacs.org/article/demystifying-emacs-window-manager
;; Splitting Windows
(defun xzr:split-below (arg)
  "Split window below from the parent or from root with ARG."
  (interactive "P")
  (split-window (if arg (frame-root-window)
                  (window-parent (selected-window)))
                nil 'below nil))

(defun xzr:split-right (arg)
  "Split window right from the parent or from root with ARG."
  (interactive "P")
  (split-window (if arg (frame-root-window)
                  (window-parent (selected-window)))
                nil 'right nil))

(defun xzr:split-left (arg)
  "Split window left from the parent or from root with ARG."
  (interactive "P")
  (split-window (if arg (frame-root-window)
                  (window-parent (selected-window)))
                nil 'left nil))

(defun xzr:split-above (arg)
  "Split window above from the parent or from root with ARG."
  (interactive "P")
  (split-window (if arg (frame-root-window)
                  (window-parent (selected-window)))
                nil 'above nil))

(defun xzr:toggle-final-newline ()
    "Toggles if a newline is added at the end of file on save."
  (interactive)
  (setq require-final-newline (not require-final-newline)))

(defun xzr:occur-non-ascii ()
  "Find any non-ascii characters in the current buffer."
  (interactive)
  (occur "[^[:ascii:]]"))

;;; .emacs ends here
