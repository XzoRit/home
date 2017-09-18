;; I need package to install my packages
(require 'package)
;; I need the marmalade archive
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
;; packages which I always like to have installed
(defvar xzr:my-packages '(
                          ace-link
                          ace-window
                          adoc-mode
                          alchemist
                          avy
                          cider
                          clojure-mode
                          company-irony
                          company-irony-c-headers
                          d-mode
                          dumb-jump
                          elixir-mode
                          flycheck
                          flycheck-irony
                          fringe-helper
                          git-gutter-fringe
                          go-mode
                          haskell-mode
                          helm
                          helm-gtags
                          helm-projectile
                          helm-swoop
                          hydra
                          iedit
                          irony
                          magit
                          move-text
                          ox-asciidoc
                          ox-reveal
                          powerline
                          projectile
                          plantuml-mode
                          scala-mode
                          swiper-helm
                          use-package
                          yasnippet
                          ))
;; install my packages
(dolist (p xzr:my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

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
;; make pc keyboard's win key or other to type super or hyper
;; left windows key
(setq w32-pass-lwindow-to-system nil)
(setq w32-lwindow-modifier 'super)
;; right windows key
(setq w32-pass-rwindow-to-system nil)
(setq w32-rwindow-modifier 'super)
;; menue key
(setq w32-pass-apps-to-system nil)
(setq w32-apps-modifier 'hyper)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; show trailing whitespaces
;; that can mess up your diff
(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))
;; use space to indent by default
(setq-default indent-tabs-mode nil)
;; set appearance of a tab that is represented by 2 spaces
(setq-default tab-width 2)

;; I do not need the toolbar
(ns-toggle-toolbar)
;; no scroll bars in buffers
(scroll-bar-mode -1)
;; no menu bar
(menu-bar-mode -1)

;; .h files are c++ headers
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
;; set indenting style
(setq c-default-style "stroustrup")

;; I want to use bash installed via ports
;; under /opt/local/bin via the M-x shell command
(setq explicit-shell-file-name "/opt/local/bin/bash")
(setq shell-file-name "bash")
(setq explicit-bash-args '("--login" "--noediting" "-i"))

;; C-h shall be delete char backwards
;; C-h is replaced with M-?
(global-set-key (kbd "M-?") 'help-command)
(global-set-key (kbd "C-h") 'delete-backward-char)

(global-set-key (kbd "C-?") 'mark-paragraph)
(global-set-key (kbd "M-h") 'backward-kill-word)

;; my ls does not understand the --dired option
;; so we tell dired to use the emacs emulation of ls
(setq ls-lisp-use-insert-directory-program nil)
(require 'ls-lisp)

;; I want to enable winner-mode
;; for un/re-doing window config
(winner-mode)

;; I want to be able to use
;; meta + arrow keys to navigate between windows
(windmove-default-keybindings 'meta)

;; plantuml-mode
(require 'plantuml-mode)
;; set path to plantuml.jar file
;; note plantuml.jar uses graphviz dot for creating pictures
;; it searches in /usr/bin but if GRAPHVIZ_DOT is set
;; as an environment variable it uses that dot executeable
(setq plantuml-jar-path "~/projects/programming/plantuml/plantuml.jar")

;; org-mode config
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-agenda)
;; sets timestamp and enter a note each time a task is DONE
(setq org-log-done 'note)
;; Insert state change notes and time stamps into logbook drawer
(setq org-log-into-drawer t)
;; here are my org files to be considered
;; when an agenda view is requested
(setq org-agenda-files (quote ("~/org")))
;; when moving/copying an entry consider all org-agenda files
;; as possible targets
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 3))))
;; file names are shown as refile targets
(setq org-refile-use-outline-path 'file)
;; ask for confirmation if a node is refiled with an unknown parent node
(setq org-refile-allow-creating-parent-nodes 'confirm)
;; this makes helm completion work for org-refile
(setq org-outline-path-complete-in-steps nil)
;; use that hydra from the hydra-examples file to change
;; the different view modes of an agenda
(define-key org-agenda-mode-map "v" 'hydra-org-agenda-view/body)
;; org-babel config
;; org babel belongs to org mode
;; it enables org mode to embedd programming languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . t)
   (dot . t)
   (plantuml . t)))
;; path to my plantuml jar file
(setq org-plantuml-jar-path
  (expand-file-name "~/projects/programming/plantuml/plantuml.jar"))

;; iedit
(require 'iedit)
;; C-; does not work on mac as key binding
(global-set-key (kbd "C-c ;") 'iedit-mode)

;; yasnippet
(require 'yasnippet)
(yas-global-mode t)

;; helm
(require 'helm)
(require 'helm-config)
;; using helm-M-x instead of the normal
;; M-x since it is more powerful
(global-set-key (kbd "M-x") 'helm-M-x)
;; makes navigating the kill-ring
;; much more easier
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
;; helm-mini instead of ido buffer
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
;; even opens directory in dired when not on a file
(global-set-key (kbd "C-x C-f") 'helm-find-files)
;; hellm-occur instead of occur
;; seems to be very similar to swiper ...
(global-set-key (kbd "M-s o") 'helm-occur)

;; set appearance of helm mini buffer
(setq
 ;; open helm buffer inside current window, not occupy whole other window
 helm-split-window-in-side-p t
 ;; move to end or beginning of source when reaching top or bottom of source
 helm-move-to-line-cycle-in-source t
 ;; search for library in `require' and `declare-function' sexp
 helm-ff-search-library-in-sexp t
 ;; scroll 8 lines other window using M-<next>/M-<prior>
 helm-scroll-amount 8
 ;; show recently used files too
 helm-ff-file-name-history-use-recentf t)
;; turn on fuzzy matching for helm-M-x
(setq helm-M-x-fuzzy-match t)
;; turn on fuzzy matching for helm-mini
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)

(helm-mode 1)

;; helm-swoop
;; a more intelligent way of doing isearch
(require 'helm-swoop)
;; search in current buffer
(global-set-key (kbd "M-i") 'helm-swoop)
;; go back to where search started
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
;; search in all opened buffers
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop-all)
;; mark buffer to be searched
(global-set-key (kbd "C-x M-i") 'helm-multi-swoop)
;; When doing isearch, hand the word over to helm-swoop
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
;; From helm-swoop to helm-multi-swoop-all
(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
;; Instead of helm-multi-swoop-all, you can also use helm-multi-swoop-current-mode
(define-key helm-swoop-map (kbd "M-m") 'helm-multi-swoop-current-mode-from-helm-swoop)
;; Save buffer when helm-multi-swoop-edit complete
(setq helm-multi-swoop-edit-save t)
;; If this value is t, split window inside the current window
(setq helm-swoop-split-with-multiple-windows t)
;; Split direcion. 'split-window-vertically or 'split-window-horizontally
(setq helm-swoop-split-direction 'split-window-vertically)
;; If nil, you can slightly boost invoke speed in exchange for text color
(setq helm-swoop-speed-or-color nil)
;; Go to the opposite side of line from the end or beginning of line
(setq helm-swoop-move-to-line-cycle t)
;; If you prefer fuzzy matching
(setq helm-swoop-use-fuzzy-match nil)

;; helm-gtags
;; code browser, parses files generated by gtags (gnu global)
;; and uses the helm way to display candidates
(require 'helm-gtags)
;; c++/c-sources shall be in helm-gtags-mode by default
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)

;; undo-tree
;; undo/redo functions
;; and an undo tree view
(require 'undo-tree)
(global-undo-tree-mode)

;; ox-reveal
(require 'ox-reveal)
;; where is reveal.js?
(setq org-reveal-root "file:////Users/Tobi/projects/programming/reveal_js/reveal.js")

;; powerline
(require 'powerline)
;; my powerline theme
(powerline-vim-theme)

;; graphviz-mode
(require 'graphviz-dot-mode)

;; jam-mode for boost jam files
;; register different ending to jam mode
(require 'jam-mode)
(add-to-list 'auto-mode-alist '("\\.jam$" . jam-mode))
(add-to-list 'auto-mode-alist '("^Jamfile$\\|^Jamfile\\.v2$" . jam-mode))

;; swiper-helm
(require 'swiper-helm)
(global-set-key (kbd "C-c s") 'swiper)

;; move-text
(require 'move-text)

;; fringe-helper
;(require 'fringe-helper)

;; git-gutter-fringe
;(require 'git-gutter-fringe)
;; update diff every n seconds
(setq git-gutter:update-interval 2)
;; set minor mode indicator to GG
;; first character should be a space
(setq git-gutter:lighter " GG")
;; enable git gutter globally
(global-git-gutter-mode)
;; update diff infor every n-seconds
(custom-set-variables
 '(git-gutter:update-interval 2))

;; avy
(require 'avy)
(global-set-key (kbd "C-c SPC") 'avy-goto-subword-1)

;; ace-window
(require 'ace-window)
(global-set-key (kbd "C-c a") 'ace-window)

;; ace-link
(require 'ace-link)
;; just press o in a buffer containing links
;; and you can jump to them like with avy
(ace-link-setup-default)

;; company (complete anything)
;; emacs text completion framework
;; shall be switched on globally
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; irony
;; c/c++ completion package based on irony-server
;; which uses libclang
(require 'irony)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
;; irony shall use be a completion backend for company
(require 'company-irony)
;; autocompletion for c/c++ headers
(require 'company-irony-c-headers)
(eval-after-load 'company
  '(add-to-list 'company-backends '(company-irony-c-headers company-irony)))
;; replace the completion-at-point' and complete-symbol bindings in
;; irony-mode's buffers by irony-mode's function
(defun xzr:irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'xzr:irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; flycheck
;; on the fly syntax checker
(require 'flycheck)
;; enable flycheck globally
(add-hook 'after-init-hook #'global-flycheck-mode)
;; flycheck-irony
(require 'flycheck-irony)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;; package for managing projects
;; e.g.
;; - which directories belong to a project
;; - finding only project specific files
;; - building a project
;; - ...
(require 'projectile)
;; helm as completion backend for projectile
(setq projectile-completion-system 'helm)
;; some more helmified projectile functions
(require 'helm-projectile)
;; projectiles functions shall replaced
;; by the according helm version
(helm-projectile-on)

;; dumb-jumb
;; https://github.com/jacktasia/dumb-jump
;; Dumb Jump is an Emacs "jump to definition" package
;; with support for multiple programming languages that favors "just working".
;; This means minimal -- and ideally zero -- configuration
;; with absolutely no stored indexes (TAGS) or persistent background processes.
;; Dumb Jump uses The Silver Searcher ag, ripgrep rg, or grep
;; to find potential definitions of a function or variable under point
(require 'dumb-jump)
(dumb-jump-mode)
;; use helm for showing possible matches
(setq dumb-jump-selector 'helm)

;; hydra
(require 'hydra)
;; here are my little hydra functions
;; needed for splitter movement
;; and introduces all the examples from
;; hydra-examples.el
(require 'hydra-examples)
(defhydra xzr:hydra-window ()
"
Movement^^      ^Split^            ^Switch^        ^Resize^
----------------------------------------------------------------
_<left>_  ←     _v_ertical         _b_uffer        _l_ |←
_<down>_  ↓     _h_orizontal       _f_ind files    _d_ -↓
_<up>_    ↑     _z_ undo           _a_ce other     _u_ -↑
_<right>_ →     _Z_ reset          _s_ave          _r_ |→
_F_ollow        _k_ill other       _S_wap          _o_nly other
_q_uit          _K_ill this                        _O_nly this
"
  ("<left>" windmove-left)
  ("<right>" windmove-right)
  ("<up>" windmove-up)
  ("<down>" windmove-down)
  ("l" hydra-move-splitter-left)
  ("d" hydra-move-splitter-down)
  ("u" hydra-move-splitter-up)
  ("r" hydra-move-splitter-right)
  ("b" helm-mini)
  ("f" helm-find-files)
  ("F" follow-mode)
  ("a" (lambda ()
	 (interactive)
	 (ace-window 1)
	 (add-hook 'ace-window-end-once-hook
		   'xzr:hydra-window/body))
   )
  ("v" (lambda ()
	 (interactive)
	 (split-window-right)
	 (windmove-right))
   )
  ("h" (lambda ()
	 (interactive)
	 (split-window-below)
	 (windmove-down))
   )
  ("S" (lambda ()
	 (interactive)
	 (ace-window 4)
	 (add-hook 'ace-window-end-once-hook
		   'xzr:hydra-window/body)))
  ("s" save-buffer)
  ("K" delete-window)
  ("k" (lambda ()
	 (interactive)
	 (ace-window 16)
	 (add-hook 'ace-window-end-once-hook
		   'xzr:hydra-window/body))
   )
  ("O" delete-other-windows)
  ("o" ace-maximize-window)
  ("z" (progn
	 (winner-undo)
	 (setq this-command 'winner-undo))
   )
  ("Z" winner-redo)
  ("q" nil)
  )
(global-set-key (kbd "C-c o") 'xzr:hydra-window/body)

(defhydra xzr:hydra-move ()
  "Move cursor"
  ("n" next-line)
  ("p" previous-line)
  ("f" forward-char)
  ("F" forward-word)
  ("b" backward-char)
  ("B" backward-word)
  ("a" beginning-of-line)
  ("e" move-end-of-line)
  ("l" recenter-top-bottom)
  ("v" scroll-up-command)
  ("V" scroll-down-command)
  ("<" beginning-of-buffer)
  (">" end-of-buffer))
(global-set-key (kbd "C-c n") 'xzr:hydra-move/body)

(defhydra xzr:hydra-move-text ()
  "Move text"
  ("<up>" move-text-up "up")
  ("<down>" move-text-down "down"))
(global-set-key (kbd "C-c m") 'xzr:hydra-move-text/body)

(defhydra xzr:hydra-transpose (:color red)
  "Transpose"
  ("c" transpose-chars "characters")
  ("w" transpose-words "words")
  ("l" transpose-lines "lines")
  ("s" transpose-sentences "sentences")
  ("p" transpose-paragraphs "paragraphs")
  ("o" org-transpose-words "Org mode words")
  ("e" org-transpose-elements "Org mode elements")
  ("t" org-table-transpose-table-at-point "Org mode table")
  ("q" nil "cancel" :color blue))
(global-set-key (kbd "C-c t") 'xzr:hydra-transpose/body)

(defhydra xzr:hydra-goto-line (goto-map ""
                                        :pre (linum-mode 1)
                                        :post (linum-mode -1))
  "goto-line"
  ("g" goto-line "go")
  ("m" set-mark-command "mark" :bind nil)
  ("q" nil "quit"))
(global-set-key (kbd "M-g g") 'xzr:hydra-goto-line/body)

(defhydra xzr:hydra-git-gutter (:body-pre (git-gutter-mode 1)
				:hint nil)
  "
Git gutter:
  _n_: next hunk        _s_tage hunk     _q_uit
  _p_: previous hunk    _r_evert hunk    _Q_uit and deactivate git-gutter
  _a_: first hunk       p_o_pup hunk
  _e_: last hunk        _m_ark hunk

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
  ("R" git-gutter:set-start-revision)
  ("q" nil :color blue)
  ("Q" (progn (git-gutter-mode -1)
              ;; git-gutter-fringe doesn't seem to
              ;; clear the markup right away
              (sit-for 0.1)
              (git-gutter:clear))
   :color blue))
(global-set-key (kbd "C-c g") 'xzr:hydra-git-gutter/body)

(defhydra xzr:hydra-helm-gtags (:color blue)
  "helm-gtags"
  ("d" helm-gtags-dwim "dwim")
  ("s" helm-gtags-find-symbol "symbol")
  ("g" helm-gtags-find-pattern "grep")
  ("f" helm-gtags-find-file "file"))
(global-set-key (kbd "C-c r") 'xzr:hydra-helm-gtags/body)

(defhydra xzr:hydra-projectile (:color teal :columns 4)
("ESC" projectile-project-buffers-other-buffer           "project-buffers-other-buffer")
("!"   projectile-run-shell-command-in-root              "run-shell-command-in-root")
("&"   projectile-run-async-shell-command-in-root        "run-async-shell-command-in-root")
("D"   projectile-dired                                  "dired")
("E"   projectile-edit-dir-locals                        "edit-dir-locals")
("F"   helm-projectile-find-in-known-projects            "find-file-in-known-projects")
("I"   projectile-ibuffer                                "ibuffer")
("P"   projectile-test-project                           "test-project")
("R"   projectile-regenerate-tags                        "regenerate-tags")
("S"   projectile-save-project-buffers                   "save-project-buffers")
("T"   projectile-find-test-file                         "find-test-file")
("V"   projectile-browse-dirty-projects                  "browse-dirty-projects")
("a"   projectile-find-other-file                        "find-other-file")
("b"   helm-projectile-switch-to-buffer                  "switch-to-buffer")
("c"   projectile-compile-project                        "compile-project")
("d"   helm-projectile-find-dir                          "find-dir")
("e"   helm-projectile-recentf                           "recentf")
("f"   helm-projectile-find-file                         "find-file")
("g"   helm-projectile-find-file-dwim                    "find-file-dwim")
("i"   projectile-invalidate-cache                       "invalidate-cache")
("j"   projectile-find-tag                               "find-tag")
("k"   projectile-kill-buffers                           "kill-buffers")
("l"   projectile-find-file-in-directory                 "find-file-in-directory")
("m"   projectile-commander                              "commander")
("o"   projectile-multi-occur                            "multi-occur")
("p"   helm-projectile-switch-project                    "switch-project")
("q"   projectile-switch-open-project                    "switch-open-project")
("r"   projectile-replace                                "replace")
("t"   projectile-toggle-between-implementation-and-test "toggle-between-implementation-and-test")
("u"   projectile-run-project                            "run-project")
("v"   projectile-vc                                     "vc")
("z"   projectile-cache-current-file                     "cache-current-file")

("xe" projectile-run-eshell "run-eshell")
("xs" projectile-run-shell  "run-shell")
("xt" projectile-run-term   "run-term")

("sg" helm-projectile-grep "grep")
("ss" helm-projectile-ag    "ag")

("Oo" projectile-display-buffer                           "display-buffer")
("Oa" projectile-find-other-file-other-window             "find-other-file-other-window")
("Ob" projectile-switch-to-buffer-other-window            "switch-to-buffer-other-window")
("Od" projectile-find-dir-other-window                    "find-dir-other-window")
("Of" projectile-find-file-other-window                   "find-file-other-window")
("Og" projectile-find-file-dwim-other-window              "find-file-dwim-other-window")
("Ot" projectile-find-implementation-or-test-other-window "find-implementation-or-test-other-window"))
(global-set-key (kbd "C-c b") 'xzr:hydra-projectile/body)

;; hydra for dumb-jumb
(defhydra xzr:hydra-dumb-jump (:color teal)
  "helm-dumb-jump"
  ("g" dumb-jump-go "go")
  ("p" dumb-jump-go-prompt "prompt for symbol")
  ("o" dumb-jump-go-other-window "go other window")
  ("b" dumb-jump-back "jump back")
  ("l" dumb-jump-quick-look "quick look")
)
(global-set-key (kbd "C-c j") 'xzr:hydra-dumb-jump/body)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; my own emacs extensions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun xzr:other-window-backwards (&optional n)
  "Select Nth previous window."
  (interactive "P")
  (other-window (- (prefix-numeric-value n))))

(global-set-key (kbd "C-x p") 'xzr:other-window-backwards)

;; format c++ code with astyle
(defun xzr:astyle-region (pmin pmax)
  "Executes shell command astyle on region"
  (interactive "r")
  (shell-command-on-region pmin pmax
                           "/opt/local/bin/astyle --style=break --indent=spaces=4 --indent-col1-comments --pad-oper --unpad-paren --align-pointer=type --align-reference=type --convert-tabs --keep-one-line-statements --max-code-length=80 --break-after-logical --lineend=linux"
                           (current-buffer) t
                           (get-buffer-create "*Astyle Errors*") t))

(global-set-key (kbd "C-c f") 'xzr:astyle-region)

;; splits current buffer
;; into 4 quadrants
(fset 'xzr:split-x
      "\C-x3\C-x2\C-xp\C-x2\C-xp\C-xp")
(global-set-key (kbd "C-c x") ' xzr:split-x)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacs-custom config ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes (quote (deeper-blue)))
 '(git-gutter:added-sign ">")
 '(git-gutter:deleted-sign "<")
 '(git-gutter:lighter " GG")
 '(git-gutter:modified-sign "=")
 '(git-gutter:update-interval 2)
 '(package-selected-packages
   (quote
    (plantuml-mode dumb-jump helm-ag use-package flycheck flycheck-irony helm-projectile company-irony-c-headers undo-tree helm-gtags company-irony irony hydra ace-window swiper-helm yasnippet scala-mode powerline ox-reveal ox-asciidoc magit lua-mode jam-mode iedit haskell-mode graphviz-dot-mode go-mode d-mode cider alchemist adoc-mode)))
 '(projectile-mode t nil (projectile))
 '(safe-local-variable-values (quote ((projectile-project-name . "chrono_date")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
