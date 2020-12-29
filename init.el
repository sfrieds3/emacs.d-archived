;;;; init.el --- scwfri init.el

;;; Commentary:
;;     place 'local-settings.el' file (provide 'local-settings)
;;     in .emacs.d directory to overwrite settings (loaded at end)

;;; Code:

(let ((default-directory  (expand-file-name "lisp" user-emacs-directory)))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; use-package to manage packages
(eval-when-compile
  (require 'use-package))

;;; backup settings
(setq backup-directory-alist '(("." . "~/.emacs.d/tmp/")))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/tmp/" t)))

;;; history settings
(setq savehist-file (expand-file-name "savehist" "~"))
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))

(let ((home-settings (expand-file-name "home.el" user-emacs-directory)))
  (when (file-exists-p home-settings)
    (load-file home-settings)))

(setq custom-file (concat user-emacs-directory "shared-config.el"))

;;; byte recompile everything
;;;(byte-recompile-directory (expand-file-name "~/.emacs.d/lisp") 0)

;;; start emacsclient if server not running already
;;(load "server")
;;(unless (server-running-p) (server-start))

;;; make scrolling work like it should
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)

;;; cursor always blinks
(setq blink-cursor-blinks -1)

;;; visuals
(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;;; inhibit visual bells
(setq visible-bell nil
      ring-bell-function #'ignore)

;;; highlight current line
(global-hl-line-mode -1)
(defvar hl-line-face)
(set-face-attribute hl-line-face nil :underline nil)

;;; spaces by default instead of tabs!
(setq-default indent-tabs-mode nil)

;;; show matching parens
(show-paren-mode 1)

;;; smart parens
(electric-pair-mode -1)
(defun $inhibit-electric-pair-mode (char)
  "Do not use smart parens in mini-buffers.  Params: CHAR."
  (minibufferp))

(setq electric-pair-inhibit-predicate #'$inhibit-electric-pair-mode)

;;; turn on recent file mode
(recentf-mode t)
(setq recentf-max-saved-items 1000)

;;; allow recursive minibuffers
(setq enable-recursive-minibuffers t)

;;; filename in titlebar
(setq frame-title-format
      (concat user-login-name "@" (system-name) ":%f [%m]"))

;;; C-x w h [REGEX] <RET> <RET> to highlight all occurances of [REGEX], and C-x w r [REGEX] <RET> to unhighlight them again.
(global-hi-lock-mode 1)

;;; stop asking about upcase and downcase region
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;; advanced management of rectangles
;; C-RET to mark a corner
;; M-r to do revexp replace within marked rectange
;; C-RET to unmark/exit rectangle editing
(cua-selection-mode 1)

;;; transient mark mode
(transient-mark-mode 1)

;;; dependencies
(use-package annalist              :defer t)
(use-package dash                  :defer t)
(use-package f                     :defer t)
(use-package highlight-indentation :defer t)
(use-package ht                    :defer t)
(use-package hydra                 :defer t)
(use-package popup                 :defer t)
(use-package pyvenv                :defer t)
(use-package s                     :defer t)
(use-package spinner               :defer t)

;;; evil
(use-package evil
  :init
  (setf evil-want-keybinding 'nil)
  :config
  (add-hook 'python-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
  (add-hook 'ruby-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
  (add-hook 'js2-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
  (setq evil-search-module 'evil-search)
  (setq evil-ex-complete-emacs-commands t)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-shift-round nil)
  (setq evil-want-C-u-scroll t)
  (evil-mode 1)
  (setq-default evil-cross-lines t)

  (defun $evil-clear-highlights ()
    "Clear highlight from evil-search."
    (interactive)
    (evil-ex "nohl")
    (exit-minibuffer))

  (defun $evil-star-keep-position ()
    "Keep position when searching."
    (interactive)
    (evil-search-word-forward)
    (evil-search-previous))

  (defun $evil-visualstar-keep-position ()
    "Keep current position on visual star search."
    (interactive)
    (when (region-active-p)
      (evil-search-word-forward)
      (evi-search-word-backward)
      (cua-cancel)))

  (defun $evil-set-jump-args (&rest ns)
    "Preserve jump list with dumb-jump.  NS args."
    (evil-set-jump))
  (advice-add 'dumb-jump-goto-file-line :before #'$evil-set-jump-args)

  (defun $evil-split-right-and-move ()
    "Split window to the right and move to it."
    (interactive)
    (split-window-right)
    (evil-window-right 1))

  (defun $evil-scroll-down-keep-pos ()
    (interactive)
    (evil-scroll-line-down 1)
    (evil-next-visual-line))

  (defun $evil-scroll-up-keep-pos ()
    (interactive)
    (evil-scroll-line-up 1)
    (evil-previous-visual-line))

  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
  (define-key evil-normal-state-map (kbd "u") '$simple-undo)
  (define-key evil-normal-state-map (kbd "C-r") '$simple-redo)
  (define-key evil-normal-state-map (kbd "C-l") 'evil-ex-nohighlight)
  (define-key evil-normal-state-map (kbd "C-]") 'counsel-etags-find-tag-at-point)
  (define-key evil-normal-state-map (kbd "\\w") 'delete-trailing-whitespace)
  (define-key evil-normal-state-map (kbd "_w") '$toggle-show-trailing-whitespace)
  (define-key evil-normal-state-map (kbd "\\f") 'find-name-dired)
  (define-key evil-normal-state-map (kbd "\\b") 'buffer-menu)
  (define-key evil-normal-state-map (kbd "\\h") 'highlight-symbol-at-point)
  (define-key evil-normal-state-map (kbd "\\H") 'unhighlight-regexp)
  (define-key evil-normal-state-map (kbd "\\c") 'global-hl-line-mode)
  (define-key evil-normal-state-map (kbd "\\C") 'column-marker-1)
  (define-key evil-normal-state-map (kbd "DEL") 'evil-switch-to-windows-last-buffer)
  (define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)
  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-normal-state-map (kbd "*") '$evil-star-keep-position)
  (define-key evil-normal-state-map (kbd "]b") 'evil-next-buffer)
  (define-key evil-normal-state-map (kbd "[b") 'evil-prev-buffer)
  (define-key evil-normal-state-map (kbd "\\\\") 'consult-imenu)
  (define-key evil-normal-state-map (kbd "gb") 'evil-next-buffer)
  (define-key evil-normal-state-map (kbd "gB") 'evil-prev-buffer)
  (define-key evil-normal-state-map (kbd "\\pt") 'counsel-etags-list-tag)
  (define-key evil-normal-state-map (kbd "\\pT") 'list-tags)
  (define-key evil-normal-state-map (kbd "\\pr") '$ido-open-recentf)
  (define-key evil-normal-state-map (kbd "\\pb") 'consult-buffer)
  (define-key evil-normal-state-map (kbd "_f") '$show-full-file-path)
  (define-key evil-normal-state-map (kbd "SPC") 'counsel-grep)
  (define-key evil-normal-state-map (kbd "gr") 'projectilel-grep)
  (define-key evil-normal-state-map (kbd "C-j") '$evil-scroll-down-keep-pos)
  (define-key evil-normal-state-map (kbd "C-k") '$evil-scroll-up-keep-pos)

  (define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-visual-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-visual-state-map (kbd "k") 'evil-previous-visual-line)
  (define-key evil-visual-state-map (kbd "gl") 'align-regexp)
  ;;(define-key evil-visual-state-map (kbd "*") '$visualstar-keep-position)

  (define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)
  (define-key evil-insert-state-map (kbd "C-u")
    (lambda ()
      (interactive)
      (evil-delete (point-at-bol) (point))))

  (evil-ex-define-cmd "Q" 'evil-quit)
  (evil-ex-define-cmd "E" 'evil-edit)
  (evil-ex-define-cmd "W" 'evil-write)
  (evil-ex-define-cmd "vs" '$evil-split-right-and-move)
  (evil-ex-define-cmd "Vs" '$evil-split-right-and-move)

  (define-key evil-normal-state-map (kbd "|") 'universal-argument)
  (define-key universal-argument-map (kbd "|") 'universal-argument-more)
  (define-key universal-argument-map (kbd "C-u") nil))

;;; evil-numbers
(use-package evil-numbers)

;;; evil-collection
(use-package evil-collection
  :commands (evil-collection-init)
  :init
  (evil-collection-init))

;;; evil-visualstar
(use-package evil-visualstar
  :config
  (global-evil-visualstar-mode))

;;; evil-surround
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package undohist
  :defer t)

;;; goto-chg
(use-package goto-chg)

;;; projectile
(use-package projectile
  :config
  (projectile-mode)
  (setq projectile-use-git-grep t)
  (define-key projectile-mode-map (kbd "M-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;;; consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (("C-s" . consult-line)
         ("M-s s" . consult-line-symbol-at-point)
         ("C-x M-:" . consult-complex-command)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r x" . consult-register)
         ("C-x r b" . consult-bookmark)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)     ;; "M-s o" is a good alternative.
         ("M-g l" . consult-line)        ;; "M-s l" is a good alternative.
         ("M-g m" . consult-mark)        ;; I recommend to bind Consult navigation
         ("M-g k" . consult-global-mark) ;; commands under the "M-g" prefix.
         ("M-g i" . consult-imenu)
         ("M-g e" . consult-error)
         ("M-s m" . consult-multi-occur)
         ("M-y" . consult-yank-pop)
         ("<help> a" . consult-apropos))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Replace `multi-occur' with `consult-multi-occur', which is a drop-in replacement.
  (fset 'multi-occur #'consult-multi-occur)

  ;; Configure other variables and modes in the :config section, after lazily loading the package
  :config

  ;; Optionally configure narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key (kbd "C-+"))
  ;; Optionally make narrowing help available in the minibuffer.
  ;; Probably not needed if you are using which-key.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; Optional configure a view library to be used by `consult-buffer'.
  ;; The view library must provide two functions, one to open the view by name,
  ;; and one function which must return a list of views as strings.
  ;; Example: https://github.com/minad/bookmark-view/
  ;; (setq consult-view-open-function #'bookmark-jump
  ;;       consult-view-list-function #'bookmark-view-names)

  ;; Optionally enable previews. Note that individual previews can be disabled
  ;; via customization variables.
  (consult-preview-mode))

;;; consult-selectrum
(use-package consult-selectrum
  :demand t)

;;; consult-flycheck
(use-package consult-flycheck
  :bind (:map flycheck-command-map
         ("!" . consult-flycheck)))

(use-package selectrum
  :after selectrum-prescient
  :config
  (selectrum-mode)
  (global-set-key (kbd "C-c C-r") #'selectrum-repeat))

(use-package prescient)

(use-package selectrum-prescient
  :after prescient
  :config
  (selectrum-prescient-mode))

(use-package company-prescient
  :after prescient
  :config
  (company-prescient-mode))

;;; counsel-etags
(use-package counsel-etags
  :bind (("C-]" . counsel-etags-find-tag-at-point))
  :init
  (add-hook 'prog-mode-hook
        (lambda ()
          (add-hook 'after-save-hook
            'counsel-etags-virtual-update-tags 'append 'local)))
  :config
  (setq counsel-etags-update-interval 60)
  (setq tags-revert-without-query t)
  (setq large-file-warning-threshold nil)
  (push "TAGS" counsel-etags-ignore-directories)
  (push "tmp" counsel-etags-ignore-directories)
  (push "bin" counsel-etags-ignore-directories)
  (push "build" counsel-etags-ignore-directories))

;;; smex
(use-package smex
  :disabled
  :commands (smex smex-initialize)
  :defer t
  :config
  (smex-initialize)
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ;; This is your old M-x.
         ("C-c C-c M-x" . execute-extended-command)))

;;; company
(use-package company
  :demand t
  :commands (global-company-mode company-mode company-indent-or-complete-common)
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-backends '((company-files company-keywords company-capf company-dabbrev-code company-etags company-dabbrev)))
  (global-company-mode 1))

;;; flycheck
(use-package flycheck
  :defer 5
  :config
  (global-flycheck-mode))

;;; elpy
(use-package elpy
  :demand t
  :commands (elpy-enable)
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :config
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (setq elpy-modules (delq 'elpy-module-yasnippet elpy-modules))
  (add-hook 'elpy-mode-hook (lambda () (highlight-indentation-mode -1)))
  (add-hook 'python-mode-hook (lambda()
                                (set (make-local-variable 'company-backends)
                                     (list 'elpy-company-backend 'company-backends))))
  :hook
  (elpy-mode-hook . flycheck-mode))

;;; yasnippet
(use-package yasnippet
  :defer t)

;;; slime
(use-package slime
  :defer t
  :commands (slime)
  :config
  (defun my/slime-keybindings ()
    "keybindings for use in slime"
    (local-set-key (kbd "C-c e") 'slime-eval-last-expression)
    (local-set-key (kbd "C-c b") 'slime-eval-buffer))
  (add-hook 'slime-mode-hook #'my/slime-keybindings)
  (add-hook 'slime-repl-mode-hook #'my/slime-keybindings)
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (setq slime-contribs '(slime-fancy)))

;;; markdown-mode
(use-package markdown-mode
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;;; dumb-jump
(use-package dumb-jump
  :defer t
  :config
  (add-to-list 'xref-backend-functions 'dumb-jump-xref-activate t) )

;;; uniquify
(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'forward)
  (uniquify-separator "/")
  ;; rename after killing unqualified
  (uniquify-after-kill-buffer-p t)
  ;; dont change names of special buffers
  (uniquify-ignore-buffers-re "^\\*"))

;;; which-key
(use-package which-key
  :config
  (which-key-mode))

;;; origami
(use-package origami
  :defer 5
  :commands (global-origami-mode)
  :init
  (global-origami-mode 1))

;;; column-marker
(use-package column-marker)

;;; esup
(use-package esup
  :config
  (setq esup-depth 0))

;;; LOAD INIT FILES
(use-package scwfri-defun)
(use-package theme-config)
(use-package scwfri-config)
(use-package modeline)
(use-package keybindings)

;;; LANGUAGE SETTINGS

;;; c++
(defun $c++-mode-hook ()
  "C++ mode stuff."
  (defvar c-basic-offset)
  (setq c-basic-offset 4)
  (c-set-offset 'substatement-open 0))
(add-hook 'c++-mode-hook '$c++-mode-hook)

;;; eldoc mode
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
(add-hook 'ielm-mode-hook 'eldoc-mode)

;;; LOAD LOCAL SETTINGS
(let ((local-settings (expand-file-name "local-settings.el" user-emacs-directory)))
  (when (file-exists-p local-settings)
    (load-file local-settings)))

(provide 'init.el)
;;; init.el ends here
