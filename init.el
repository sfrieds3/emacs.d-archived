;;;; package --- summary

;;; Commentary:
;;;     place 'local-settings.el' file (provide 'local-settings)
;;;     in .emacs.d directory to overwrite settings (loaded at end)

;;; Code:

(let ((default-directory  (expand-file-name "lisp" user-emacs-directory)))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;;; backup settings
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list" t)))

;;; history settings
(setq savehist-file (expand-file-name "savehist" user-emacs-directory))
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

(let ((shared-config (expand-file-name "shared-config.el" user-emacs-directory)))
  (when (file-exists-p shared-config)
    (load-file shared-config)))

;;; default directory stuff.. not working properly
;;(setf find-def-directory command-line-default-directory)
;;(add-hook 'find-file-hook
;; (lambda ()
;;  (setq default-directory find-def-directory)
;;  (setq find-name-arg "-iname")))


;;; byte recompile everything
;;;(byte-recompile-directory (expand-file-name "~/.emacs.d/lisp") 0)

;;; theme settings

;;; set default preferred fonts
(defvar platform-default-font)
(setq platform-default-font
      (cond ((eq system-type 'windows-nt) "DejaVu Sans Mono 10")
            ((eq system-type 'gnu/linux) "Iosevka Fixed SS14 11")
            (t nil)))

(when platform-default-font
  (set-frame-font platform-default-font nil t))

(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
(load-theme 'ample t)
(enable-theme 'ample)

;;; start emacsclient if server not running already
(load "server")
(unless (server-running-p) (server-start))

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
;; dont use smart parens in mini-buffers
(defun my-inhibit-electric-pair-mode (char)
  (minibufferp))

(setq electric-pair-inhibit-predicate #'my-inhibit-electric-pair-mode)

;;; turn on recent file mode
(recentf-mode t)
(setq recentf-max-saved-items 50)

;;; filename in titlebar
(setq frame-title-format
      (concat "%f@" system-name))

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

(vc-mode -1)
 
;;; LOAD INIT FILES
(require 'defun)
(require 'evil-config)
(require 'company-config)
(require 'ido-config)
(require 'smex-config)
(require 'org-config)
(require 'uniquify-config)
(require 'elpy-config)
(require 'slime-config)
(require 'keybindings)

;; PACKAGES

(require 'which-key)
(which-key-mode)

;;; LANGUAGE SETTINGS
;;; c++
(defun my/c++-mode-hook ()
  "C++ mode stuff."
  (defvar c-basic-offset)
  (setq c-basic-offset 4)
  (c-set-offset 'substatement-open 0))
(add-hook 'c++-mode-hook 'my/c++-mode-hook)

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
