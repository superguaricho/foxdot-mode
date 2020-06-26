;;; foxdot-init.el --- FoxDot initialization file for Emacs.

;;; Commentary:  Intitial vanilla foxdot-mode configuration

;;; Code:

;; fd-mute

(setq confirm-nonexistent-file-or-buffer nil
      inhibit-splash-screen t
      inhibit-startup-echo-area-message t
      inhibit-startup-message t
      initial-scratch-message nil
      kill-buffer-query-functions (remq 'process-kill-buffer-query-function kill-buffer-query-functions)
      ring-bell-function 'ignore)

(progn
  (show-paren-mode 1)
  (line-number-mode 1)
  (column-number-mode 1)
  (size-indication-mode 1)
  (transient-mark-mode 1)
  (delete-selection-mode 1)
  (global-font-lock-mode 1)
  
  (setq-default cursor-type 'bar)
  (setq tab-width 2)
  (setq scroll-step 1)
  (setq scroll-step 1)
  (setq auto-fill-mode nil)

  (fset 'yes-or-no-p 'y-or-n-p))

;; fd-tools

(require 'recentf)
(setq recentf-max-menu-items 25)
(recentf-mode 1)
(global-set-key "\C-cr" 'recentf-open-files)

(require 'ido)
(ido-mode t)
(setq ido-use-virtual-buffers t)

(defun load-this-buffer ()
  "Load current buffer in Emacs."
  (interactive)
  (with-current-buffer (current-buffer)
    (if (eq major-mode 'emacs-lisp-mode)
	(load (buffer-file-name))))
  )

;; fd-config

(add-to-list 'load-path (expand-file-name "foxdot-mode" user-emacs-directory))
(require 'foxdot-mode)
(add-to-list 'auto-mode-alist '("\\.foxdot)?$" . foxdot-mode))
(add-hook 'foxdot-mode-hook 'foxdot)


;;;###autoload
(defalias 'fd-sc3 'sc3-start-process "Start a sclang process in Emacs.")
  
;; fd-install-quark

;;;###autoload
(defalias 'fd-sc3-install-fd 'sc3-install-fd "Install foxdot quark.")
;;;###autoload
(defalias 'fd-install-fd 'sc3-install-fd "Install foxdot quark.")
;;;###autoload
(defalias 'install-fd 'sc3-install-fd "Install foxdot quark.")
;; (advice-remove (process-filter (get-buffer-process sc3-buffer)) #'fd-sclang-compiled-advice)
;; (advice-remove (process-filter (get-buffer-process sc3-buffer)) #'fd-scterm-advice)
;; (install-fd)

(defun fdkill-sc3 ()
  "Kill sclang process."
  (interactive)
  (with-current-buffer sc3-buffer
    (kill-buffer-and-window))
  )

;; fd-face

(setq initial-frame-alist '((fullscreen . maximized)))

(menu-bar-mode 0)
(defun show-menu-bar ()
  (interactive)
  (menu-bar-mode 1)
  )
(defun hide-menu-bar ()
  (interactive)
  (menu-bar-mode 0)
  )
(global-set-key "\C-cm" 'menu-bar-mode)

(tool-bar-mode -1)
(defun show-tool-bar ()
  (interactive)
  (tool-bar-mode 1)
  )
(defun hide-tool-bar ()
  (interactive)
  (tool-bar-mode -1)
  )
(global-set-key "\C-ct" 'tool-bar-mode)

(defun show-lin-num ()
  (interactive)
  (defvar linum-format "%d ")
  (global-linum-mode t)
  )
(defun hide-lin-num ()
  (interactive)
  (global-linum-mode nil)
  )
(global-set-key "\C-cn" 'global-linum-mode)
 
(menu-bar-mode 0)
(tool-bar-mode -1)
(global-linum-mode)
(setq linum-format "%4d \u2502")

(provide 'foxdot-init.el)
;;; foxdot-init.el ends here
