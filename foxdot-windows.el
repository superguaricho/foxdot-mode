;;; foxdot-windows.el --- Codes to handle windows when using foxdot-mode
;; 
;; Copyright (C) 2020 numa.tortolero@gmail.com
;; Author: numa.tortolero@gmail.com
;; Homepage: https://github.com/superguaricho/foxdot-mode
;; Version: 0 (alpha)
;; Keywords: tools
;; Package-Requires: ((emacs "24"))
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary:
;; 
;; This file put together some functions to handle three windows layouts:
;; - Normal foxdot layout: two windows: current work buffer and foxdot
;;   interpreter.
;; - SC3 layout: two windows: SCLang:WorkSpace and SCLang:PostBuffer.
;; - foxdot-sc3 layout: three windows: workspace, foxdot interpreter,
;;   and SCLang:PostBuffer.
;;
;; When you start foxdot with foxdot function, you get two windows:
;; your workspace and the foxdot interpreter. You can change to
;; SC3 layout if you want work a while in SC3 using `foxdot-set-sc3-layout'
;; command. If you want normal foxdot layout again, use the
;; `foxdot-set-foxdot-layout'. If you want to see the SC3 server echo
;; while you work on foxdot, use `foxdot-set-foxdot-layout' command.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

;;
;; Configure the windows display
;;

(require 'windmove)
(require 'comint)

(defvar foxdot-sc3-buffer sclang-post-buffer
  "*The name of the foxdot process buffer (default=sclang-post-buffer).")

(defvar foxdot-buffer "*FoxDot*")

(declare-function windmove-find-other-window "windmove")
(defun foxdot-get-window-in-frame (x y &optional frame)
  "Find Xth horizontal and Yth vertical window from top-left of FRAME."
  (let ((orig-x x) (orig-y y)
        (w (frame-first-window frame)))
    (while (and (windowp w) (> x 0))
      (setq w (windmove-find-other-window 'right 1 w)
            x (1- x)))
    (while (and (windowp w) (> y 0))
      (setq w (windmove-find-other-window 'down 1 w)
            y (1- y)))
    (unless (windowp w)
      (error "No window at (%d, %d)" orig-x orig-y))
    w)
  )

(defun foxdot-set-window-buffer-in-frame (x y buffer &optional frame)
  "Set Xth horizontal and Yth vertical window to BUFFER from top-left of FRAME."
  (set-window-buffer (foxdot-get-window-in-frame x y frame) buffer)
  )

(defun foxdot-get-sc3-buffer ()
  "Return foxdot buffer or nil."
  (get-buffer foxdot-sc3-buffer)
  )

(defun foxdot-bring-sc3-post-buffer ()
  "Place sc3 post buffer in selected window."
  (interactive)
  (let ((b (foxdot-get-sc3-buffer)))
    (when b (set-window-buffer (selected-window) b)))
  )

(defun foxdot-get-foxdot-buffer ()
  "Return foxdot buffer or nil."
  (get-buffer foxdot-buffer)
  )

(defun foxdot-bring-foxdot-buffer ()
  "Place foxdot buffer in selected window."
  (interactive)
  (let ((b (foxdot-get-foxdot-buffer)))
    (when b (set-window-buffer (selected-window) b)))
  )

(defun foxdot-get-buffer-window (buffer-name)
  "Get the window where is BUFFER-NAME buffer."
  (let ((b (get-buffer buffer-name)))
    (if b (get-buffer-window b t) (foxdot-sc3-foxdot-layout)))
  )

(defun foxdot-set-foxdot-window ()
  "If exists the foxdot buffer, set the *foxdot-window* variable."
  (interactive)
  (defvar *foxdot-window*)
  (setq *foxdot-window* (foxdot-get-buffer-window "*foxdot*"))
  )

(defun foxdot-set-sc3-window ()
  "If exists the foxdot buffer, set the *sc3-window* variable."
  (interactive)
  (defvar *sc3-window*)
  (setq *sc3-window* (foxdot-get-buffer-window "*SCLang:PostBuffer*"))
  )

(defun foxdot-place-sc3-buffer ()
  "Place sc3 bufer at right and bottom in frame."
  (if (and (comint-check-proc foxdot-sc3-buffer)
           (comint-check-proc foxdot-buffer))
      (with-current-buffer (current-buffer)
	(delete-other-windows)
	(foxdot-sc3-foxdot-layout)
	(foxdot-set-window-buffer-in-frame 0 1 (get-buffer foxdot-buffer))
	(other-window 2)
	(switch-to-buffer foxdot-sc3-buffer)
	(other-window -2))
    (message "*FoxDot* buffer or *SCLang:Postbuffer* does not exist."))
  )

(defun foxdot-hide-sc3-buffer ()
  "Hide *SCLang:PostBuffer*."
  (interactive)
  (let ((b (get-buffer "*SCLang:PostBuffer*")))
    (if b
	(with-current-buffer b
	  (delete-window (get-buffer-window b)))))
  )

(declare-function sclang-switch-to-workspace "sclang-interp")
(defun foxdot-show-sc-workspace-buffer ()
  "Show the *SCLang:Workspace* buffer.  If does not exist, create it."
  (interactive)
  (sclang-switch-to-workspace)
  )

(defun foxdot-set-sc3-layout ()
  "Set SC3 layout."
  (interactive)
  (delete-other-windows)
  (sclang-switch-to-workspace)
  (split-window-below)
  (foxdot-set-window-buffer-in-frame 0 1 (get-buffer "*SCLang:PostBuffer*"))
  )

(defun foxdot-set-foxdot-layout ()
  "Set SC3 layout."
  (interactive)
  (sclang-switch-to-workspace)
  (switch-to-prev-buffer)
  (delete-other-windows)
  (split-window-below)
  (foxdot-set-window-buffer-in-frame 0 1 (get-buffer "*FoxDot*"))
  )

(defun foxdot-sc3-foxdot-layout ()
  "Bring back 3x3 window configuration with my favorite buffers."
  (interactive)
  (if (and (foxdot-get-foxdot-buffer) (foxdot-get-sc3-buffer))
      (save-selected-window
	(delete-other-windows)
	(split-window-below)
	(other-window 1)
	(foxdot-bring-foxdot-buffer)
	(split-window-right)
	(other-window 1)
	(foxdot-bring-sc3-post-buffer))
    (message "*FoxDot* buffer or *SCLang:Postbuffer does not exist."))
  (if (eq (current-buffer) (get-buffer "*SCLang:Workspace*"))
      (switch-to-prev-buffer))
  )

(defun foxdot-mode-layout-keybindings ()
  "FoxDot keybindings in MAP."
  (local-set-key (kbd "C-c 3") 'foxdot-set-sc3-layout)
  (local-set-key (kbd "C-c f") 'foxdot-set-foxdot-layout)
  (local-set-key (kbd "C-c w") 'foxdot-sc3-foxdot-layout)
  )
(add-hook 'sclang-mode-hook 'foxdot-mode-layout-keybindings)

(provide 'foxdot-windows)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; foxdot-windows.el ends here
