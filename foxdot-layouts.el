;;; foxdot-layouts.el -- Layout windows handler for FoxDot.

;; Copyright (C) 2020 numa.tortolero@gmail.com
;; Author: numa.tortolero@gmail.com
;; Homepage: https://github.com/superguaricho/foxdot-mode
;; Version: 0.03 (alpha)
;; Keywords: tools
;; Package-Requires: ((emacs "24"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Layout windows handler for FoxDot.

;;; Code:

(require 'windmove)

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
  (get-buffer "*SC3:SCLang*")
  )

(defun foxdot-bring-sc3-buffer ()
  "Place sc3 post buffer in selected window."
  (interactive)
  (let ((b (foxdot-get-sc3-buffer)))
    (when b (set-window-buffer (selected-window) b)))
  )

(defun foxdot-get-foxdot-buffer ()
  "Return foxdot buffer or nil."
  (get-buffer "*FoxDot*")
  )

(defun foxdot-bring-foxdot-buffer ()
  "Place foxdot buffer in selected window."
  (interactive)
  (let ((b (foxdot-get-foxdot-buffer)))
    (when b (set-window-buffer (selected-window) b)))
  )

(defun foxdot-bring-python-buffer ()
  "Place python buffer in selected window."
  (interactive)
  (let ((b (get-buffer "*Python*")))
    (when b (set-window-buffer (selected-window) b)))
  )

(defun foxdot-hide-buffer (buffer)
  "Hide BUFFER."
  (let ((b (get-buffer buffer)))
    (if b
	(with-current-buffer b
	  (delete-window (get-buffer-window b)))))
  )

(defun foxdot-hide-sc3-buffer ()
  "Hide *SC3:SCLang*."
  (interactive)
  (foxdot-hide-buffer "*SC3:SCLang*")
  )

(defun foxdot-hide-foxdot-buffer ()
  "Hide *FoxDot*."
  (interactive)
  (foxdot-hide-buffer "*FoxDot*")
  )

(defun foxdot-delete-split-window ()
  "Delete other windows and split the current."
  (delete-other-windows)
  (split-window-below)
  )

(defun foxdot-set-two-win-layout (buffer)
  "Set two windows layout, with BUFFER at botton."
  (when (get-buffer buffer)
    (foxdot-delete-split-window)
    (foxdot-set-window-buffer-in-frame 0 1 (get-buffer buffer)))
  (unless (get-buffer buffer)
    (message "Does not exist %s buffer" buffer))
  )

(defun foxdot-set-sc3-layout ()
  "Set SC3 layout."
  (interactive)
  (save-selected-window (foxdot-set-two-win-layout "*SC3:SCLang*"))
  )

(defun foxdot-set-foxdot-layout ()
  "Set foxdot layout."
  (interactive)
  (let ((b (or (get-buffer "*FoxDot*") (get-buffer "*Python*"))))
    (if b (save-selected-window (foxdot-set-two-win-layout b))))
  )

(defun foxdot-sc3-foxdot-layout (&optional b)
    "Bring back 3x3 window configuration with my favorite buffers.
B is a buffer that you want in top left most window."
  (interactive)
  (let ((b (or b (current-buffer)))
	(p (or (get-buffer "*Python*") (get-buffer "*FoxDot*")))
	(s (get-buffer "*SC3:SCLang*")))
    (when (and (or p s))
      (switch-to-buffer b)
      (delete-other-windows)
      (display-buffer p '((display-buffer-below-selected) (inhibit-same-window . t)))
      (with-selected-window (get-buffer-window p)
	(display-buffer s '((display-buffer-below-selected) (inhibit-same-window . t))))))
  )

(defun foxdot-mode-layout-keybindings ()
  "FoxDot keybindings in MAP."
  (local-set-key (kbd "C-c 3") 'foxdot-sc3-foxdot-layout)
  (local-set-key (kbd "C-c C-w") 'foxdot-set-foxdot-layout)
  (local-set-key (kbd "C-c w") 'foxdot-set-sc3-layout)
  )
(add-hook 'sc3-mode-hook 'foxdot-mode-layout-keybindings)
(add-hook 'foxdot-mode-hook 'foxdot-mode-layout-keybindings)

(provide 'foxdot-layouts)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; foxdot-layouts.el ends here
