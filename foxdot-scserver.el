;;; foxdot-scserver.el --- Codes to work with supercollider in foxdot-mode
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
;; This file includes codes to handle SC3 process from `foxdot-mode'.
;; You can start only SC3 server using `foxdot-run-sclang' command,
;; and you can kill only SC3 calling `foxdot-kill-sc3' command.
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

(require 'sclang)
(require 'foxdot-windows)

;;
;; SC3 process variables
;;

(declare-function sclang-start "sclang-interp")
(declare-function sclang-eval-expression "sclang-interp")
(declare-function sclang-eval-string "sclang-interp")

(defvar foxdot-sc3-interpreter
  "scsynth"
  "*The foxdot scsynth (default=scsynth).")

(defvar foxdot-sc3-process sclang-process
  "*The foxdot scsynth process (default=sclang-process).")

(defvar foxdot-sc3-interpreter-arguments
  (list "-u 57110")
  "*Arguments to the scsynth interpreter (default=none).")

;;
;; SC3 server
;;

(defun foxdot-test-sc3 ()
  "Test if foxdot system is running."
  (interactive)
  (sclang-eval-string "{ SinOsc.ar(440, 0, Line.kr(0.3, 0, 1, doneAction:2)) }.play")
  (sclang-eval-string "\"\nSuperCollider server is running!\".postln;")
  (when (member 'foxdot-test-sc3 sclang-library-startup-hook)
    (remove-hook 'sclang-library-startup-hook 'foxdot-test-sc3))
  )

(defun foxdot-start-sc3-foxdot ()
  "Start SC3 FoxDot."
  (remove-hook 'sclang-library-startup-hook 'foxdot-start-sc3-foxdot)
  (sclang-eval-string "s.boot;")
  (sclang-eval-string "FoxDot.start;")
  (sit-for 1)
  (foxdot-test-sc3)
  (foxdot-start-foxdot)
  )

(defun foxdot-sclang-on-library-startup ()
  "What run on SC3 library startup."
    (sclang-message "Initializing library...")
    (setq sclang-library-initialized-p t)
    (run-hooks 'sclang-library-startup-hook)
    (sclang-message "Initializing library...done")
    (if (eq (get-buffer sclang-workspace-buffer) (current-buffer))
	(switch-to-prev-buffer))
    (sclang-set-command-handler '_init (lambda (arg) (sclang-on-library-startup)))
  )

(declare-function sclang-get-process "sclang-interp")
(declare-function foxdot-reset-windows "foxdot-windows")
(declare-function foxdot-set-window-buffer-in-frame "foxdot-set-window-buffer-in-frame")
(defun foxdot-run-sclang ()
  "Run the SuperCollider server."
  (interactive)
  (let ((b (current-buffer)))
    (with-current-buffer b
      (unless (sclang-get-process)
	(sclang-set-command-handler '_init (lambda (arg) (foxdot-sclang-on-library-startup)))
	(sclang-start)
	(foxdot-set-window-buffer-in-frame 0 0 b)
	(pop-to-buffer b))))
  )

(defun foxdot-quit-sc3 ()
  "Quit 'foxdot-sc3-buffer'."
  (interactive)
  (if (get-buffer foxdot-sc3-buffer) (kill-buffer foxdot-sc3-buffer))
  (if (get-buffer "*SCLang:Workspace*") (kill-buffer "*SCLang:Workspace*"))
  )

(defun foxdot-kill-sc3 ()
  "If is running, kill the sc3 process and delete its window."
  (interactive)
  (let* ((p (get-process sclang-process)))
    (if p (sclang-kill))
    (foxdot-quit-sc3))
  )

(defun foxdot-mode-sc3-keybindings ()
  "FoxDot keybindings in MAP."
  (local-set-key (kbd "C-c s") 'foxdot-run-sclang)
  (local-set-key  [?\C-c ?\.] 'foxdot-kill-sc3)
  (local-set-key  (kbd "C-c C-t") 'foxdot-test-sc3)
  )

(provide 'foxdot-scserver)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; foxdot-scserver.el ends here
