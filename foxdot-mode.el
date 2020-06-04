;;; foxdot-mode.el -- FoxDot Mode for Emacs.

;; Copyright (C) 2020 numa.tortolero@gmail.com
;; Author: numa.tortolero@gmail.com
;; Homepage: https://github.com/superguaricho/foxdot-mode
;; Version: 0 (alpha)
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
;; This Emacs configuration file allows you to interact with FoxDot for live coding
;; patterns.
;;
;; Requirements: SuperCollider, python with FoxDot library.  You will need too 'python'
;; Emacs package; this comes with your Emacs distribution.
;;
;; Instructions:
;; (1) If you have not doen it, install FoxDot.  From a shell command line do:
;;         $ pip install FoxDot
;;
;;     Start SuperCollider and install FoxDot quark, evaluating the following line:
;;         Quarks.install("FoxDot")
;;
;;     Recompile the SuperCollider class library by going to
;;         Menu -> Language -> Recompile Class Library
;;     or pressing Ctrl+Shift+L.
;;
;; (2) Start FoxDot.
;;     Open SuperCollider and evaluate the following (this needs to be done
;;     before opening FoxDot):
;;         FoxDot.start
;;
;;     SuperCollider is now listening for messages from FoxDot.
;;
;; (3) Install Emacs FoxDot mode.
;;     Put this file, 'foxdot-mode.el', in some directory as '~/.emacs.d' or any directory in
;;     'load-path' list.  For example, you can create a directory as '~/.emacs.d/site-lisp/foxdot-mode',
;;     put 'foxdot.mode.el' there and, in your '~/.emacs' initialization file, add the following two lines:
;;         (add-to-list 'load-path (expand-file-name "site-lisp/foxdot-mode" "~/.emacs.d"))
;;         (require 'foxdot-mode)
;;
;;     Evaluate that line or restart Emacs.
;;
;; (5) Open a file with .py or .foxdot extension.
;;
;; (6) Start foxdot, typing:
;;     Alt+x foxdot ENTER
;;
;; That is all.  Now you can write and evaluate your livecoding lines, seting the cursor over the line
;; that you want execute and using the folowing keys:
;;   Ctrl+c Ctrl+c (foxdot-run-line)
;;   Ctrl+c Ctrl+g (foxdot-run-line-and-go).  This command send a line to the interpreter and
;;                                            advance the cursor to he next non blank line.
;;   Ctrl+c e (foxdot-run-block).  Send the paragraphe or block where is the cursor to the interpreter.
;;   Ctrl+c Ctrl+e (foxdot-run-block).  Send the paragraphe or block where is the cursor to the interpreter
;;                                            and go to the next non blank line.
;;   Ctrl+c Ctrl+e (foxdot-run-region).  Send the selected region to the interpreter.
;;
;; You can start foxdot interpreter with:
;;   Ctrl+c s (foxdot-start-foxdot)
;;
;; To quit foxdot:
;;     Alt+x kill-foxdot ENTER
;; or
;;   Ctrl+c q (foxdot-kill-foxdot)
;;
;; Problems: Is not compatible with elpy.
;;
;; This code is in alpha state, are not very tested, by that reason is not in Melpa (May, 2020)
;;
;; Inspiration: tidal.el (from TidalCycles project), hsc3.el (form hsc3 project) and foxdot-mode.

;;; Code:

(require 'python)

(defvar foxdot-buffer-name "*FoxDot*")

(defvar fox-dot-cli-init-string
  "import os
import cmd
from FoxDot import *

class FoxDotConsole(cmd.Cmd):
    prompt = \"FoxDot> \"
    intro = \"LiveCoding with Python and SuperCollider\"

    def default(self, line):
        execute(line)

if __name__ == \"__main__\":
    FoxDotConsole().cmdloop()
")

(defvar foxdot-mode-map nil)

;;

(defun foxdot-goto-next-non-blank-line ()
  "Move to the nex non-blank line."
  (interactive)
  (goto-char (line-end-position))
  (forward-line)
  (while (and (eolp) (bolp) (not (eobp))) (forward-line))
  )

(declare-function python-shell-send-string "python")
(defun foxdot-run-line ()
  "Send the current line to the interpreter."
  (interactive)
  (let* ((start (line-beginning-position))
         (end (line-end-position))
         (b (current-buffer))
         (s (buffer-substring start end)))
    (pulse-momentary-highlight-one-line (point))
    (sit-for 0.25)
    (python-shell-send-string s (get-process "Python")))
  )

(defun foxdot-run-line-and-go ()
  "Send the current line to the interpreter."
  (interactive)
  (let ((b (current-buffer)))
    (foxdot-run-line)
    (with-current-buffer b
      (foxdot-goto-next-non-blank-line)))
  )

(defun foxdot-hush ()
  "Hush foxdot."
  (interactive)
  (python-shell-send-string "Clock.clear()" (get-process "Python"))
  )

(defun foxdot-run-region ()
  "Send the current block to the interpreter."
  (interactive)
  (if (use-region-p)
      (let* ((s (buffer-substring-no-properties (region-beginning)
                                                (region-end))))
        (python-shell-send-string s (get-process "Python"))))
  (pulse-momentary-highlight-region (mark) (point))
  (deactivate-mark t)
  )

(defun foxdot-run-block ()
  "Send the current block to the interpreter."
  (interactive)
  (save-excursion (mark-paragraph -1) (foxdot-run-region))
  )

(defun foxdot-run-block-and-go ()
  "Send the current block to the interpreter."
  (interactive)
  (mark-paragraph -1)
  (foxdot-run-region)
  (deactivate-mark t)
  (foxdot-goto-next-non-blank-line)
  )

;;

(defun foxdot-set-foxdot-mode (b)
  "Set foxdot mode to B buffer."
  (when (or (string-match "\\.py\\([0-9]\\|[iw]\\)?$" (buffer-name b))
            (string-match "\\.foxdot$" (buffer-name b)))
    (with-current-buffer b
      (if (not (equal major-mode 'foxdot-mode)) (foxdot-mode))
      (turn-on-foxdot-keybindings)))
  )

(defun foxdot-set-foxdot-in-all-buffers ()
  "Walk through buffers and set foxdot-mode in py buffers."
  (interactive)
  (loop for b in (buffer-list) do (foxdot-set-foxdot-mode b))
  )

(defun foxdot-clear-foxdot ()
  "Clear the *FoxDot window."
  (interactive)
  (with-current-buffer "*FoxDot*" (comint-clear-buffer))
  )

(defun foxdot-start-foxdot ()
  "Start FoxDot Interpreter."
  (interactive)
  (let ((python-shell-interpreter-args "")
        (fd-code-buffer (get-buffer (buffer-name))))
    (run-python)
    (python-shell-send-string fox-dot-cli-init-string)
    (python-shell-switch-to-shell)
    (rename-buffer foxdot-buffer-name)
    (sit-for 0.5)
    (with-current-buffer "*FoxDot*" (comint-clear-buffer))
    (switch-to-buffer-other-window fd-code-buffer)
    (foxdot-set-foxdot-in-all-buffers))
  (add-to-list 'auto-mode-alist '("\\.py\\([0-9]\\|[iw]\\)?$" . foxdot-mode))
  )
(defalias 'start-foxdot 'foxdot-start-foxdot)
(defalias 'foxdot 'foxdot-start-foxdot)

;;

(defun foxdot-set-python-mode (b)
  "Set foxdot mode to B buffer."
  (when (string-match "\\.py\\([0-9]\\|[iw]\\)?$" (buffer-name b))
    (with-current-buffer b
      (if (not (equal major-mode 'python-mode)) (python-mode))))
  )

(defun foxdot-set-python-in-all-buffers ()
  "Walk through buffers and set 'python-mode' in python buffers."
  (interactive)
  (loop for b in (buffer-list) do (foxdot-set-python-mode b))
  )

(defun foxdot-kill-foxdot ()
  "Kill csound repl."
  (interactive)
  (let ((b (get-buffer foxdot-buffer-name))
        (c (current-buffer)))
    (if b (with-current-buffer b (kill-buffer-and-window))
      (error "Is not there \"*FoxDot*\" buffer"))
    (foxdot-set-python-in-all-buffers)
    (switch-to-buffer c))
  (if (member '("\\.py\\([0-9]\\|[iw]\\)?$" . foxdot-mode) auto-mode-alist)
      (setq auto-mode-alist (delete '("\\.py\\([0-9]\\|[iw]\\)?$" . foxdot-mode) auto-mode-alist)))
  )
(defalias 'foxdot-quit-foxdot 'foxdot-kill-foxdot)
(defalias 'kill-foxdot 'foxdot-kill-foxdot)
(defalias 'quit-foxdot 'foxdot-kill-foxdot)

;;

(defun foxdot-mode-keybindings (map)
  "FoxDot keybindings in MAP."
  (define-key map [?\C-c ?\s] 'foxdot-start-foxdot)
  (define-key map [?\C-c ?\q] 'foxdot-kill-foxdot)
  (define-key map [?\C-c ?\c] 'foxdot-run-line)
  (define-key map [?\C-c ?\C-g] 'foxdot-run-line-and-go)
  (define-key map [?\C-c ?\g] 'foxdot-goto-next-non-blank-line)
  (define-key map [?\C-c ?\e] 'foxdot-run-block)
  (define-key map [?\C-c ?\C-e] 'foxdot-run-block-and-go)
  (define-key map [?\C-c ?\C-r] 'foxdot-run-region)
  (define-key map [?\C-c ?\C-u] 'foxdot-hush)
  (define-key map [?\C-c ?\l] 'foxdot-load-buffer)
  )

(defun turn-on-foxdot-keybindings ()
  "Foxdot keybindings in the local map."
  (interactive)
  (local-set-key [?\C-c ?\s] 'foxdot-start-foxdot)
  (local-set-key [?\C-c ?\q] 'foxdot-kill-foxdot)
  (local-set-key [?\C-c ?\C-c] 'foxdot-run-line)
  (local-set-key [?\C-c ?\C-g] 'foxdot-run-line-and-go)
  (local-set-key [?\C-c ?\g] 'foxdot-goto-next-non-blank-line)
  (local-set-key [?\C-c ?\e] 'foxdot-run-block)
  (local-set-key [?\C-c ?\C-e] 'foxdot-run-block-and-go)
  (local-set-key [?\C-c ?\C-r] 'foxdot-run-region)
  (local-set-key [?\C-c ?\C-u] 'foxdot-hush)
  (local-set-key [?\C-c ?\l] 'foxdot-load-buffer)
  )
(add-hook 'foxdot-mode-hook 'turn-on-foxdot-keybindings)

(defun foxdot-mode-menu (map)
  "FoxDot menu from MAP."
  (define-key map [menu-bar foxdot]
    (cons "Python-FoxDot" (make-sparse-keymap "FoxDot")))
  (define-key map [menu-bar foxdot run-region]
    '("Run region" . foxdot-run-region))
  (define-key map [menu-bar foxdot run-block]
    '("Run block" . foxdot-run-block))
  (define-key map [menu-bar foxdot run-line]
    '("Run line" . foxdot-run-line))
  (define-key map [menu-bar foxdot run-line-and-go]
    '("Run line and go" . foxdot-run-line-and-go))
  (define-key map [menu-bar foxdot start-foxdot]
    '("Start FoxDot" . foxdot-start-foxdot))
  (define-key map [menu-bar foxdot quit-foxdot]
    '("Quit FoxDot" . foxdot-kill-foxdot))
  )

(unless foxdot-mode-map
  (let ((map (make-sparse-keymap "Python-FoxDot")))
    (foxdot-mode-keybindings map)
    (foxdot-mode-menu map)
    (setq foxdot-mode-map map)))

;;;###autoload
(define-derived-mode
  foxdot-mode
  python-mode
  "Python Foxdot"
  "Major mode for interacting with an inferior FoxDot process."
  (set (make-local-variable 'paragraph-start) "\f\\|[ \t]*$")
  (set (make-local-variable 'paragraph-separate) "[ \t\f]*$")
  (turn-on-font-lock))

(provide 'foxdot-mode)
;;; foxdot-mode.el ends here
