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
;; (2)  Assuming that you have installed SuperCollider, then surely you must have installed
;; the SuperCollider Emacs package. In Debian and Ubuntu: supercollider-emacs.
;; 
;; $ sudo apt install supercollider-emacs
;; 
;; In your ~/.emacs configuration file add:
;; 
;; (require 'sclang)
;; 
;; Evaluate that line or restart Emacs and run the SuperCollider server typing
;; 
;; Alt+x sclang-start ENTER.
;; 
;; Test the audio. In the *SCLang:Workspace* buffer type:
;; 
;; { SinOsc.ar(440, 0, Line.kr(0.3, 0, 1, doneAction:2)) }.play
;; Ctlc-c Ctl-c
;; 
;; You must hear a simple sound. If you don't hear it, something is wrong,
;; there is a problem with SuperCollider configuration or your audio system.
;; 
;; (3) Install FoxDot quark, evaluating the following line in *SCLang:Workspace*
;; buffer:
;; 
;; Quarks.install("FoxDot")
;; 
;; Recompile the SuperCollider class library:
;;
;; Alt+x sclang-recompile
;; 
;; (4) Start FoxDot. In *SCLang:Workspace*, evaluate the following line:
;; 
;; FoxDot.start
;; 
;; SuperCollider is now listening for messages from FoxDot.
;; 
;; (5) Install Emacs FoxDot mode. Clone the foxdot-mode project from git in some
;; directory like "\~/.emacs.d" or any directory in "load-path" list. For example,
;; from the command line, you can create a directory like "\~/.emacs.d/site-lisp/"
;; (mkdir ~/.emacs.d/site-lisp), move to that directory (cd ~/.emacs.d/site-lisp),
;; and clone the repository:
;; 
;; $ git clone https://github.com/superguaricho/foxdot-mode
;; 
;; In your "\~/.emacs" initialization file, add the following lines:
;; 
;; (add-to-list 'load-path (expand-file-name "site-lisp/foxdot-mode" "~/.emacs.d"))
;; (require 'foxdot-mode)
;; 
;; Where «(expand-file-name "site-lisp/foxdot-mode" "~/.emacs.d")» evaluates to
;; «/home/user/.emacs.d/site-lisp/foxdot-mode».
;; 
;; Evaluate those lines or restart Emacs.
;; 
;; (6) Open a file with .py or .foxdot extension.
;; 
;; (7) Start foxdot, typing: Alt+x foxdot ENTER
;; 
;; If you want FoxDot buffer launch when you open "myfile.foxdot", add the
;; following lines to ~/.emacs:
;; 
;; (add-to-list 'auto-mode-alist '("\\.foxdot)?$" . foxdot-mode))
;; (add-hook 'foxdot-mode-hook 'foxdot)
;; 
;; If you do this, don't need use Alt+x foxdot. The FoxDot interpreter will
;; launch when you open a .foxdot file.
;; 
;; That is all.
;; 
;; I have cloned the foxdot-mode repository in "~/.emacs/site-lisp" path and
;; added these lines to my ~/.emacs file:
;; 
;; (add-to-list 'load-path (expand-file-name "site-lisp/foxdot-mode" user-emacs-directory))
;; (require 'foxdot-mode)
;; (add-to-list 'auto-mode-alist '("\\.foxdot)?$" . foxdot-mode))
;; (add-hook 'foxdot-mode-hook 'foxdot-start-foxdot)
;; 
;; Now, when I open a .foxdot file in Emacs, it launchs SuperCollider, start FoxDot,
;; creates a *FoxDot* process and I can write and evaluate my livecoding lines,
;; seting the cursor over the line that I want execute and using the folowing keys:
;; 
;; Ctrl+c Ctrl+c (foxdot-run-line)
;; Ctrl+c Ctrl+g (foxdot-run-line-and-go). This command send a line to the interpreter and
;; advance the cursor to he next non blank line.
;; Ctrl+c e (foxdot-execute-block). Send the paragraphe or block where is the cursor to the interpreter.
;; Ctrl+c Ctrl+e (foxdot-execute-block-and-go). Send the paragraphe or block where is the cursor to the interpreter
;; and go to the next non blank line.
;; Ctrl+c Ctrl+r (foxdot-run-region). Send the selected region to the interpreter.
;; Ctrl+c n (foxdot-run-block-by-lines).  Send a block line by line.
;; Ctrl+c o (foxdot-run-block-by-lines-and-go).  Send a block line by line and go to next non empty line.
;; Ctrl+c Ctrl+a (foxdot-clear-foxdot).  Clear the foxdot interpreter screen.
;; Ctrl+c Ctrl+u (foxdot-hush).  Mute foxdot sending "Clock.clear()" command to the interpreter.
;; 
;; You can start foxdot interpreter with:
;; 
;; Ctrl+c s (foxdot-start-foxdot)
;; 
;; To quit foxdot: Alt+x kill-foxdot ENTER, or:
;; 
;; Ctrl+c q (foxdot-kill-foxdot)
;; 
;; You can work on SuperCollider at same time. If FoxDot is running, you can use:
;; 
;; Ctrl+c 3 (foxdot-set-sc3-layout)
;; 
;; This set a window layout where you can see the *SCLang:Workspace* and the
;; *SCLang:PostBuffer* buffers. You can create your own synths and sounds in sclang
;; language. If you want to see again your original foxdot and *FoxDot* buffers,
;; type:
;; 
;; Ctrl+c f (foxdot-set-foxdot-layout)
;; 
;; I you want to see the foxdot commands echo in *FoxDot* and *SCLang:PostBuffer*
;; buffers, type:
;; 
;; Ctrl+c w (foxdot-sc3-foxdot-layout)
;; 
;; Problems:
;; 
;; This code is in alpha state, is not very tested (June, 2020).
;; 
;; Acknowledgments:
;; 
;; Thanks to Jean Argenty for its ideas and help testing this codes.
;; 
;;; Code:

(require 'python)
(require 'foxdot-scserver)
(require 'foxdot-windows)

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
(defvar foxdot-mode-hook nil "List of functions to run after `foxdot-mode' is enabled.")

;; To avoid the "Can’t guess python-indent-offset" warning.
(if (boundp 'python-indent-guess-indent-offset-verbose)
    (setq python-indent-guess-indent-offset-verbose nil)
  (defvar python-indent-guess-indent-offset-verbose nil)
  )

(setq python-shell-completion-native-enable nil)
(add-to-list 'python-shell-completion-native-disabled-interpreters "python")

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

(defun foxdot-is-char-after (char)
  "Test if CHAR character is before the point."
  (string= (char-to-string (char-after)) char)
  )

(defun foxdot-end-of-line-point ()
  "Return the point of current line."
  (save-excursion
    (end-of-line)
    (point))
  )

(defun foxdot-empty-line-p ()
  "If cursor is in a empty line, return t, else nil."
  (or (save-excursion (beginning-of-line) (re-search-forward "^[ \t]+$" (foxdot-end-of-line-point) t))
      (save-excursion (beginning-of-line) (foxdot-is-char-after "\n")))
  )

(defun foxdot-run-block-by-lines ()
  "Run the block where cursor is, line by line."
  (interactive)
  (save-excursion
    (mark-paragraph)
    (deactivate-mark t)
    (forward-line)
    (pulse-momentary-highlight-region (mark) (point))
    (sit-for 0.25)
    (while (and (not (foxdot-empty-line-p))
                (not (eobp)))
      (foxdot-run-line)
      (forward-line)))
  )

(defun foxdot-run-block-by-lines-and-go ()
  "Run the block where cursor is, line by line and go to next non empty line."
  (interactive)
  (foxdot-run-block-by-lines)
  (forward-paragraph)
  (foxdot-goto-next-non-blank-line)
  )

(defun foxdot-execute-block ()
  "Execute the current block in the interpreter with echo."
  (interactive)
  (let ((current-buffer (current-buffer)))
    (save-excursion
      (mark-paragraph -1)
      (save-selected-window
	(if (use-region-p)
	    (let* ((b (get-buffer foxdot-buffer-name)))
	      (cond (b
		     (append-to-buffer (get-buffer foxdot-buffer-name)
				       (region-beginning)
                                       (region-end))
		     (switch-to-buffer-other-window b)
		     		     (comint-send-input))
		    (t (message "There is not *FoxDot* buffer."))))))
      (pulse-momentary-highlight-region (mark) (point))
      (deactivate-mark t)))
  )

(defun foxdot-execute-block-and-go ()
  "Execute the current block in the interpreter and go to nex non empty line."
  (interactive)
  (foxdot-execute-block)
  (forward-paragraph)
  (foxdot-goto-next-non-blank-line)
  )

;;

(defun foxdot-set-removing-start-hooks ()
  "Set foxdot-mode in current buffer removing sensible hooks when need it.
This avoids launch multiple FoxDot interpreters when you start foxdot."
  (cond ((member 'foxdot-start-foxdot foxdot-mode-hook)
	 (remove-hook 'foxdot-mode-hook 'foxdot-start-foxdot)
	 (foxdot-mode)
	 (add-hook 'foxdot-mode-hook 'foxdot-start-foxdot))
	((member 'start-foxdot foxdot-mode-hook)
	 (remove-hook 'foxdot-mode-hook 'start-foxdot)
	 (foxdot-mode)
	 (add-hook 'foxdot-mode-hook 'start-foxdot))
	((member 'foxdot foxdot-mode-hook)
	 (remove-hook 'foxdot-mode-hook 'foxdot)
	 (foxdot-mode)
	 (add-hook 'foxdot-mode-hook 'foxdot))
	(t (foxdot-mode)))
  )

(defun foxdot-surely-set-mode (b)
  "Set surely 'foxdot-mode' to B."
  (with-current-buffer b (foxdot-set-removing-start-hooks))
  )

(defun foxdot-set-foxdot-mode (&optional b)
  "If B buffer has .py or .foxdot extension, set 'foxdot-mode'.
If you have not passed a buffer B, uses current buffer."
  (interactive)
  (let* ((buf (if b b (current-buffer)))
	 (bufname (buffer-name buf)))
    (when (and (or (string-match "\\.py\\([0-9]\\|[iw]\\)?$" bufname)
		   (string-match "\\.foxdot$" bufname))
	       (not (equal major-mode 'foxdot-mode)))
      (foxdot-surely-set-mode buf)))
  )

(defun foxdot-set-foxdot-in-all-buffers ()
  "Walk through buffers and set foxdot-mode in py buffers."
  (interactive)
  (cl-loop for b in (buffer-list) do (foxdot-set-foxdot-mode b))
  )

(defun foxdot-clear-foxdot ()
  "Clear the *FoxDot window."
  (interactive)
  (when (get-buffer foxdot-buffer-name)
    (with-current-buffer (get-buffer foxdot-buffer-name) (comint-clear-buffer)))
  )

(defun foxdot-python-buffer ()
  "If it is running, return the *Python* buffer."
  (get-buffer "*Python*")
  )

(defun foxdot-init-foxdot-buffer ()
  "Initialize *FoxDot* buffer."
  (when (foxdot-python-buffer)
    (with-current-buffer (foxdot-python-buffer)
      (python-shell-send-string fox-dot-cli-init-string)
      (pop-to-buffer (foxdot-python-buffer))
      (rename-buffer foxdot-buffer-name)
      (comint-clear-buffer)))
  )

;;;###autoload
(defun foxdot-start-foxdot ()
  "Start FoxDot Interpreter."
  (interactive)
  (save-selected-window
    (let ((python-shell-interpreter-args "")
          (fd-code-buffer (get-buffer (buffer-name))))
      (cond ((and (foxdot-python-buffer)
		  (not (get-buffer "*FoxDot*"))
		  (foxdot-init-foxdot-buffer)
		  (foxdot-set-foxdot-in-all-buffers)))
	     ((get-buffer "*FoxDot*") (foxdot-set-foxdot-mode))
	     (t (run-python)
		(sit-for 0.5)
		(foxdot-init-foxdot-buffer)
		(foxdot-set-foxdot-in-all-buffers)
		(add-to-list 'auto-mode-alist '("\\.py\\([0-9]\\|[iw]\\)?$" . foxdot-mode))))))
  )
;;;###autoload
(defalias 'start-foxdot 'foxdot-start-foxdot)
(add-hook 'foxdot-mode-hook '(lambda () (flycheck-mode 0)))
(add-hook 'foxdot-mode-hook 'foxdot-mode-sc3-keybindings)
(add-hook 'foxdot-mode-hook 'foxdot-mode-layout-keybindings)

;;;###autoload
(defun foxdot-super-foxdot ()
  "Run SC3 server and foxdot."
  (interactive)
  (add-hook 'sclang-library-startup-hook 'foxdot-start-sc3-foxdot)
  (foxdot-run-sclang)
  )
;;;###autoload
(defalias 'foxdot 'foxdot-super-foxdot)

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
  (cl-loop for b in (buffer-list) do (foxdot-set-python-mode b))
  )

;;;###autoload
(defun foxdot-kill-foxdot ()
  "Kill csound repl."
  (interactive)
  (let ((b (get-buffer foxdot-buffer-name))
	(c (current-buffer)))
    (if b (with-current-buffer b (kill-buffer-and-window))
      (error "There is not *FoxDot* buffer"))
    (if (sclang-get-process) (sclang-kill) (message "There is not SC3 process runing."))
    (foxdot-set-python-in-all-buffers)
    (switch-to-buffer c)
    (delete-other-windows))
  (if (member '("\\.py\\([0-9]\\|[iw]\\)?$" . foxdot-mode) auto-mode-alist)
      (setq auto-mode-alist (delete '("\\.py\\([0-9]\\|[iw]\\)?$" . foxdot-mode) auto-mode-alist)))
  )
(defalias 'foxdot-quit-foxdot 'foxdot-kill-foxdot)
(defalias 'quit-foxdot 'foxdot-kill-foxdot)

;;;###autoload
(defun foxdot-kill-sc-foxdot ()
  "If running, kill SC3 and foxdot."
  (interactive)
  (if (get-process sclang-process) (foxdot-kill-sc3))
  (foxdot-kill-foxdot)
  )
(defalias 'kill-foxdot 'foxdot-kill-sc-foxdot)

;;

(defun foxdot-mode-keybindings (map)
  "FoxDot keybindings in MAP."
  (define-key map (kbd "C-c C-s") 'foxdot-start-foxdot)
  (define-key map [?\C-c ?\q] 'foxdot-kill-foxdot)
  (define-key map [?\C-c ?\C-c] 'foxdot-run-line)
  (define-key map [?\C-c ?\C-g] 'foxdot-run-line-and-go)
  (define-key map [?\C-c ?\g] 'foxdot-goto-next-non-blank-line)
  (define-key map (kbd "C-c e") 'foxdot-execute-block)
  (define-key map [?\C-c ?\C-e] 'foxdot-execute-block-and-go)
  (define-key map [?\C-c ?\C-r] 'foxdot-run-region)
  (define-key map [?\C-c ?\C-n] 'foxdot-run-block-by-lines)
  (define-key map [?\C-c ?\o] 'foxdot-run-block-by-lines-and-go)
  (define-key map [?\C-c ?\C-u] 'foxdot-hush)
  (define-key map [?\C-c ?\C-a] 'foxdot-clear-foxdot)
  (define-key map [?\C-c ?\l] 'foxdot-load-buffer)

  (define-key map (kbd "C-c s") 'foxdot-run-sclang)
  (define-key map [?\C-c ?\.] 'foxdot-kill-sc3)
  (define-key map (kbd "C-c C-t") 'foxdot-test-sc3)

  (define-key map (kbd "C-c 3") 'foxdot-set-sc3-layout)
  (define-key map (kbd "C-c f") 'foxdot-set-foxdot-layout)
  (define-key map (kbd "C-c w") 'foxdot-sc3-foxdot-layout)
  )

(defun turn-on-foxdot-keybindings ()
  "Foxdot keybindings in the local map."
  (interactive)
  (local-set-key (kbd "C-c C-s") 'foxdot-start-foxdot)
  (local-set-key [?\C-c ?\q] 'foxdot-kill-foxdot)
  (local-set-key [?\C-c ?\C-c] 'foxdot-run-line)
  (local-set-key [?\C-c ?\C-g] 'foxdot-run-line-and-go)
  (local-set-key [?\C-c ?\g] 'foxdot-goto-next-non-blank-line)
  (local-set-key (kbd "C-c e") 'foxdot-execute-block)
  (local-set-key [?\C-c ?\C-e] 'foxdot-execute-block-and-go)
  (local-set-key [?\C-c ?\C-r] 'foxdot-run-region)
  (local-set-key [?\C-c ?\C-n] 'foxdot-run-block-by-lines)
  (local-set-key [?\C-c ?\o] 'foxdot-run-block-by-lines-and-go)
  (local-set-key [?\C-c ?\C-u] 'foxdot-hush)
  (local-set-key [?\C-c ?\C-a] 'foxdot-clear-foxdot)
  (local-set-key [?\C-c ?\l] 'foxdot-load-buffer)
  )
(add-hook 'foxdot-mode-hook 'turn-on-foxdot-keybindings)

(defun foxdot-mode-menu (map)
  "FoxDot menu from MAP."
  (define-key map [menu-bar foxdot]
    (cons "Python-FoxDot" (make-sparse-keymap "FoxDot")))
  (define-key map [menu-bar foxdot quit-foxdot]
    '("Quit FoxDot" . foxdot-kill-foxdot))
  (define-key map [menu-bar foxdot start-foxdot]
    '("Start FoxDot" . foxdot-start-foxdot))
  (define-key map [menu-bar foxdot process-separator]
    '(menu-item "--"))
  (define-key map [menu-bar foxdot run-region]
    '("Run region" . foxdot-run-region))
  (define-key map [menu-bar foxdot run-block-by-lines-and-go]
    '("Run block by lines and go" . foxdot-run-block-by-lines-and-go))
  (define-key map [menu-bar foxdot run-block-by-lines]
    '("Run block by lines" . foxdot-run-block-by-lines))
  (define-key map [menu-bar foxdot execute-block-and-go]
    '("Execute block and go" . foxdot-execute-block-and-go))
  (define-key map [menu-bar foxdot execute-block]
    '("Execute block" . foxdot-execute-block))
  (define-key map [menu-bar foxdot run-line-and-go]
    '("Run line and go" . foxdot-run-line-and-go))
  (define-key map [menu-bar foxdot run-line]
    '("Run line" . foxdot-run-line))
  (define-key map [menu-bar foxdot foxdot-separator]
    '(menu-item "--"))
  (define-key map [menu-bar foxdot kill-sc3]
    '("Kill SC3" . foxdot-kill-sc3))
  (define-key map [menu-bar foxdot run-sclang]
    '("Run SC3" . foxdot-run-sclang))
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
