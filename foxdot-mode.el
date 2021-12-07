;;; foxdot-mode.el -- FoxDot Mode for Emacs.

;; Copyright (C) 2020 numa.tortolero@gmail.com
;; Author: numa.tortolero@gmail.com
;; Homepage: https://github.com/superguaricho/foxdot-mode
;; Version: 1.01 (alpha)
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
;; (2) Install Emacs FoxDot mode.
;; Clone the foxdot-mode project from git in some directory like "\~/.emacs.d" or
;; any directory in "load-path" list.  For example, from the command line, you can
;; create a directory like "\~/.emacs.d/site-lisp/" (mkdir ~/.emacs.d/site-lisp),
;; move to that directory (cd ~/.emacs.d/site-lisp), and clone the repository:
;;
;; $ git clone https://github.com/superguaricho/foxdot-mode
;;
;; In your "\~/.emacs" initialization file, add the following lines:
;;
;;   (add-to-list 'load-path (expand-file-name "site-lisp/foxdot-mode" "~/.emacs.d"))
;;   (require 'foxdot-mode)
;;
;; Where «(expand-file-name "site-lisp/foxdot-mode" "~/.emacs.d")» evaluates to
;; «/home/user/.emacs.d/site-lisp/foxdot-mode».
;;
;; Evaluate those lines or restart Emacs.
;;
;; (3) Install the SuperCollider FoxDot quark.
;;
;; Assuming that you have installed SuperCollider, from Emacs do Alt-x install-foxdot-quark
;;
;; This run sclang in a Emacs buffer and, if you are in line, will install FoxDot quark.
;;
;; Recompile the SuperCollider class library, type: Alt+x recompile-classlib ENTER.
;;
;; Start the FoxDot quark, type; Alt+x foxdot-quark-start ENTER.
;;
;; Test the audio, type: Alt+x test-audio ENTER.
;;
;; You must hear a simple sound.  If you don't hear it, something is wrong,
;; there is a problem with SuperCollider configuration or in your audio system.
;;
;; Kill sclang process: Ctrl+c k, or type Alt+x sc3-kill-process
;;
;; (4) Start foxdot.
;; Open a file with .foxdot extension.  Type Alt+x foxdot ENTER.
;; This run sclang and FoxDot process buffers.  Wait and you will see three horizontal
;; windows: the .foxdot file (your workspace), the  *FoxDot* and the *SCLang:SC3* buffers.
;;
;; (5) Play with some codes.
;; For example, type in your workspace:
;;
;; p1 >> pluck([12], dur=0.25, echo=0.8)
;;
;; With the cursor over the line type Ctrl+c Ctrl+c.
;;
;; Do Ctrl+c Ctrl+u to stop the sounds.
;;
;; ;;
;;
;; If you want FoxDot buffer launch when you open any "myfile.foxdot", add the
;; following lines to ~/.emacs:
;;
;; (add-to-list 'auto-mode-alist '("\\.foxdot)?$" . foxdot-mode))
;; (add-hook 'foxdot-mode-hook 'foxdot)
;;
;; If you do this, don't need use Alt+x foxdot.  The FoxDot interpreter will
;; launch anytime you open a .foxdot file.
;;
;; ;;
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
;; Ctrl+c Ctrl+c (foxdot-run-line)). Send a line to the interpreter.
;; Ctrl+c Ctrl+g (foxdot-run-line-and-go).  Send a line to the interpreter and
;; advance the cursor to the next non blank line.
;; Ctrl+c b (foxdot-run-block).  Send the block where is the cursor to the interpreter.
;; Ctrl+c Ctrl+b (foxdot-run-block-and-go).  Send the block where is the cursor to the interpreter and
;; advance the cursor to the next non blank line.
;; and go to the next non blank line.
;; Ctrl+c Ctrl+r (foxdot-run-region).  Send the selected region to the interpreter.
;; Ctrl+c n (foxdot-run-block-by-lines).  Send a block line by line.
;; Ctrl+c o (foxdot-run-block-by-lines-and-go).  Send a block line by line and go to next non empty line.
;; Ctrl+c Ctrl+a (foxdot-clear-foxdot).  Clear the foxdot interpreter screen.
;; Ctrl+c Ctrl+u (foxdot-hush).  Mute foxdot sending "Clock.clear()" command to the interpreter.
;;
;; You can start sclang and foxdot interpreters with:
;;
;; Ctrl+c Ctrl+s (foxdot-sclang-foxdot-start)
;;
;; To quit sclang and foxdot: Alt+x kill-foxdot ENTER, or:
;;
;; Ctrl+c Ctrl+k (foxdot-sclang-foxdot-quit)
;;
;; Other intesting keys:
;;
;; Ctrl+c s (foxdot-sclang-start).  Run sclang process only.
;; Ctrl+c k (foxdot-sclang-kill).  Kill sclang process only.
;; Ctrl+c f (foxdot-start-foxdot).  Run python foxdot only.
;; Ctrl+c q (foxdot-kill-foxdot).  Kill python foxdot only.
;;
;; Layouts:
;;
;; Foxdot-mode can show three layouts:
;;
;;  Ctrl+c w (foxdot-set-sc3-layout).  Three vertical windows: workspace, foxdot and sclang.
;;  Ctrl+c Ctrl+w (foxdot-set-foxdot-layout).  Two windows: workspace and foxdot.
;;  Ctrl+c 3 (foxdot-sc3-foxdot-layout).  Two windows: workspace and sclang.
;;
;; Problems:
;;
;; This code is in alpha state, is not very tested (June, 2020).
;;
;; Acknowledgments:
;;
;; Thanks to Jean Argenty for its ideas and help testing this codes.

;;; Code:

(require 'python)
(require 'foxdot-sc3-mode)
(require 'foxdot-layouts)

(let ((py-interpreter (executable-find "python3")))
  (if py-interpreter
      (setq python-shell-interpreter py-interpreter
            python-shell-interpreter-args ""
            elpy-rpc-python-command py-interpreter)
    (message "Seems that python3 is not installed.  Foxdot works with python 3.")))

(defvar foxdot-buffer-name "*FoxDot*")

(defconst fox-dot-cli-init-string
  "from FoxDot import *
")

(defvar foxdot-mode-map nil)
(defvar foxdot-mode-hook nil "List of functions to run after `foxdot-mode' is enabled.")

;; To avoid the "Can’t guess python-indent-offset" warning.
(if (boundp 'python-indent-guess-indent-offset-verbose)
    (setq python-indent-guess-indent-offset-verbose nil)
  (defvar python-indent-guess-indent-offset-verbose nil))

(setq python-shell-completion-native-enable nil)
(add-to-list 'python-shell-completion-native-disabled-interpreters "python")

;;

(defun foxdot-execute-string (string)
  "Execute STRING in *FoxDot* buffer."
  (let ((current-buffer (current-buffer)))
    (let* ((b (get-buffer foxdot-buffer-name)))
      (cond (b
             (switch-to-buffer-other-window b)
             (insert string)
             (comint-send-input))
            (t (message "There is not *FoxDot* buffer.")))))
  )

(defun foxdot-next-non-blank-line ()
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
  (foxdot-run-line)
  (foxdot-next-non-blank-line)
  )

(defun foxdot-hush ()
  "Hush foxdot."
  (interactive)
  (python-shell-send-string "Clock.clear();\n" (get-process "Python"))
  )

;;;

(defun foxdot-send-region ()
  "Send the region delimited by START and END to inferior Python process."
  (interactive)
  (if (use-region-p)
      (python-shell-send-region (region-beginning)
                                (region-end) nil)
    (message "No region selected"))
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
  "Send the current block to the interpreter and jump to the next non blank line."
  (interactive)
  (foxdot-run-block)
  (mark-paragraph -1)
  (deactivate-mark t)
  (foxdot-next-non-blank-line)
  )

;;;;

(defun foxdot-is-char-after (char)
  "Test if CHAR character is before the point."
  (string= (char-to-string (char-after)) char)
  )


(defun foxdot-end-of-line-point ()
  "Return the point of current line."
  (save-excursion (end-of-line) (point))
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
  (foxdot-next-non-blank-line)
  )

;;;;

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
  "Execute the current block in the interpreter with echo and go to next non empty line."
  (interactive)
  (foxdot-execute-block)
  (forward-paragraph)
  (foxdot-next-non-blank-line)
  )

;;;;

(defun foxdot-test-audio ()
  "Test foxdot audio."
  (interactive)
  (python-shell-send-string "p1 >> pluck([12], dur=1, echo=0.8)"
			    (get-process "Python"))
  (sit-for 2)
  (foxdot-hush)
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
      (with-current-buffer buf (foxdot-mode))))
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
    (with-current-buffer (get-buffer foxdot-buffer-name)
      (comint-send-input)
      (comint-clear-buffer)))
  )

(defun foxdot-python-buffer ()
  "If it is running, return the *Python* buffer."
  (get-buffer "*Python*")
  )

(defun foxdot-py-send-command ()
  "Send input to python process."
  (with-current-buffer (or (get-buffer "*Python*") (get-buffer "*FoxDot*"))
    (comint-send-input))
  )

(defun foxdot-set-prompt ()
  "Set foxdot prompt."
  (python-shell-send-string "import sys\n" (get-process "Python"))
  (python-shell-send-string "sys.ps1 = \"FoxDot>>> \"\n" (get-process "Python"))
  )

(defun foxdot-error-kill-python ()
  "Kill python because error."
  (kill-process  (get-process "Python"))
  (kill-buffer (or (get-buffer "*Python*") (get-buffer "*FoxDot*")))
  (delete-other-windows)
  )

(defun foxdot-python-process ()
  "Get Python process."
  (if (get-buffer "*Python*")
      (get-buffer-process (get-buffer "*Python*")))
  )

(defun foxdot-tracing-function (orig-fun &rest args)
  "Foxdot tracing ARGS passed to ORIG-FUN function."
  (if (or (string-match "Error sending message to SuperCollider server instance" (nth 1 args))
          (string-match "Error: No connection made to SuperCollider server instancee" (nth 1 args)))
      (let ((p (foxdot-python-process)))
	(advice-remove (process-filter p) #'foxdot-tracing-function)
        (error "It seems that SuperCollider is not running: %S"
               (replace-regexp-in-string "\n$" "" (nth 1 args)))
	(foxdot-error-kill-python)))
  (if (string-match "Warning: Could not fetch info from SCLang server. Using defaults" (nth 1 args))
      (message "There is a problem running SuperCollider."))
  (apply orig-fun args)
  )

(defun foxdot-init-foxdot-buffer ()
  "Initialize *FoxDot* buffer."
  (when (foxdot-python-buffer)
    (with-current-buffer (foxdot-python-buffer)
      (unless (foxdot-get-sc3-buffer)
	(pop-to-buffer (foxdot-python-buffer)))
      (let ((p (foxdot-python-process)))
	(when p (advice-add (process-filter p) :around #'foxdot-tracing-function)
	      (rename-buffer foxdot-buffer-name)
	      (foxdot-set-prompt)
	      (python-shell-send-string fox-dot-cli-init-string p)
	      (sit-for 1.5)
	      (comint-clear-buffer)
	      (advice-remove (process-filter p) #'foxdot-tracing-function)))))
  )

(defun foxdot-start-python ()
  "Start python buffer."
  (let ((python-shell-interpreter-args ""))
    (when (and (not (foxdot-python-buffer)) (not (get-buffer "*FoxDot*")))
      (run-python)
      (when (not (foxdot-get-sc3-buffer))
	(delete-other-windows)
	(split-window-below)
	(foxdot-set-window-buffer-in-frame 0 1 (get-buffer "*Python*")))
      (if (foxdot-get-sc3-buffer)
	  (foxdot-sc3-foxdot-layout))))
  )

(defun foxdot-do-restart ()
  "What to do when foxdot buffer exists."
  (when (get-buffer "*FoxDot*")
    (kill-buffer (get-buffer "*FoxDot*"))
    (sit-for 0.5)
    (foxdot-start-foxdot))
  )
(defalias 'foxdot-restart-foxdot 'foxdot-do-restart)
(defalias 'restart-foxdot 'foxdot-do-restart)

;;;###autoload
(defun foxdot-start-foxdot ()
  "Start FoxDot Interpreter."
  (interactive)
  (with-current-buffer (current-buffer)
    (save-selected-window
      (unless (get-buffer "*FoxDot*")
	(foxdot-start-python)
	(if (foxdot-python-buffer) (foxdot-init-foxdot-buffer))
	(if (foxdot-get-sc3-buffer)
	    (foxdot-sc3-foxdot-layout)
	  (foxdot-set-foxdot-layout))
	(add-to-list 'auto-mode-alist '("\\.py\\([0-9]\\|[iw]\\)?$" . foxdot-mode))
	(foxdot-set-foxdot-in-all-buffers))))
  )

;;;###autoload
(defalias 'start-foxdot 'foxdot-start-foxdot)
(if (featurep 'flycheck) (add-hook 'foxdot-mode-hook #'(lambda () (flycheck-mode 0))))

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

(defun foxdot-kill-foxdot ()
  "Kill csound repl."
  (interactive)
  (let ((b (or (get-buffer foxdot-buffer-name) (get-buffer "*Python*")))
        (c (current-buffer))
        (s (get-buffer "*SC3:SCLang*")))
    (if s (with-current-buffer s (kill-buffer-and-window)))
    (if b (with-current-buffer b (kill-buffer-and-window))
      (error "There is not *FoxDot* buffer"))
    (foxdot-set-python-in-all-buffers)
    (switch-to-buffer c)
    (delete-other-windows))
  (if (member '("\\.py\\([0-9]\\|[iw]\\)?$" . foxdot-mode) auto-mode-alist)
      (setq auto-mode-alist (delete '("\\.py\\([0-9]\\|[iw]\\)?$" . foxdot-mode) auto-mode-alist)))
  )
(defalias 'foxdot-quit-foxdot 'foxdot-kill-foxdot)
(defalias 'kill-foxdot 'foxdot-kill-foxdot)

;;;;

(declare-function sc3-start-process "foxdot-sc3-mode")
(declare-function sc3-kill-process "foxdot-sc3-mode")
(declare-function foxdot-sc3-foxdot-layout "foxdot-sc3-mode")

(defun foxdot-sclang-advice (orig-fun &rest args)
  "Foxdot tracing ARGS passed to ORIG-FUN function."
  (when (string-match "Listening for messages from FoxDot" (nth 1 args))
    (advice-remove
     (process-filter (get-process "sc3:sclang")) #'foxdot-sclang-advice)
    (foxdot-start-foxdot))
  (apply orig-fun args)
  )

(defun foxdot-sclang-foxdot-start ()
  "Start SCLang and FoxDot."
  (interactive)
  (with-current-buffer (current-buffer)
    (save-selected-window
      (when (sc3-start-process)
	(if (get-process "sc3:sclang")
	    (advice-add (process-filter (get-process "sc3:sclang")) :around #'foxdot-sclang-advice)))))
  )
(defalias 'foxdot 'foxdot-sclang-foxdot-start)

(defun foxdot-sc3-kill-process ()
  "Kill SCLang process."
  (interactive)
  (sc3-kill-process)
  )

(defun foxdot-sclang-foxdot-quit ()
  "Quit SCLang and FoxDot."
  (interactive)
  (foxdot-sc3-kill-process)
  (foxdot-kill-foxdot)
  )

(defun foxdot-mode-keybindings (map)
  "FoxDot keybindings in MAP."
  (define-key map (kbd "C-c C-s") 'foxdot-sclang-foxdot-start)
  (define-key map (kbd "C-c C-k") 'foxdot-sclang-foxdot-quit)
  (define-key map (kbd "C-c s") 'sc3-start-process)
  (define-key map (kbd "C-c k") 'fsc3-kill-process)
  (define-key map (kbd "C-c f") 'foxdot-start-foxdot)
  (define-key map (kbd "C-c q") 'foxdot-kill-foxdot)
  (define-key map [?\C-c ?\C-c] 'foxdot-run-line)
  (define-key map [?\C-c ?\C-g] 'foxdot-run-line-and-go)
  (define-key map [?\C-c ?\g]   'foxdot-next-non-blank-line)
  (define-key map (kbd "C-c b") 'foxdot-run-block)
  (define-key map [?\C-c ?\C-b] 'foxdot-run-block-and-go)
  (define-key map (kbd "C-c e") 'foxdot-execute-block)
  (define-key map [?\C-c ?\C-e] 'foxdot-execute-block-and-go)
  (define-key map [?\C-c ?\C-r] 'foxdot-run-region)
  (define-key map [?\C-c ?\C-n] 'foxdot-run-block-by-lines)
  (define-key map [?\C-c ?\o]   'foxdot-run-block-by-lines-and-go)
  (define-key map [?\C-c ?\C-u] 'foxdot-hush)
  (define-key map [?\C-c ?\C-a] 'foxdot-clear-foxdot)
  ;;  (define-key map [?\C-c ?\l] 'foxdot-load-buffer)
  )

(defun turn-on-foxdot-keybindings ()
  "Foxdot keybindings in the local map."
  (interactive)
  (local-set-key (kbd "C-c C-s") 'foxdot-sclang-foxdot-start)
  (local-set-key (kbd "C-c C-k") 'foxdot-sclang-foxdot-quit)
  (local-set-key (kbd "C-c s") 'foxdot-sc3-start-process)
  (local-set-key (kbd "C-c k") 'foxdot-sc3-kill-process)
  (local-set-key (kbd "C-c f") 'foxdot-start-foxdot)
  (local-set-key (kbd "C-c q") 'foxdot-kill-foxdot)
  (local-set-key [?\C-c ?\C-c] 'foxdot-run-line)
  (local-set-key [?\C-c ?\C-g] 'foxdot-run-line-and-go)
  (local-set-key [?\C-c ?\g]   'foxdot-next-non-blank-line)
  (local-set-key (kbd "C-c b") 'foxdot-run-block)
  (local-set-key [?\C-c ?\C-b] 'foxdot-run-block-and-go)
  (local-set-key (kbd "C-c e") 'foxdot-execute-block)
  (local-set-key [?\C-c ?\C-e] 'foxdot-execute-block-and-go)
  (local-set-key [?\C-c ?\C-r] 'foxdot-run-region)
  (local-set-key [?\C-c ?\C-n] 'foxdot-run-block-by-lines)
  (local-set-key [?\C-c ?\o]   'foxdot-run-block-by-lines-and-go)
  (local-set-key [?\C-c ?\C-u] 'foxdot-hush)
  (local-set-key [?\C-c ?\C-a] 'foxdot-clear-foxdot)
  ;;  (local-set-key [?\C-c ?\l] 'foxdot-load-buffer)
  )
(add-hook 'foxdot-mode-hook 'turn-on-foxdot-keybindings)

(defun foxdot-set-keybindings ()
  "To turn on foxdot key bindings in other modes buffers."
  (local-set-key (kbd "C-c C-s") 'foxdot-sclang-foxdot-start)
  (local-set-key (kbd "C-c C-k") 'foxdot-sclang-foxdot-quit)
  (local-set-key (kbd "C-c s") 'foxdot-sc3-start-process)
  (local-set-key (kbd "C-c k") 'foxdot-sc3-kill-process)
  (local-set-key (kbd "C-c f") 'foxdot-start-foxdot)
  (local-set-key (kbd "C-c q") 'foxdot-kill-foxdot)
  )
(add-hook 'sc3-mode-hook 'foxdot-set-keybindings)

(declare-function sc3-turn-on-keybindings "sc3-mode")
(add-hook 'foxdot-mode-hook 'sc3-turn-on-keybindings)

;;

(defun foxdot-mode-menu (map)
  "FoxDot menu from MAP."
  (define-key map [menu-bar foxdot]
    (cons "Python-FoxDot" (make-sparse-keymap "FoxDot")))
  (define-key map [menu-bar foxdot quit-foxdot]
    '("Quit FoxDot" . foxdot-kill-foxdot))
  (define-key map [menu-bar foxdot sclang-start-foxdot]
    '("Start FoxDot" . foxdot-start-foxdot))
  (define-key map [menu-bar foxdot quit-foxdot]
    '("Quit SCLang" . sc3-kill-process))
  (define-key map [menu-bar foxdot sclang-start-foxdot]
    '("Start SCLang" . sc3-start-process))
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
  (define-key map [menu-bar foxdot run-block-and-go]
    '("Run block and go" . foxdot-run-block-and-go))
  (define-key map [menu-bar foxdot run-block]
    '("Run block" . foxdot-run-block))
  (define-key map [menu-bar foxdot run-line-and-go]
    '("Run line and go" . foxdot-run-line-and-go))
  (define-key map [menu-bar foxdot run-line]
    '("Run line" . foxdot-run-line))
  (define-key map [menu-bar foxdot foxdot-separator]
    '(menu-item "--"))
  (define-key map [menu-bar foxdot sclang-foxdot-quit]
    '("Quit SCLang-FoxDot" . foxdot-sclang-foxdot-quit))
  (define-key map [menu-bar foxdot sclang-foxdot-start]
    '("Start SCLang-FoxDot" . foxdot-sclang-foxdot-start))
  )

(unless foxdot-mode-map
  (let ((map (make-sparse-keymap "Python-FoxDot")))
    (foxdot-mode-keybindings map)
    (foxdot-mode-menu map)
    (setq foxdot-mode-map map)))

(defconst fd-keywords
  '("dur" "amp" "pan" "echo" "pan" "indent" "stop" "oct" "cutoff" "room" "lpf" "rate" "sus"))

(defconst fd-functions
  '("lazer" "crunch" "swell" "quin" "sinepad" "pads" "loop" "blip" "squish" "pasha" "space" "growl" "dirt" "snick" "spark" "orient" "karp" "twang" "ambi" "sawbass" "dub" "soft" "creep" "varsaw" "saw" "arpy" "viola" "gong" "prophet" "glass" "pulse" "rave" "dab" "razz" "soprano" "noise" "keys" "audioin" "scratch" "ripple" "stretch" "bass" "zap" "bug" "donk" "star" "jbass" "scatter" "fuzz" "bell" "dbass" "marimba" "feel" "klank" "play1" "charm" "sitar" "play2" "nylon" "pluck" "Clock" "play" "print" "SynthDefs" "PWhite" "PRand" "var" "linvar" "return"))

(setq fd-keywords (cl-concatenate 'list fd-keywords sc3-keywords))
(setq fd-functions (cl-concatenate 'list fd-functions sc3-functions))

(defvar fd-font-lock-keywords
  (list
   `("\"[^\\].*\"" . font-lock-string-face)
   `("\'..\\(.\\|
\\)*\'..\\|#.*$" . font-lock-comment-face)
   ;; `(,(rx (or (and "\'\'\'" (*? anything) "\'\'\'") (and "#" (*? anything) eol))) . font-lock-comments)
   `(,(concat "\\_<" (regexp-opt fd-keywords) "\\_>") . font-lock-keyword-face)
   `(,(concat "\\_<" (regexp-opt fd-functions) "\\_>") . font-lock-function-name-face)
   `("^[a-z]*[^(][a-zA-Z]*[^(>]" . font-lock-function-name-face)
   `(">> " . font-lock-reference-face))
  ;; `("^[a-z]\\(.* >> \\|[a-z] \\|\\([1-9][0-9]\\|[1-9]\\)\\)" . font-lock-reference-face)))
  "Additional expressions to highlight in `foxdot-mode'.")

;;;###autoload
(define-derived-mode
  foxdot-mode
  python-mode
  "Python Foxdot"
  "Major mode for interacting with an inferior FoxDot process."
  (set (make-local-variable 'paragraph-start) "\f\\|[ \t]*$")
  (set (make-local-variable 'paragraph-separate) "[ \t\f]*$")
  (set (make-local-variable 'font-lock-defaults) '(fd-font-lock-keywords t))
  (turn-on-font-lock)
  )

(add-hook 'foxdot-mode-hook #'(lambda () (show-paren-mode 1)))
(setq show-paren-style 'parenthesis)

(provide 'foxdot-mode)
;;; foxdot-mode.el ends here

