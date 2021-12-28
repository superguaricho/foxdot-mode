;;; foxdot-mode.el -- FoxDot Mode for Emacs.
;; Copyright (C) 2020 numa.tortolero@gmail.com
;; Author: numa.tortolero@gmail.com
;; Homepage: https://github.com/superguaricho/foxdot-mode
;; Version: 1.03 (alpha)
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
;;
;;         $ pip install FoxDot
;;
;;;;;;
;;
;; (2) Install Emacs FoxDot mode.
;;
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
;;;;;;;
;;
;; (3) Install the SuperCollider FoxDot quark.
;;
;; Assuming that you have installed SuperCollider, from Emacs do Alt-x install-foxdot-quark
;;
;; This search for sclang and, if it is in your PATH, run it in a Emacs buffer. Then, if you
;; are in line, will install FoxDot quark, recompile the SuperCollider class library and
;; start the FoxDot quark for you.
;;
;; You must hear a simple sound.  If you don't hear it, do `Alt-x test-sc3'. If yet
;; you don't hear a sound, something is wrong: there is a problem with SuperCollider
;; configuration or in your audio system.
;;
;;;;;;;
;;
;; (4) Start foxdot.
;;
;; Open a file with .foxdot extension.  Type `Alt+x foxdot-start ENTER'.
;; This run sclang and FoxDot process buffers.  Wait and you will see three horizontal
;; windows: the .foxdot file (your workspace), the  *FoxDot* and the *SC3:SCLang* buffers.
;;
;; Then, you can start sc3 and foxdot process with `Alt+x foxdot ENTER'.
;;
;;;;;;;
;;
;; (5) Play with some codes.
;;
;; For example, type in your workspace:
;;
;; p1 >> pluck([12], dur=0.25, echo=0.8)
;;
;; With the cursor over the line type Ctrl+c Ctrl+c.
;;
;; Do Ctrl+c Ctrl+u to stop the sounds.
;;
;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;;
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
;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;;
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
;;                       advance the cursor to the next non blank line.
;; Ctrl+c b (foxdot-run-block).  Send the block where is the cursor to the interpreter.
;; Ctrl+c Ctrl+b (foxdot-run-block-and-go).  Send the block where is the cursor to the
;;                       interpreter and advance the cursor to the next non blank line.
;; Ctrl+c Ctrl+r (foxdot-run-region).  Send the selected region to the interpreter.
;; Ctrl+c n (foxdot-run-block-by-lines).  Send a block line by line.
;; Ctrl+c o (foxdot-run-block-by-lines-and-go).  Send a block line by line and go to next non empty line.
;; Ctrl+c Ctrl+a (foxdot-clear-foxdot).  Clear the foxdot interpreter screen.
;; Ctrl+c Ctrl+u (foxdot-hush).  Mute foxdot sending "Clock.clear()" command to the interpreter.
;;
;; You can start sclang and foxdot interpreters with:
;;
;; Ctrl+c Ctrl+s (foxdot-sc3-foxdot-start)
;;
;; To quit sclang and foxdot: Alt+x kill-foxdot ENTER, or:
;;
;; Ctrl+c Ctrl+k (foxdot-sc3-foxdot-quit)
;;
;; Other intesting keys:
;;
;; Ctrl+c s (sc3-start-process).  Run sclang process only.
;; Ctrl+c k (sc3-kill-process).  Kill sclang process only.
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
;; This code is in alpha state, is not very tested (December, 2021).
;;
;; Acknowledgments:
;;
;; Thanks to Jean Argenty for its ideas and help testing this codes.

;;; Code:

(require 'python)
(require 'foxdot-sc3-mode)
(require 'foxdot-layouts)

(defvar foxdot-py-interpreter (or (executable-find "python3") "python3"))
(defconst foxdot-buffer-name "*FoxDot*")
(defconst fox-dot-cli-init-string
  "from FoxDot import *
print(\"Starting Foxdot...\")
")
(defvar foxdot-mode-map nil)
(defvar foxdot-mode-hook nil "List of functions to run after `foxdot-mode' is enabled.")

;; To avoid the "Can’t guess python-indent-offset" warning.
(if (boundp 'python-indent-guess-indent-offset-verbose)
    (setq python-indent-guess-indent-offset-verbose nil)
  (defvar python-indent-guess-indent-offset-verbose nil))

(setq python-shell-completion-native-enable nil)
(add-to-list 'python-shell-completion-native-disabled-interpreters "python")

(defvar foxdot-work-buffer (current-buffer))

;;; line

(defun foxdot-next-non-blank-line ()
  "Move to the next non-blank line."
  (interactive)
  (goto-char (line-end-position))
  (forward-line)
  (while (and (eolp) (bolp) (not (eobp))) (forward-line))
  )

(declare-function python-shell-send-string "python")
(defun foxdot-run-line ()
  "Send the current line to the interpreter."
  (interactive)
  (let*((s (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
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

;;; region and blocks

(defun foxdot-run-region ()
  "Send the current block to the interpreter."
  (interactive)
  (if (and (use-region-p) (get-process "Python"))
      (let* ((s (buffer-substring-no-properties (region-beginning) (region-end))))
        (python-shell-send-string s (get-process "Python"))
	(pulse-momentary-highlight-region (mark) (point))
	(sit-for 0.25))
    (message "No region selected"))
  )

(defun foxdot-run-block ()
  "Send the current block to the interpreter."
  (interactive)
  (save-excursion (mark-paragraph) (foxdot-run-region) (deactivate-mark t))
  )

(defun foxdot-run-block-and-go ()
  "Send the current block to the interpreter and jump to the next non blank line."
  (interactive)
  (foxdot-run-block)
  (forward-paragraph)
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
  (save-excursion
    (mark-paragraph -1)
    (save-selected-window
      (if (use-region-p)
          (let* ((b (get-buffer foxdot-buffer-name)))
            (cond (b
                   (append-to-buffer b (region-beginning) (region-end))
                   (switch-to-buffer-other-window b)
                   (comint-send-input))
                  (t (message "There is not *FoxDot* buffer."))))))
    (deactivate-mark t)
    (pulse-momentary-highlight-region (mark) (point))
    (sit-for 0.25))
  )

(defun foxdot-execute-block-and-go ()
  "Execute the current block in the interpreter with echo and go to next non empty line."
  (interactive)
  (foxdot-execute-block)
  (forward-paragraph)
  (foxdot-next-non-blank-line)
  )

;;; commands

(defun foxdot-hush ()
  "Hush foxdot."
  (interactive)
  (python-shell-send-string "Clock.clear();\n" (get-process "Python"))
  (sit-for 0.25)
  (with-current-buffer "*FoxDot*" (comint-send-input))
  )

(defun foxdot-test-audio ()
  "Test foxdot audio."
  (interactive)
  (python-shell-send-string "p1 >> pluck([12], dur=1, echo=0.8)" (get-process "Python"))
  (sit-for 6)
  (foxdot-hush)
  )

(defun foxdot-clear-foxdot ()
  "Clear the *FoxDot window."
  (interactive)
  (when (get-buffer foxdot-buffer-name)
    (with-current-buffer (get-buffer foxdot-buffer-name)
      (comint-send-input)
      (sit-for 0.25)
      (comint-clear-buffer)))
  )

;;; set py and foxdot files to foxdot-mode

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

;;; foxdot process

(defun foxdot-error-kill-python ()
  "Kill python because error."
  (let ((p  (get-process "Python"))
	(b (or (get-buffer "*Python*") (get-buffer "*FoxDot*"))))
    (and p (kill-process  p))
    (and b (with-current-buffer b (kill-buffer-and-window))))
  )

(defun foxdot-python-process ()
  "Get Python process."
  (if (get-buffer "*Python*")
      (get-buffer-process (get-buffer "*Python*")))
  )

(defun foxdot-python-buffer ()
  "If it is running, return the *Python* buffer."
  (get-buffer "*Python*")
  )

(defun foxdot-set-prompt ()
  "Set foxdot prompt."
  (let ((p (get-process "Python")))
    (when p (python-shell-send-string "import sys\n" p)
	  (python-shell-send-string "sys.ps1 = \"FoxDot>>> \"\n" p)))
  )

(defun foxdot-set-layout ()
  "Set foxdot layout."
  (let* ((b foxdot-work-buffer)
	 (p (with-current-buffer b (point))))
    (if (and (get-buffer "*SC3:SCLang*") (or (get-buffer "*FoxDot*") (get-buffer "*Python*")))
	(foxdot-sc3-foxdot-layout b)
      (if (foxdot-get-sc3-buffer)
	  (foxdot-set-sc3-layout)
	(if (or (get-buffer "*FoxDot*") (get-buffer "*Python*"))
	    (foxdot-set-foxdot-layout))))
    (when (and (get-buffer b) (window-live-p (get-buffer-window b)))
      (select-window (get-buffer-window b))
      (with-current-buffer b (goto-char p))))
  )

(defun foxdot-set-prompt-and-buffer ()
  "If Python buffer exist, set FoxDot prompt and buffer."
  (let ((b (foxdot-python-buffer)))
    (when b
      (with-current-buffer b
	(comint-clear-buffer)
	(foxdot-set-prompt)
	(sit-for 0.25)
	(comint-send-input)
	(comint-clear-buffer)
	(rename-buffer "*FoxDot*"))))
  )

(defun foxdot-buffer-and-layout ()
  "Set FoxDot buffer and the layout."
  (foxdot-set-prompt-and-buffer)
  (foxdot-set-layout)
  )

(defun foxdot-tracing-function (orig-fun &rest args)
  "Foxdot tracing ARGS passed to ORIG-FUN function."
  (let ((p (nth 0 args))
	(s (nth 1 args)))
    (when (or (string-match "Error sending message to SuperCollider server instance" s)
              (string-match "Error: No connection made to SuperCollider server instancee" s))
      (advice-remove (process-filter p) #'foxdot-tracing-function)
      (message "It seems that SuperCollider is not running: %S" (replace-regexp-in-string "\n$" "" s))
      (sit-for 10)
      (foxdot-error-kill-python))
    (cond ((string-match "Warning: Could not fetch info from SCLang server. Using defaults" s)
	   (message "There is a problem running SuperCollider."))
	  ((string-match "Starting Foxdot..." s)
	   (advice-remove (process-filter p) #'foxdot-tracing-function)
	   (foxdot-buffer-and-layout))))
  (apply orig-fun args)
  )

(defun foxdot-display-buffer (buffer)
  "Display BUFFER buffer bellow selected window."
  (display-buffer
   (get-buffer buffer)
   '((display-buffer-below-selected)
     (inhibit-same-window . t)
     (dedicated . t)
     (inhibit-switch-frame . t)
     (window-height . (/ (frame-height) 4))))
  (unless (eq (frame-root-window) window)
    (window-resize window (- (/ (frame-height) 2) (window-height))))
  )

(defun foxdot-init-foxdot-buffer (handle window)
  "Initialize *FoxDot* buffer using HANDLE handle init."
  (select-window window)
  (save-selected-window
    (let ((b (foxdot-python-buffer)))
      (when b
        (foxdot-display-buffer b)
        (if (with-current-buffer b (save-excursion (goto-char (point-min)) (search-forward "Starting Foxdot..." nil t)))
            (foxdot-buffer-and-layout)
          (advice-add (process-filter (get-buffer-process b)) :around #'foxdot-tracing-function))
        (if (eq handle #'foxdot-start-python) (python-shell-send-string fox-dot-cli-init-string)))))
  )

(defun foxdot-start-python ()
  "Start python buffer."
  (let ((python-shell-interpreter-args ""))
    (unless (foxdot-python-buffer) (run-python) (foxdot-python-buffer)))
  )

(defun foxdot-start-foxdot-handle (handle window)
  "Start FoxDot interpreter using HANDLE function."
  (select-window window)
  (save-selected-window
    (unless (get-buffer "*FoxDot*")
      (setq foxdot-work-buffer (current-buffer))
      (and (funcall handle) (foxdot-init-foxdot-buffer handle window))))
  (add-to-list 'auto-mode-alist '("\\.py\\([0-9]\\|[iw]\\)?$" . foxdot-mode))
  (foxdot-set-foxdot-in-all-buffers)
  )

;;;###autoload
(defun foxdot-start-foxdot ()
  "Start FoxDot Interpreter."
  (interactive)
  (foxdot-start-foxdot-handle #'foxdot-start-python (selected-window))
  )
;;;###autoload
(defalias 'start-foxdot 'foxdot-start-foxdot)

;; Run Foxdot with temporal buffer init file.

(defun foxdot-start-py-with-temp-file ()
  "Start FioxDot loading a temp file."
  (let* ((temp-file (make-temp-file
                     (expand-file-name (make-temp-name "foxdot")
                                       (or small-temporary-file-directory
                                           temporary-file-directory))))
	 (python-shell-interpreter (or foxdot-py-interpreter "python3"))
	 (python-shell-interpreter-args (format "-i %s" temp-file)))
    (with-temp-file temp-file (insert fox-dot-cli-init-string))
    (run-python (format "%s %s" python-shell-interpreter python-shell-interpreter-args))
    (sit-for 1)
    (delete-file temp-file))
  (foxdot-python-buffer)
  )

;;;###autoload
(defun foxdot-run-foxdot ()
  "Run FoxDot interpreter loading a temp file."
  (interactive)
  (foxdot-start-foxdot-handle #'foxdot-start-py-with-temp-file (selected-window))
  )
(defalias 'foxdot-run 'foxdot-run-foxdot)

;;

(defun foxdot-do-restart ()
  "What to do when foxdot buffer exists."
  (when (get-buffer "*FoxDot*")
    (kill-buffer (get-buffer "*FoxDot*"))
    (sit-for 0.5)
    (foxdot-start-foxdot))
  )
(defalias 'foxdot-restart-foxdot 'foxdot-do-restart)
(defalias 'restart-foxdot 'foxdot-do-restart)

;; Kill foxdot

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
  "Kill foxdot."
  (interactive)
  (let* ((cb (current-buffer))
	 (p (with-current-buffer cb (point)))
	 (b (or (get-buffer foxdot-buffer-name) (foxdot-python-buffer))))
    (if b (with-current-buffer b (kill-buffer-and-window))
      (error "There is not *FoxDot* buffer"))
    (foxdot-set-python-in-all-buffers)
    (foxdot-set-window-buffer-in-frame 0 0 cb)
    (with-current-buffer cb (select-window (get-buffer-window)) (goto-char p)))
  (if (member '("\\.py\\([0-9]\\|[iw]\\)?$" . foxdot-mode) auto-mode-alist)
      (setq auto-mode-alist (delete '("\\.py\\([0-9]\\|[iw]\\)?$" . foxdot-mode) auto-mode-alist)))
  )
(defalias 'foxdot-quit-foxdot 'foxdot-kill-foxdot)
(defalias 'kill-foxdot 'foxdot-kill-foxdot)

;;;; Start sc3 and FoxDot

(declare-function sc3-start-process "foxdot-sc3-mode")
(declare-function sc3-kill-process "foxdot-sc3-mode")
(declare-function foxdot-sc3-foxdot-layout "foxdot-sc3-mode")

(defun foxdot-sc3-advice (orig-fun &rest args)
  "Foxdot tracing ARGS passed to ORIG-FUN function."
  (let ((proc (nth 0 args)))
    (if (buffer-live-p (process-buffer proc))
	(when (string-match "Listening for messages from FoxDot" (nth 1 args))
	(advice-remove (process-filter proc) #'foxdot-sc3-advice)
	(foxdot-start-foxdot))))
  (apply orig-fun args)
  )

;;;###autoload
(defun foxdot-sc3-foxdot-start ()
  "Start SCLang and FoxDot."
  (interactive)
  (save-selected-window
    (if (or (executable-find sc3-cli-file-path) (executable-find "sclang"))
	(let ((proc (sc3-start-process))) (if proc (advice-add (process-filter proc) :around #'foxdot-sc3-advice)))
      (message "sclang is not in PATH or SuperCollider is not installed.")))
  )

;;;###autoload
(defalias 'foxdot 'foxdot-sc3-foxdot-start)
(defalias 'sc3-foxdot-start 'foxdot-sc3-foxdot-start)
(defalias 'foxdot-sc3-kill-process 'sc3-kill-process)

;;; Run sc3 and foxdot with temporal buffer init file.

(defun foxdot-run-sc3-advice (orig-fun &rest args)
  "Foxdot tracing ARGS passed to ORIG-FUN function."
  (let ((proc (nth 0 args)))
    (if (buffer-live-p (process-buffer proc))
	(when (string-match "Listening for messages from FoxDot" (nth 1 args))
	  (advice-remove (process-filter proc) #'foxdot-run-sc3-advice)
	  (foxdot-run-foxdot))))
  (apply orig-fun args)
  )

;;;###autoload
(defun foxdot-sc3-foxdot-run ()
  "Run SCLang and FoxDot."
  (interactive)
  (save-selected-window
    (if (or (executable-find sc3-cli-file-path) (executable-find "sclang"))
	(let ((proc (sc3-start-process))) (if proc (advice-add (process-filter proc) :around #'foxdot-run-sc3-advice)))
      (message "sclang is not in PATH or SuperCollider is not installed.")))
  )

;; Quit SCLang and FoxDot.

(defun foxdot-sc3-foxdot-quit ()
  "Quit SCLang and FoxDot."
  (interactive)
  (foxdot-sc3-kill-process)
  (foxdot-kill-foxdot)
  )

;;; key map and bindings

(defun foxdot-mode-keybindings (map)
  "FoxDot keybindings in MAP."
  (define-key map (kbd "C-c C-s") 'foxdot-sc3-foxdot-start)
  (define-key map (kbd "C-c C-i") 'foxdot-sc3-foxdot-run)
  (define-key map (kbd "C-c C-k") 'foxdot-sc3-foxdot-quit)
  
  (define-key map (kbd "C-c s") 'sc3-start-process)
  (define-key map (kbd "C-c k") 'sc3-kill-process)
  
  (define-key map (kbd "C-c f") 'foxdot-start-foxdot)
  (define-key map (kbd "C-c i") 'foxdot-run-foxdot)
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
  (local-set-key (kbd "C-c C-s") 'foxdot-sc3-foxdot-start)
  (local-set-key (kbd "C-c C-i") 'foxdot-sc3-foxdot-run)
  (local-set-key (kbd "C-c C-k") 'foxdot-sc3-foxdot-quit)
  
  (local-set-key (kbd "C-c s") 'sc3-start-process)
  (local-set-key (kbd "C-c k") 'sc3-kill-process)
  
  (local-set-key (kbd "C-c f") 'foxdot-start-foxdot)
  (local-set-key (kbd "C-c i") 'foxdot-run-foxdot)
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
(add-hook 'foxdot-mode-hook 'sc3-turn-on-keybindings)

(defun foxdot-set-keybindings ()
  "To turn on foxdot key bindings in other modes buffers."
  (local-set-key (kbd "C-c C-s") 'foxdot-sc3-foxdot-start)
  (local-set-key (kbd "C-c C-i") 'foxdot-sc3-foxdot-run)
  (local-set-key (kbd "C-c C-k") 'foxdot-sc3-foxdot-quit)
  
  (local-set-key (kbd "C-c s") 'sc3-start-process)
  (local-set-key (kbd "C-c i") 'foxdot-run-foxdot)
  (local-set-key (kbd "C-c k") 'sc3-kill-process)
  
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
  (define-key map [menu-bar foxdot sc3-start-foxdot]
    '("Start FoxDot" . foxdot-start-foxdot))
  
  (define-key map [menu-bar foxdot quit-foxdot]
    '("Quit SCLang" . sc3-kill-process))
  (define-key map [menu-bar foxdot sc3-start-foxdot]
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
  
  (define-key map [menu-bar foxdot sc3-foxdot-quit]
    '("Quit SC3" . foxdot-sc3-foxdot-quit))
  (define-key map [menu-bar foxdot sc3-foxdot-start]
    '("Start SC3" . foxdot-sc3-foxdot-start))
  )

(unless foxdot-mode-map
  (let ((map (make-sparse-keymap "Python-FoxDot")))
    (foxdot-mode-keybindings map)
    (foxdot-mode-menu map)
    (setq foxdot-mode-map map)))

(defconst fd-keywords
  '("dur" "amp" "pan" "echo" "pan" "indent" "stop" "oct" "cutoff" "room" "lpf" "rate" "sus"))

(defconst fd-functions
  '("follow" "lazer" "crunch" "swell" "quin" "sinepad" "pads" "loop" "blip" "squish" "pasha" "space" "growl" "dirt" "snick" "spark" "orient" "karp" "twang" "ambi" "sawbass" "dub" "soft" "creep" "varsaw" "saw" "arpy" "viola" "gong" "prophet" "glass" "pulse" "rave" "dab" "razz" "soprano" "noise" "keys" "audioin" "scratch" "ripple" "stretch" "bass" "zap" "bug" "donk" "star" "jbass" "scatter" "fuzz" "bell" "dbass" "marimba" "feel" "klank" "play1" "charm" "sitar" "play2" "nylon" "pluck" "Clock" "play" "print" "SynthDefs" "PWhite" "PRand" "var" "linvar" "return"))

(defvar sc3-fd-keywords nil)
(defvar sc3-fd-functions nil)

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

(if (featurep 'flycheck) (add-hook 'foxdot-mode-hook #'(lambda () (flycheck-mode 0))))

(add-hook 'foxdot-mode-hook #'(lambda () (show-paren-mode 1)))
(setq show-paren-style 'parenthesis)

(provide 'foxdot-mode)
;;; foxdot-mode.el ends here
