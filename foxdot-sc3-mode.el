;;; foxdot-sc3-mode.el -- SCLang Mode for FoxDot.

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
;; SCLang Mode for FoxDot. It would come with foxdot-mode.

;;; Code:

(require 'comint)

(defvar sc3-cli-file-path (executable-find "sclang")
  "Path to the sclang program.")

(defvar sc3-cli-arguments '()
  "Commandline arguments to pass to sclang.")

(defvar sc3-mode-map nil
  "Mode map for `sc3-mode'.")

(defvar sc3-proc-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    (define-key map "\t" 'completion-at-point)
    map)
  "Basic mode map for sclang process.")

(defvar sc3-process "sc3:sclang" "SC3 process name.")

(defvar sc3-buffer "*SC3:SCLang*" "SC3 buffer name.")

;;

(defun sc3-send-string (string)
  "Send STRING to sclang process."
  (let ((p (get-process sc3-process)))
    (when p (process-send-string p (concat string "\n"))))
  )

(defun sc3-shell-newline ()
  "Send new line to *FoxDot* buffer."
  (interactive)
  (when (get-buffer sc3-buffer)
    (with-current-buffer (get-buffer sc3-buffer)
      (sc3-send-string "")))
  )

(defun sc3-next-non-blank-line ()
  "Move to the nex non-blank line."
  (interactive)
  (goto-char (line-end-position))
  (forward-line)
  (while (and (eolp) (bolp) (not (eobp))) (forward-line))
  )

(declare-function python-shell-send-string "python")
(defun sc3-run-line ()
  "Send the current line to the interpreter."
  (interactive)
  (let* ((start (line-beginning-position))
         (end (line-end-position))
         (b (current-buffer))
         (s (buffer-substring start end)))
    (pulse-momentary-highlight-one-line (point))
    (sit-for 0.25)
    (sc3-send-string s ))
  )

(defun sc3-run-line-and-go ()
  "Send the current line to the interpreter."
  (interactive)
  (sc3-run-line)
  (sc3-next-non-blank-line)
  )

;;

(defun sc3-run-region ()
  "Send the current block to the interpreter."
  (interactive)
  (if (use-region-p)
      (let* ((s (buffer-substring-no-properties (region-beginning)
                                                (region-end))))
        (sc3-send-string (replace-regexp-in-string "\n" " " s))))
  (pulse-momentary-highlight-region (mark) (point))
  (deactivate-mark t)
  )

(defun sc3-run-block ()
  "Send the current block to the interpreter."
  (interactive)
  (save-excursion (mark-paragraph -1) (sc3-run-region))
  )

(defun sc3-run-block-and-go ()
  "Send the current block to the interpreter."
  (interactive)
  (sc3-run-block)
  (mark-paragraph -1)
  (deactivate-mark t)
  (sc3-next-non-blank-line)
  )

(defun sc3-hush ()
  "Hush sclang."
  (interactive)
  (sc3-send-string "thisProcess.stop")
  )

;;

(defun sc3-start-fdquark ()
  "Start foxdot quark in SuperCollider."
  (interactive)
  (sc3-send-string "FoxDot.start;")
  )
(defalias 'foxdot-quark-start 'sc3-start-fdquark)
(defalias 'start-fd-quark 'sc3-start-fdquark)

(defun sc3-test-audio ()
  "Test SuperCollider audio."
  (interactive)
  (sc3-send-string
   "{ SinOsc.ar(440, 0, Line.kr(0.3, 0, 1, doneAction:2)) }.play;")
  )
(defalias 'test-sc3 'sc3-test-audio)

;;
(declare-function foxdot-start-foxdot "foxdot-mode")
(defun sc3-when-foxdot-buffer ()
  "What to do if *FoxDot* buffer exists."
  (if (get-buffer "*FoxDot*") (foxdot-start-foxdot))
  )

(defun sc3-when-listening-foxdot ()
  "What to do when listening foxdot."
  (sc3-test-audio)
  (sit-for 1)
  (sc3-when-foxdot-buffer)
  )

(defun sc3-insertion-filter (proc string)
  "SC3 PROC process STRING filter."
  (when (buffer-live-p (process-buffer proc))
    (display-buffer (process-buffer proc))
    (when (string-match "Shared memory server interface initialized" string)
      (sc3-start-fdquark))
    (when (string-match "Listening for messages from FoxDot" string)
      (sc3-when-listening-foxdot))
    (with-current-buffer (process-buffer proc)
	  (let ((moving (= (point) (process-mark proc))))
	    (goto-char (process-mark proc))
	    (insert string)
	    (set-marker (process-mark proc) (point))
	    (set-window-point (get-buffer-window sc3-buffer) (point))
	    (goto-char (process-mark proc))))
    (when (or
	   (string-match "could not initialize audio" string)
	   (string-match "jack server is not running or cannot be started" string))
      (message "It seems that SuperCollider could not run: %S" (replace-regexp-in-string "\n$" "" string))
      (sit-for 5)
      (when (get-buffer sc3-buffer)
	(delete-window (get-buffer-window sc3-buffer))
	(kill-process proc)
	(kill-buffer sc3-buffer))))
  )

(defun sc3-create-process ()
  "Create a SC3:SCLang process."
  (make-process :name sc3-process
                :buffer sc3-buffer
                :command (list sc3-cli-file-path)
                :coding 'utf-8
                :filter 'sc3-insertion-filter
                :connection-type 'pipe)
  )

(declare-function foxdot-set-sc3-layout "foxdot-mode")
(declare-function foxdot-set-foxdot-layout "foxdot-mode")
(declare-function foxdot-sc3-foxdot-layout "foxdot-mode")
(declare-function foxdot-do-restart "foxdot-mode")

(defun sc3-start-process ()
  "Create a SC3:SCLang process."
  (interactive)
  (with-current-buffer (current-buffer)
    (save-selected-window
      (if (executable-find "sclang")
	  (if (not (get-process sc3-process))
	      (with-current-buffer (process-buffer (sc3-create-process)) (sc3-proc-mode))
	    (message "A sclang process is running.  Kill it before restart it."))
	(message "sclang is not in PATH or SuperCollider is not installed."))
      (if (get-buffer sc3-buffer) (foxdot-set-sc3-layout))
      (when (and (get-buffer sc3-buffer) (get-buffer "*FoxDot*"))
	(with-current-buffer (get-buffer sc3-buffer) (read-only-mode))
	(foxdot-sc3-foxdot-layout)
	(foxdot-do-restart))
      (get-process sc3-process)))
  )

(defun sc3-kill-process ()
  "Kill sc3-process and buffer."
  (interactive)
  (if (get-process sc3-process) (with-current-buffer sc3-buffer (kill-buffer-and-window)))
  )

;;; sc3-install-fd-quark

(defun sc3-install-quark ()
  "Install fixdot quark in SuperCollider."
  (interactive)
  (sc3-send-string "Quarks.install(\"FoxDot\");")
  )
(defalias 'install-quark 'sc3-install-quark)

(defun sc3-recompile-classlib ()
  "Recompile the SC3 class library."
  (interactive)
  (sc3-send-string "thisProcess.recompile();")
  )
(defalias 'recompile-classlib 'sc3-recompile-classlib)


(defun sc3-compile-advice (orig-fun &rest args)
  "Control foxdot quark installation.
When foxdot quark is installed, compile the SC3 class lib.
ORIG-FUN is the advised function and ARGS its arguments."
  (let ((proc (get-process sc3-process)))
    (when (string-match "FoxDot installed" (nth 1 args))
      (advice-remove (process-filter proc) #'sc3-compile-advice)
      (sc3-recompile-classlib))
    (unless (string-match "Shared memory server" (nth 1 args))
      (apply orig-fun args)))
  )

(defun sc3-install-advice (orig-fun &rest args)
  "Control foxdot quark installation.
When SC3 has initialized, install the foxdot quark and recompile class lib.
ORIG-FUN is the adviced function and ARGS its arguments/."
  (let ((proc (get-process sc3-process)))
    (when proc
      (cond ((string-match "Shared memory server" (nth 1 args))
	     (advice-remove (process-filter proc) #'sc3-install-advice)
	     (advice-add 'sc3-insertion-filter :around #'sc3-compile-advice)
	     (sc3-install-quark))
	    (t (apply orig-fun args)))))
  )

;;;###autoload
(defun sc3-install-foxdog-quark ()
  "Run sclang in a Emacs buffer and, if you are in line, will install FoxDot quark.Install foxdot quark."
  (interactive)
  (if (executable-find "sclang")
      (cond ((not (get-buffer sc3-buffer))
	     (sc3-start-process)
	     (advice-add 'sc3-insertion-filter :around #'sc3-install-advice))
	    (t (sc3-install-quark)
	       (advice-add 'sc3-insertion-filter :around #'sc3-compile-advice)))
    (message "sclang is not in PATH or SuperCollider is not installed."))
  )
(defalias 'install-foxdot-quark 'sc3-install-foxdog-quark)
(defalias 'install-fd-quark 'sc3-install-foxdog-quark)
(defalias 'install-fdq 'sc3-install-foxdog-quark)

;;

(defconst sc3-prompt-regexp "sc3>" "Prompt for sclang process buffer.")

(defconst sc3-keywords
  '("play" "load" "plot" "var" "late" "release" "doneAction" "ar" "kr"))

(defconst sc3-functions
  '("sc3>" "Synth" "SynthDef" "SinOsc" "Line" "Env" "CombN" "PMOsc" "MouseY"  "MouseX" "sine" "circle" "EnvGen"))

(defvar sc3-font-lock-keywords
  (list
   `("\"[^\"]*\"" . font-lock-string-face)
   `("/\\*.*[:ascii:].*\\*/" . font-lock-comments)
   `(,(concat "\\_<" (regexp-opt sc3-keywords) "\\_>")  . font-lock-keyword-face)
   `(,(concat "\\_<" (regexp-opt sc3-functions) "\\_>") . font-lock-function-name-face))
  "Additional expressions to highlight in `sc3-mode'.")

;;

(defun sc3-mode-keybindings (map)
  "FoxDot keybindings in MAP."
  (define-key map (kbd "C-z C-s") 'sc3-start-process)
  (define-key map (kbd "C-z C-k") 'sc3-kill-process)

  (define-key map (kbd "C-z C-c") 'sc3-run-line)
  (define-key map (kbd "C-z C-g") 'sc3-run-line-and-go)
  (define-key map (kbd "C-z b") 'sc3-run-block)
  (define-key map (kbd "C-z C-b") 'sc3-run-block-and-go)
  (define-key map (kbd "C-z C-u") 'sc3-hush)
  )

(defun sc3-turn-on-keybindings ()
  "Foxdot keybindings in the local map."
  (interactive)
  (local-set-key (kbd "C-z C-s") 'sc3-start-process)
  (local-set-key (kbd "C-z C-k") 'sc3-kill-process)

  (local-set-key (kbd "C-z C-c") 'sc3-run-line)
  (local-set-key (kbd "C-z C-g") 'sc3-run-line-and-go)
  (local-set-key (kbd "C-z b") 'sc3-run-block)
  (local-set-key (kbd "C-z C-b") 'sc3-run-block-and-go)
  (local-set-key (kbd "C-z C-u") 'sc3-hush)
  )
(add-hook 'sc3t-mode-hook 'sc3-turn-on-keybindings)

(defun foxdot-set-sc3-keybindings ()
  "To turn on sc3 key bindings in other modes buffers."
  (local-set-key (kbd "C-z C-s") 'sc3-start-process)
  (local-set-key (kbd "C-z C-k") 'sc3-kill-process)
  )

(defun sc3-mode-menu (map)
  "FoxDot menu from MAP."
  (define-key map [menu-bar sc3-mode]
    (cons "SC3-FoxDot" (make-sparse-keymap "SC3-FoxDot")))
  
  (define-key map [menu-bar sc3-mode kill-process]
    '("Kill sclang" . sc3-kill-process))
  (define-key map [menu-bar sc3-mode start-process]
    '("Start sclang" . sc3-start-process))
  
  (define-key map [menu-bar sc3-mode separator]
    '(menu-item "--"))
  
  (define-key map [menu-bar sc3-mode run-block-and-go]
    '("Run block and jump" . sc3-run-block-and-go))
  (define-key map [menu-bar sc3-mode run-block]
    '("Run block" . sc3-run-block))
  (define-key map [menu-bar sc3-mode run-line-and-go]
    '("Run line and jump" . sc3-run-line-and-go))
  (define-key map [menu-bar sc3-mode run-line]
    '("Run line" . sc3-run-line))
  )

(unless sc3-mode-map
  (let ((map (make-sparse-keymap "SC3-FoxDot")))
    (sc3-mode-keybindings map)
    (sc3-mode-menu map)
    (setq sc3-mode-map map)))

(defun sc3-reset-mode ()
  "Reset `sc3-mode' in current buffer."
  (interactive)
  (with-current-buffer (current-buffer)
    ( turn-on-foxdot-keybindings)
    (let ((map (make-sparse-keymap "SC3-FoxDot")))
      (sc3-mode-keybindings map)
      (sc3-mode-menu map)
      (setq sc3-mode-map map)))
  (sc3-mode)
  )

(define-derived-mode sc3-mode prog-mode "SC3 FoxDot"
  "Major mode for interacting with an scalng process.
\\<sc3-mode-map>"
  (set (make-local-variable 'font-lock-defaults) '(sc3-font-lock-keywords t))
  (set (make-local-variable 'paragraph-start) sc3-prompt-regexp)
  (turn-on-font-lock)
  )

(define-derived-mode sc3-proc-mode prog-mode "SC3 SCLang"
  "Major mode for interacting with an scalng process.
\\<sc3-proc-mode-map>"
  (set (make-local-variable 'font-lock-defaults) '(sc3-font-lock-keywords t))
  (set (make-local-variable 'paragraph-start) sc3-prompt-regexp)
  (turn-on-font-lock)
  )

(add-to-list 'auto-mode-alist '("\\.sc3\\'" . sc3-mode))

(provide 'foxdot-sc3-mode)
;;; foxdot-sc3-mode ends here
