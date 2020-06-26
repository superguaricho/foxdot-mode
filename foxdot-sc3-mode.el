;;; foxdot-sc3-mode.el -- SCLang Mode for FoxDot.

;; Copyright (C) 2020 numa.tortolero@gmail.com
;; Author: numa.tortolero@gmail.com
;; Homepage: https://github.com/superguaricho/foxdot-mode
;; Version: 0.01 (alpha)
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

;; Commentary:
;; SCLang Mode for FoxDot. It would come with foxdot-mode.

;;; Code:

(defvar sc3-cli-file-path (executable-find "sclang")
  "Path to the program used by `run-cli'.")

(defvar sc3-cli-arguments '()
  "Commandline arguments to pass to `cli-cli'.")

(defvar sc3-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    (define-key map "\t" 'completion-at-point)
    map)
  "Basic mode map for `run-cli'.")

(defvar sc3-process "sc3:sclang" "SC3 process name.")

(defvar sc3-buffer "*SC3:SCLang*" "SC3 buffer name.")

;;

(defun sc3-send-string (string)
  "Send STRING to sclang process."
  (let ((p (get-process sc3-process)))
    (when p (process-send-string p (concat string "\n"))))
  )

(defun sc3-test-audio ()
  (interactive)
  (process-send-string (get-process sc3-process)
                       "{ SinOsc.ar(440, 0, Line.kr(0.3, 0, 1, doneAction:2)) }.play\n")
  (sit-for 1)
  )

(defun sc3-insertion-filter (proc string)
  "SC3 PROC process STRING filter."
  (when (buffer-live-p (process-buffer proc))
    (display-buffer (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc))))
        (goto-char (process-mark proc))
        (insert string)
        ;;(if (string-match "*** Welcome to SuperCollider" string)
        ;;    (process-send-string proc "s.boot;\n"))
        (when (string-match "Shared memory server interface initialized" string)
          (process-send-string proc "{ SinOsc.ar(440, 0, Line.kr(0.3, 0, 1, doneAction:2)) }.play\n")
          (sit-for 1)
          (process-send-string proc "FoxDot.start\n"))
        (set-marker (process-mark proc) (point))
        (set-window-point (get-buffer-window sc3-buffer) (point))
        (if moving (goto-char (process-mark proc))))))
  )

(defun sc3-create-process ()
  "Create a SC3:SCLang process."
  (interactive)
  (make-process :name sc3-process
                :buffer sc3-buffer
                :command (list sc3-cli-file-path)
                :coding 'utf-8
                :filter 'sc3-insertion-filter
                :connection-type 'pipe)
  )

(defun sc3-start-process ()
  "Create a SC3:SCLang process."
  (interactive)
  (if (executable-find "sclang")
      (if (and (not (get-process sc3-process))
	       (not (get-buffer "*Python*")))
	  (with-current-buffer (process-buffer (sc3-create-process))
	    (sc3-mode)
	    (get-process sc3-process))
	(message "A sclang process is running."))
    (message "sclang is not in PATH or SuperCollider is not installed."))
  )

(defun sc3-kill-process ()
  "Kill sc3-process and buffer."
  (interactive)
  (if (get-process sc3-process) (with-current-buffer sc3-buffer (kill-buffer-and-window)))
  )

;;; sc3-install-fd-quark

(defun fdsend-str-sc3 (string)
  "Send STRING to sclang process."
  (let ((p (get-process sc3-process)))
    (when p (process-send-string p (concat string "\n"))))
  )

(defun sc3-install-quark ()
  "Install fixdot quark in SuperCollider."
  (interactive)
  (fdsend-str-sc3 "Quarks.install(\"FoxDot\");")
  )

(defun sc3-recompile-classlib ()
  "Recompile the SC3 class library."
  (interactive)
  (fdsend-str-sc3 "thisProcess.recompile();")
  )

(defun sc3-compile-advice (orig-fun &rest args)
  "Control foxdot quark installation.
When foxdot quark is installed, compile the SC3 class lib."
  (let ((proc (get-process sc3-process)))
    (when (string-match "FoxDot installed" (nth 1 args))
      (advice-remove (process-filter proc) #'sc3-compile-advice)
      (sc3-recompile-classlib)))
  (apply orig-fun args)
  )

(defun sc3-install-advice (orig-fun &rest args)
  "Control foxdot quark installation.
When SC3 has initialized, install the foxdot quark and recompile class lib."
  (let ((proc (get-process sc3-process)))
    (when proc
      (cond ((string-match "Shared memory server" (nth 1 args))
	     (advice-remove (process-filter proc) #'sc3-install-advice)
	     (advice-add 'sc3-insertion-filter :around #'sc3-compile-advice)
	     (sc3-install-quark))
	    (t (apply orig-fun args)))))
  )

;;;###autoload
(defun sc3-install-fd ()
  "Install foxdot quark."
  (interactive)
  (if (executable-find "sclang")
      (cond ((not (get-buffer sc3-buffer))
	     (sc3-start-process)
	     (advice-add 'sc3-insertion-filter :around #'sc3-install-advice))
	    (t (sc3-install-quark)
	       (advice-add 'sc3-insertion-filter :around #'sc3-compile-advice)))
    (message "sclang is not in PATH or SuperCollider is not installed."))
  )
(defalias 'install-fd 'sc3-install-fd)
;;

(defconst sc3-prompt-regexp "sc3>" "Prompt for `run-sc3'.")

(defconst sc3-keywords
  '("play" "load" "plot" "var" "Synth" "late"))

(defconst sc3-functions
  '("sc3>" "SinOsc" "Line" "Env" "sine" "circle" "EnvGen"))

(defvar sc3-font-lock-keywords
  (list
   `("\"\\.\\*\\?" . font-lock-string-face)
   `("/\\*.*[:ascii:].*\\*/" . font-lock-comments)
   `(,(concat "\\_<" (regexp-opt sc3-keywords) "\\_>") . font-lock-keyword-face)
   `(,(concat "\\_<" (regexp-opt sc3-functions) "\\_>") . font-lock-function-name-face))
  "Additional expressions to highlight in `sc3-mode'.")

(defun sc3-mode-keybindings (map)
  "FoxDot keybindings in MAP."
  (define-key map (kbd "C-c C-s") 'sc3-start-process)
  (define-key map (kbd "C-c C-k") 'sc3-kill-process)
  )

(defun sc3-mode-menu (map)
  "FoxDot menu from MAP."
  (define-key map [menu-bar sc3-mode]
    (cons "sc3-mode" (make-sparse-keymap "SCLang:sc3-mode")))
  (define-key map [menu-bar sc3-mode quit-foxdot]
    '("kill sclang" . sc3-kill-process))
  (define-key map [menu-bar sc3-mode sclang-start-foxdot]
    '("Start sclang" . sc3-start-sclang))
  )

(unless sc3-mode-map
  (let ((map (make-sparse-keymap "sc3-mode")))
    (sc3-mode-keybindings map)
    (sc3-mode-menu map)
    (setq sc3-mode-map map)))

(define-derived-mode sc3-mode prog-mode "sc3-mode"
  "Major mode for interacting with an scalng process.
\\<sc3-mode-map>"
  (set (make-local-variable 'font-lock-defaults) '(sc3-font-lock-keywords t))
  (set (make-local-variable 'paragraph-start) sc3-prompt-regexp)
  (turn-on-font-lock)
  )

(provide 'foxdot-sc3-mode)
;;; foxdot-sc3-mode ends here
