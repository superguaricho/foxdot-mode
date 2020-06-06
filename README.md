# foxdot-mode
<a href="http://foxdot.org">FoxDot</a> Mode for <a href="https://www.gnu.org/software/emacs/">Emacs</a>

Version: 0 (alpha)

Author: numa.tortolero@gmail.com

This Emacs configuration file allows you to interact with FoxDot (http://foxdot.org) for live coding patterns.

Inspiration: tidal.el (from <a href="https://tidalcycles.org/index.php/Welcome">TidalCycles</a> project), hsc3.el (form <a href="https://github.com/rd--/hsc3/tree/master/emacs">hsc3</a> project) and <a href="https://gitlab.com/umejam/foxdot-mode">foxdot-mode</a>.

<h2>Requirements</h2>

<a href="https://supercollider.github.io/">SuperCollider</a>, python with <a href="http://foxdot.org">FoxDot</a> library. You will need too python package for <a href="https://www.gnu.org/software/emacs/">Emacs</a>; this comes with your Emacs distribution.

<h2>Instructions</h2>

We assume that you are working in Linux.

<b>(1)</b> If you have not doen it, install FoxDot. From a shell command line do:

    $ pip install FoxDot

  Start SuperCollider and install FoxDot quark, evaluating the following line:

    Quarks.install("FoxDot")

  Recompile the SuperCollider class library by going to "Menu -> Language -> Recompile Class Library" or pressing Ctrl+Shift+L.

<b>(2)</b> Start FoxDot. Open SuperCollider and evaluate the following (this needs to be done before opening FoxDot):

    FoxDot.start

   SuperCollider is now listening for messages from FoxDot.

<b>(3)</b> Install Emacs FoxDot mode. Put the file "foxdot-mode.el" in some directory as "\~/.emacs.d" or any directory in "load-path" list. For example, you can create a directory as "\~/.emacs.d/site-lisp/foxdot-mode", put "foxdot.mode.el" there and, in your "\~/.emacs" initialization file, add the following lines:

    (add-to-list 'load-path (expand-file-name "site-lisp/foxdot-mode" "~/.emacs.d"))
    (require 'foxdot-mode)

Where «(expand-file-name "site-lisp/foxdot-mode" "~/.emacs.d")» evaluates to «/home/user/.emacs.d/site-lisp/foxdot-mode».

Evaluate those lines or restart Emacs.

<b>(4)</b> Open a file with .py or .foxdot extension.

<b>(5)</b> Start foxdot, typing: Alt+x foxdot ENTER

If you want FoxDot buffer launch when you open "myfie.foxdot", add the following lines to ~/.emacs:

    (add-to-list 'auto-mode-alist '("\\.foxdot)?$" . foxdot-mode))
    (add-hook 'foxdot-mode-hook 'foxdot-start-foxdot)

If you do this you don't need use Alt+x foxdot. The FoxDot interpreter will launch when you open a .foxdot file.

That is all.

I have cloned the foxdot-mode repository in "~/.emacs/site-lisp" path and added these lines to my ~/.emacs file:

    (add-to-list 'load-path (expand-file-name "site-lisp/foxdot-mode" user-emacs-directory))
    (require 'foxdot-mode)
    (add-to-list 'auto-mode-alist '("\\.foxdot)?$" . foxdot-mode))
    (add-hook 'foxdot-mode-hook 'foxdot-start-foxdot)

Now, when I have started SuperCollider and open a .foxdot file in Emacs, it launchs *FoxDot* process and I can write and evaluate my livecoding lines, seting the cursor over the line that I want execute and using the folowing keys:

    Ctrl+c Ctrl+c (foxdot-run-line)
    Ctrl+c Ctrl+g (foxdot-run-line-and-go). This command send a line to the interpreter and
                                            advance the cursor to he next non blank line.
    Ctrl+c e (foxdot-run-block). Send the paragraphe or block where is the cursor to the interpreter.
    Ctrl+c Ctrl+e (foxdot-run-block). Send the paragraphe or block where is the cursor to the interpreter
                                            and go to the next non blank line.
    Ctrl+c Ctrl+e (foxdot-run-region). Send the selected region to the interpreter.
    Ctrl+c Ctrl+a (foxdot-clear-foxdot).  Clear the foxdot interpreter screen.

You can start foxdot interpreter with:

    Ctrl+c i (foxdot-start-foxdot)

To quit foxdot: Alt+x kill-foxdot ENTER, or:

    Ctrl+c q (foxdot-kill-foxdot)

<h2>Problems</h2>

Is not compatible with elpy.

This code is in alpha state, are not very tested, by that reason is not in Melpa (June, 2020).

