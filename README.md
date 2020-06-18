# foxdot-mode
<a href="http://foxdot.org">FoxDot</a> Mode for <a href="https://www.gnu.org/software/emacs/">Emacs</a>

Version: 0.01 (alpha)

Author: numa.tortolero@gmail.com

This Emacs configuration file allows you to interact with FoxDot (http://foxdot.org) for live coding patterns.

Inspiration: tidal.el (from <a href="https://tidalcycles.org/index.php/Welcome">TidalCycles</a> project), hsc3.el (form <a href="https://github.com/rd--/hsc3/tree/master/emacs">hsc3</a> project) and <a href="https://gitlab.com/umejam/foxdot-mode">foxdot-mode</a>.

<h2>Requirements</h2>

<a href="https://supercollider.github.io/">SuperCollider</a>, python with <a href="http://foxdot.org">FoxDot</a> library. You will need too python package for <a href="https://www.gnu.org/software/emacs/">Emacs</a>; this comes with your Emacs distribution.

<h2>Instructions</h2>

We assume that you are working in Linux.

<b>(1)</b> If you have not doen it, install FoxDot. From a shell command line do:

    $ pip install FoxDot

<b>(2)</b>  Assuming that you have installed SuperCollider, then surely you must have installed
  the SuperCollider Emacs package. In Debian and Ubuntu: supercollider-emacs.

    $ sudo apt install supercollider-emacs

  In your ~/.emacs configuration file add:

    (require 'sclang)

  Evaluate that line or restart Emacs and run the SuperCollider server typing
  
    Alt+x sclang-start ENTER.

  Test the audio. In the *SCLang:Workspace* buffer type:

    { SinOsc.ar(440, 0, Line.kr(0.3, 0, 1, doneAction:2)) }.play
    Ctlc-c Ctl-c

   You must hear a simple sound. If you don't hear it, something is wrong,
   there is a problem with SuperCollider configuration or your audio system.

<b>(3)</b> Install FoxDot quark, evaluating the following line in \*SCLang:Workspace\*
buffer:

    Quarks.install("FoxDot")

Recompile the SuperCollider class library:

    Alt-x sclang-recompile

<b>(4)</b> Start FoxDot. In \*SCLang:Workspace\*, evaluate the following line:

    FoxDot.start

SuperCollider is now listening for messages from FoxDot.

<b>(5)</b> Install Emacs FoxDot mode. Clone the foxdot-mode project from git in some directory like "\~/.emacs.d" or any directory in "load-path" list. For example, from the command line, you can create a directory like "\~/.emacs.d/site-lisp/" (mkdir ~/.emacs.d/site-lisp), move to that directory (cd ~/.emacs.d/site-lisp), and clone the repository:

    $ git clone https://github.com/superguaricho/foxdot-mode

In your "\~/.emacs" initialization file, add the following lines:

    (add-to-list 'load-path (expand-file-name "site-lisp/foxdot-mode" "~/.emacs.d"))
    (require 'foxdot-mode)

Where «(expand-file-name "site-lisp/foxdot-mode" "~/.emacs.d")» evaluates to «/home/user/.emacs.d/site-lisp/foxdot-mode».

Evaluate those lines or restart Emacs.

<b>(6)</b> Open a file with .py or .foxdot extension.

<b>(7)</b> Start foxdot, typing: Alt+x foxdot ENTER

If you want FoxDot buffer launch when you open "myfile.foxdot", add the following lines to ~/.emacs:

    (add-to-list 'auto-mode-alist '("\\.foxdot)?$" . foxdot-mode))
    (add-hook 'foxdot-mode-hook 'foxdot)

If you do this, don't need use Alt+x foxdot. The FoxDot interpreter will launch when you open a .foxdot file.

That is all.

I have cloned the foxdot-mode repository in "~/.emacs/site-lisp" path and added these lines to my ~/.emacs file:

    (add-to-list 'load-path (expand-file-name "site-lisp/foxdot-mode" user-emacs-directory))
    (require 'foxdot-mode)
    (add-to-list 'auto-mode-alist '("\\.foxdot)?$" . foxdot-mode))
    (add-hook 'foxdot-mode-hook 'foxdot-start-foxdot)

Now, when I open a .foxdot file in Emacs, it launchs SuperCollider, start FoxDot, creates a \*FoxDot\* process and I can write and evaluate my livecoding lines, seting the cursor over the line that I want execute and using the folowing keys:

    Ctrl+c Ctrl+c (foxdot-run-line)
    Ctrl+c Ctrl+g (foxdot-run-line-and-go). This command send a line to the interpreter and
                                            advance the cursor to he next non blank line.
    Ctrl+c b (foxdot-run-block). Send the block where is the cursor to the interpreter.
    Ctrl+c Ctrl+b (foxdot-run-block-and-go). Send the block where is the cursor to the interpreter and
    advance the cursor to he next non blank line.
    Ctrl+c e (foxdot-execute-block). Send the paragraphe or block where is the cursor with echo to the interpreter.
    Ctrl+c Ctrl+e (foxdot-execute-block-and-go). Send the paragraphe or block where is the cursor with echo to the interpreter
    and go to the next non blank line.
    Ctrl+c Ctrl+r (foxdot-run-region). Send the selected region to the interpreter.
    Ctrl+c n (foxdot-run-block-by-lines).  Send a block line by line.
    Ctrl+c o (foxdot-run-block-by-lines-and-go).  Send a block line by line and go to next non empty line.
    Ctrl+c Ctrl+a (foxdot-clear-foxdot).  Clear the foxdot interpreter screen.
    Ctrl+c Ctrl+u (foxdot-hush).  Mute foxdot sending "Clock.clear()" command to the interpreter.

You can start foxdot interpreter with:

    Ctrl+c s (foxdot-start-foxdot)

To quit foxdot: Alt+x kill-foxdot ENTER, or:

    Ctrl+c q (foxdot-kill-foxdot)

You can work on SuperCollider at same time. If FoxDot is running, you can use:

    Ctrl+c 3 (foxdot-set-sc3-layout)

This set a window layout where you can see the \*SCLang:Workspace\* and the
\*SCLang:PostBuffer\* buffers. You can create your own synths and sounds in sclang
language. If you want to see again your original foxdot and \*FoxDot\* buffers,
type:

    Ctrl+c f (foxdot-set-foxdot-layout)

I you want to see the foxdot commands echo in \*FoxDot\* and \*SCLang:PostBuffer\*
buffers, type:

    Ctrl+c w (foxdot-sc3-foxdot-layout)

<h2>Problems</h2>

This code is in alpha state, is not very tested (June, 2020).

<h2>Acknowledgments</h2>

Thanks to Jean Argenty from TOPLAB for its ideas and help testing this codes.
