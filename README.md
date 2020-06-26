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

<ol>
<li> If you have not doen it, install FoxDot python library. From a shell command line do:

    $ pip install FoxDot

<li> Install Emacs FoxDot mode.

Clone the foxdot-mode project from git in some directory like "\~/.emacs.d" or any directory in "load-path" list. For example, from the command line, you can create a directory like "\~/.emacs.d/site-lisp/" (mkdir ~/.emacs.d/site-lisp), move to that directory (cd ~/.emacs.d/site-lisp), and clone the repository:

    $ git clone https://github.com/superguaricho/foxdot-mode

In your "\~/.emacs" initialization file, add the following lines:

    (add-to-list 'load-path (expand-file-name "site-lisp/foxdot-mode" "~/.emacs.d"))
    (require 'foxdot-mode)

Where «(expand-file-name "site-lisp/foxdot-mode" "~/.emacs.d")» evaluates to «/home/user/.emacs.d/site-lisp/foxdot-mode».

Evaluate those lines or restart Emacs.

<li> Install the SuperCollider FoxDot quark.

Assuming that you have installed SuperCollider, from Ecmacs do Alt-x install-fd

This run sclang in a Emacs buffer and, if you are in line, will install FoxDot quark and recompile the SuperCollider class library.

Youst hear a simple sound. If not, do Alt+x test-sc3 ENTER. If you don't hear it, there is a problem with SuperCollider or your audio system.

<li> Start foxdot.

If you hearded the sound, you can continue.

Open a file with .foxdot extension.  Type Alt+x foxdot ENTER. This run sclang and FoxDot process buffers.

Wait and you will see three horizontal windows: the .foxdot file (your workspace), the  *FoxDot* and the *SCLang:SC3* buffers.

SuperCollider is now listening for messages from FoxDot.

<li> Enjoy now. Play with some codes.

For example, type in your workspace:

    p1 >> pluck([12], dur=0.25, echo=0.8)

With the cursor over the line type Ctrl+c Ctrl+c.

Do Ctrl+c Ctrl+u to stop the sounds.

</ol>

If you want FoxDot launch when you open "myfile.foxdot", add the following lines to ~/.emacs:

    (add-to-list 'auto-mode-alist '("\\.foxdot)?$" . foxdot-mode))
    (add-hook 'foxdot-mode-hook 'foxdot)

If you do this, don't need use Alt+x foxdot to play. The sclang and FoxDot interpreter will launch when you open a .foxdot file. If you have problem with foxdot-sc3-mode add instead:

    (add-to-list 'auto-mode-alist '("\\.foxdot)?$" . foxdot-mode))
    (add-hook 'foxdot-mode-hook 'foxdot-start-foxdot)

I have cloned the foxdot-mode repository in "~/.emacs/site-lisp" path and added these lines to my ~/.emacs file:

    (add-to-list 'load-path (expand-file-name "site-lisp/foxdot-mode" user-emacs-directory))
    (require 'foxdot-mode)
    (add-to-list 'auto-mode-alist '("\\.foxdot)?$" . foxdot-mode))
    (add-hook 'foxdot-mode-hook 'foxdot-start-foxdot)

Now, when I open a .foxdot file in Emacs, start FoxDot, creates a \*FoxDot\* process and I can write and evaluate my livecoding lines, seting the cursor over the line that I want execute and using the folowing keys:

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

You can start sclang and foxdot interpreters with:

    Ctrl+c Ctrl+s (foxdot-sclang-foxdot-start)

To quit sclang and foxdot: Alt+x kill-foxdot ENTER, or:
 
    Ctrl+c Ctrl+k (foxdot-sclang-foxdot-quit)

Other intesting keys:

    Ctrl+c s (foxdot-sclang-start). Run sclang process only.
    Ctrl+c k (foxdot-sclang-kill).  Kill sclang process only.
    Ctrl+c f (foxdot-start-foxdot). Run python foxdot only.
    Ctrl+c q (foxdot-kill-foxdot).  Kill python fpcdpt only.

Layouts:

Foxdot-mode can show three layouts:

    Ctrl+c w (foxdot-set-sc3-layout). Three vertical windows: workspace, foxdot and sclang. 
    Ctrl+c Ctrl+w (foxdot-set-foxdot-layout). Two windows: workspace and foxdot.
    Ctrl+c 3 (foxdot-sc3-foxdot-layout). Two windows: workspace and sclang.

<h2>Todo</h2>

Add to foxdot-sc3-mode.el functions to interactively work with sclang from a buffer with sc3-mode.

Some code hiliting to foxdot-mode.

<h2>Problems</h2>

This code is in alpha state, is not very tested (June, 2020).

<h2>Acknowledgments</h2>

Thanks to Jean Argenty from TOPLAB for its ideas and help testing this codes.
