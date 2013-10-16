While not being a kata, setup of the environment in which it's possible to do the programming for them is a task that needs to be fulfilled anyway. I hence see that as some sort of a separate kata, to familiarize oneself with various development environments.

These are the requirements for setting up a Clojure development environment on Windows using portable apps.
In more detail these are the exact requirements:

* All portable applications are available on drive `J:` This is a USB stick in my case.
* We'll need git, emacs and clojure of course.
* We'll also need leiningen at some later stage.
* Configuration of the application will be stored on the external drive (e.g. `J:`) as well where possible.
* We'll use nrepl instead of slime/swank. I never got the latter set up to work correctly as portable apps.


This describes the setup I'm currently using. First of all, the downloads. For the stuff that has repositories on github, I would suggest using `git clone <repositoryURI>` (after you've installed git in the first place, of course).

* Emacs (24.2 at the time of writing) can be downloaded from the [FSF Emacs server](http://ftp.gnu.org/gnu/emacs/windows/)
* git (1.7.6 at the time of writing) can be downloaded from [msys](http://code.google.com/p/msysgit/)
* leiningen requires wget, which can be installed e.g. from [here](http://gnuwin32.sourceforge.net/packages/wget.htm). Another option would be to install MinGW with git and wget, cf. [MinGW](http://www.mingw.org/)
* clojure (1.5.0 at the time of writing) can be downloaded from [Clojure.org](http://clojure.org), of course.
* leiningen version 2 can be downloaded from [leininigen github repo](https://github.com/technomancy/leiningen)
* clojure-mode version 2 can be installed via Marmalade/ELPA or manually from it's github [repository](https://github.com/technomancy/clojure-mode) 
* nrepl.el can be also be installed via Marmalade/ELPA or manually from it's github [repository](https://github.com/kingtim/nrepl.el)
* I'll throw in `magit` for smooth Emacs interaction with `git`, to be fetched via Marmalade/ELPA or manually from [magit's repository](https://github.com/magit/magit)

I'll use the following directory layout:  All applications are stored under `J:\\progs\`, e.g. Emacs 24.2 will end up as `J:\\progs\emacs-24.2\`. I put the `clojure.jar` into `J:\\progs\\clojure\` and will put `lein.bat` along with it into the clojure directory. The following shows the resulting directory as shown by dired:

	  J:
	  insgesamt 5124
	  drwx------  3 schauer schauer   16384 Nov  1  2011 emacs
	  drwx------ 12 schauer schauer   16384 Okt 16  2012 progs
	
	  J:\\progs:
	  drwx------  8 schauer schauer     16384 Nov  1  2011 clojure
	  drwx------  8 schauer schauer     16384 Okt  7  2012 emacs-24.2
	  drwx------ 11 schauer schauer     16384 Nov  1  2011 git
	  drwx------  8 schauer schauer     16384 Nov  1  2011 wget
	

My Emacs configuration resides in a separate directory on `J:`, namely in `J:\\emacs\`. As I already have quite a lot of emacs configuration, I'm going to put all configuration options into separate files, which are placed in `J:\\emacs\elisp\config\`. Code from other people will go in separate directories as well, with `J:\\emacs\elisp\others\` as the top-level folder.  `clojure-mode` hence goes to `J:\\emacs\elisp\others\clojure-mode`. `nrepl.el` is a mode but a single file and goes straight into `J:\\emacs\elisp\others\`. Emacs looks for default.el or site-start.el during startup to look for personal or site-wide configuration. Both files can be placed in the site-lisp directory, i.e. in `J:\\progs\emacs-24.2\site-lisp\`

	  J:\\emacs:
	  drwx------  6 schauer schauer   16384 Nov  1  2011 elisp
	
	  J:\\emacs\elisp:
	  drwx------ 3 schauer schauer 16384 Nov  1  2011 config
	  drwx------ 4 schauer schauer 16384 Nov  1  2011 development
	  drwx------ 6 schauer schauer 16384 Nov  1  2011 others

Next we need to adopt the load-path, i.e. where Emacs looks for libraries. This means we need to put some content in `J:\\progs\emacs-24.2\site-lisp\default.el` that takes care of figuring out the drive letter and sets paths correctly:


	(defun get-drive-from-filename (filename)
	  "Returns a windows drive letter if filename contains a drive letter."
	  (if (string-match "^\\(.:\\)/" filename)
	      (match-string 1 filename)))
	
	(defun get-drive-for-emacspath ()
	  "Returns windows drive letter for the drive emacs can be found on."
	  (get-drive-from-filename (getenv "EMACSPATH")))
	
	(let ((emacsdrive (get-drive-for-emacspath))
	       loadpath-additions)
	  (dolist (dirname
		   '("/emacs/elisp/"
		     "/emacs/elisp/config/" 
		     "/emacs/elisp/others/"
		     "/emacs/elisp/others/clojure-mode/"))
	    (setq loadpath-additions
		  (cons (concat emacsdrive dirname) loadpath-additions)))
	  (setq load-path
		(append loadpath-additions load-path)))
	    
	(require 'nrepl)	    
	(require 'clojure-mode)
	(setq clojure-mode-inf-lisp-command 
	      (concat (get-drive-for-emacspath)
		       "/progs/clojure/lein.bat repl"))
	
	
	(require 'magit)
	(setq magit-git-executable
	      (concat (get-drive-for-emacspath)
		       "/progs/git/bin/git"))


The next step is to install leiningen. There are two ways: either downloading `lein.bat` and running it from cmd or downloading `lein`, the shell script and running it via the `git` bash prompt. I chose the latter. You will probably need to adjust your path to where you put the lein shell script, e.g. (bash syntax):

	export PATH=$PATH:/j/progs/clojure/

To install leiningen locally (i.e. not in your `%HOME%`), you have to set the `LEIN_HOME` environment variable, i.e. like this (bash syntax):

	export LEIN_HOME=/j/progs/clojure

Remember to always set this variable afterwards before running leiningen commands. Point your classpath to where you installed clojure: 

	export CLASSPATH=/j/progs/clojure/clojure-1.5.0/clojure-1.5.0

If you don't want to set all these variables all the time, you can put them either in a `.profile` file in your `%HOME%` or in the global profile file that comes with git which resides in `/j/progs/git/etc/`. I added the following lines:

	CLOJUREPATH=/j/progs/clojure 
	if test -x $CLOJUREPATH
	then 
	     export PATH=$PATH:$CLOJUREPATH
	     export LEIN_HOME=$CLOJUREPATH
	     export CLASSPATH=$CLOJUREPATH/clojure-1.5.0/clojure-1.5.0
	else
	     echo "Can not access /j/progs/clojure"
	     exit 1
	fi
	
To figure out how to get rid of the hardcoded drive letter in bash is left as an exercise to the reader (hint: `pwd` and `cut` are your friends).

If you also want to keep the files / jars which leiningen retrieves in a local, non-standard maven repository, you need to set a variable in your `$LEIN_HOME/profiles.clj` file, like this:

	{:user {:local-repo "j://progs/clojure/.m2/"
	        :repositories  {"local" {:url "file://j/progs/clojure/.m2"
	                                  :releases {:checksum :ignore}}}
	        :plugins [[lein-localrepo "0.5.2"]]}}
	
Then run `lein self-install`. Afterwards, a `lein repl` should give you a Clojure read-eval-print-loop.

Now if you want to use `nrepl` and would like to use the support for nrepl/inferior-lisp which comes with `clojure-mode` you need to add a corresponding dependency to your `project.clj` for each project, cf. [nrepl installation](https://github.com/clojure/tools.nrepl#installation-)




