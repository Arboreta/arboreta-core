# Arboreta - Lisp Environment

A userspace and IDE inspired by those monolothic systems as found in the Symbolics Lisp Machines, 
Pharo Smalltalk, or Project Oberon; realized in Common Lisp atop a Linux host.

Arboreta uses pango for font rendering. If your antialiased fonts don't look as nice as they should,
it's likely your fontconfig is misconfigured. In particular, make sure 
[hinting is set to full](https://wiki.archlinux.org/index.php/Font_Configuration#Hinting), 
and fontconfig knows which 
[subpixel rendering](https://wiki.archlinux.org/index.php/Font_Configuration#Subpixel_rendering)
format to use, for the font in question.

The individual applications will be available in the full environment, 
as well as in some limited capacity as standalone WM applications.

![scrot](https://raw.githubusercontent.com/Arboreta/arboreta-core/master/repl.png)

## dependencies and installation

Arboreta depends on xlib, cairo, and pango, the debian packages for which are: libx11-dev, libcairo2-dev, and libpango1.0-dev.

Arboreta requires quicklisp to run. It's been tested on sbcl, but should work on other CL implementations.
to install quicklisp, head over to [quicklisp's website](https://www.quicklisp.org/beta/) and follow the instructions there.
Make sure you run `(ql:add-to-init-file)`, otherwise quicklisp won't be avaliable when you start your interpreter.

To use arboreta, clone the repo somewhere, cd into it, and run `./install.sh`. It will generate scripts which you can call
to run the various applications.

# Current Roadmap:

* window clipping
* window resizing
* mouse event handling
* figure out how to get font information, make everything relative to that
* arboreta-specific constructs for font, text, editable text, ect.
* design data-based or hybrid window system, which can be optimized a lot whole more than the lambda model
* figure out how to get screen dimensions for wm mode so it's the right size
* reduce memory usage?

# Near Future:

* figure out how to interface with sbcl in such a way conditions can be displayed graphically
* general interface primitives -- menus, items, ect.
* parsing .xresources for font and colors
* install script that auto-generates the sesssion manager and desktop files
* listener REPL ala swank
* symbol translation table layer
* (discuss) importing/managing CL libraries, files
* (discuss) "windowing"
* keybindings object
* (discuss) symbol translation table editor/navigator
* (discuss) S-EXP/image/dependency editor/navigator
* (discuss) debugger
* (discuss) clipboard and text selection

# Down the road:

* import/exporting/navigating plaintext
* Common Lisp Hyperspec import
* symbol/context inspection -> hyperspec/debugging
* macro recording/management
* syntax highlighting
* theming
* state recording/garbage collector
* symbol/sexp use analysis/statistics
* autocomplete
* QuickLisp documentation import
* advanced Linux binds
* genetic programming with symbol statistics
* symbol categorization
* contextual autocomplete
* web browser
* 3D interface/ricing

