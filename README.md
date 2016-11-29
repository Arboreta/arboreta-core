# Arboreta - Lisp Environment

A userspace and IDE inspired by those monolothic systems as found in the Symbolics Lisp Machines, 
Pharo Smalltalk, or Project Oberon; realized in Common Lisp atop a Linux host.

Arboreta uses pango for font rendering. If your antialiased fonts don't look as nice as they should,
it's likely your fontconfig is misconfigured. In particular, make sure 
[hinting is set to full](https://wiki.archlinux.org/index.php/Font_Configuration#Hinting), 
and fontconfig knows which 
[subpixel rendering](https://wiki.archlinux.org/index.php/Font_Configuration#Subpixel_rendering)
format to use, for the font in question.

# Current Roadmap:

* window clipping
* design data-based or hybrid window system, which can be optimized a lot whole more than the lambda model
* frame drawing limitations/fix flicker issues
* xlib for the mouse
* window resizing, also xlib
* figure out how to get screen dimensions for wm mode?

# Near Future:

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

