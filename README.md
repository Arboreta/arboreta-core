# Arboreta - Lisp Environment

Arboreta is a graphical framework designed to make prototyping and writing common lisp applications easy,
and aims to host a full suite of development tools, so common lisp development can be done without having to rely on other languages.
Conceptually, it's somewhere between [Sketch](https://github.com/vydd/sketch) and [McCLIM](https://github.com/robert-strandh/McCLIM), 
covering both basic drawing and interface design.

![scrot](https://raw.githubusercontent.com/Arboreta/arboreta-core/master/repl.png)

## dependencies and installation

Arboreta depends on xlib, xkb, cairo, and pango, the debian packages for which are: libx11-dev, libxkbcommon-x11-dev, libcairo2-dev, and libpango1.0-dev.

Arboreta requires quicklisp to run. It's been tested on sbcl, but should work on other CL implementations.
to install quicklisp, head over to [quicklisp's website](https://www.quicklisp.org/beta/) and follow the instructions there.
Make sure you run `(ql:add-to-init-file)`, otherwise quicklisp won't be avaliable when you start your interpreter.

