#!/bin/sh
cd /home/arathnim/repos/arboreta-core/src
sbcl --noinform --end-runtime-options --load fancy-repl.cl --end-toplevel-options $*
