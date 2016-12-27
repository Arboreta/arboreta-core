#!/bin/sh
cd /home/zannix/src/arboreta-core/src
sbcl --noinform --end-runtime-options --load fancy-repl.cl --end-toplevel-options $*
