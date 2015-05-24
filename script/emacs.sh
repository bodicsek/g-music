#!/bin/bash
rm -f g-music.elc
cask exec emacs -nw -Q --directory $PWD --eval "(progn (require 'g-music) (g-music))" $@
