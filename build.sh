#!/bin/bash
COMPILER=dmd
#COMPILER=ldmd2
FLAGS="--compiler=$COMPILER --build-only --force -O -release -inline -noboundscheck"
rdmd $FLAGS dgrep.d
rdmd $FLAGS relay_hosts.d
