#!/bin/bash
if [ -z "$COMPILER" ] ; then
	COMPILER=dmd
fi
#COMPILER=ldmd2
FLAGS="--compiler=$COMPILER --build-only --force -O -release -inline -noboundscheck"
rdmd $FLAGS dgrep.d
rdmd $FLAGS relay_hosts.d
