#!/bin/sh
FLAGS="--build-only -O -release -inline -noboundscheck"
rdmd $FLAGS dgrep.d
rdmd $FLAGS relay_hosts.d
