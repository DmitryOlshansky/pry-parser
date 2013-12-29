#!/bin/sh
FLAGS="--build-only --force -O -release -inline -noboundscheck"
rdmd $FLAGS dgrep.d
rdmd $FLAGS relay_hosts.d
