#!/usr/bin/shell/bash

OBJ=$1
case "$OBJ" in
    *.mp|*.pset|*.plan) air sandbox run ${OBJ} ;;
    *)  ksh ${OBJ}  ;;
esac;
RC=$?
if [ ${RC} -eq 0 ]; then echo "Command Completed Successfully"; else echo "Command failed!"; fi;