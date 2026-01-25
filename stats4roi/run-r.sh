#!/bin/bash
export R_HOME="$(pwd)/r-mac"
export R_LIBS_USER="$R_HOME/library"
export PATH="$R_HOME/bin:$PATH"
"$R_HOME/bin/Rscript" "$@"
