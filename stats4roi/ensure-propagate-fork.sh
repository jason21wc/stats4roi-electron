#!/usr/bin/env bash
#
# Ensure the bundled R runtime has Steve's Shiny-compatible fork of `propagate`
# (ProfessorPeregrine/propagate) rather than CRAN propagate.
#
# WHY THIS EXISTS
#   CRAN's predictNLS prints to stdout inside Shiny reactive contexts, which
#   corrupts scatterplot confidence/prediction-interval rendering
#   (scatterplot_server.R calls predictNLS 6+ times). The fork is CRAN 1.0-6
#   plus a one-line patch commenting out that message. It is an OLDER base than
#   CRAN 1.0-7, but it is the configuration Steve develops and tests against.
#
#   Any fresh `r-mac/` assembly pulls CRAN propagate from the default repo and
#   silently regresses. app.R only warns at startup — downstream of the damage.
#   This script is the structural fix: run it after assembling r-mac/.
#
# WHY THE FLIBS OVERRIDE
#   r-mac/etc/Makeconf hardcodes FLIBS to /opt/gfortran, which usually does not
#   exist on a dev Mac, so linking fails with "ld: library 'emutls_w' not found".
#   propagate's src/ is pure C/C++/Rcpp — no Fortran — so blanking FLIBS links
#   cleanly (the SHLIB recipe uses -undefined dynamic_lookup).
#
# USAGE
#   ./ensure-propagate-fork.sh            # install only if the fork is missing
#   ./ensure-propagate-fork.sh --force    # reinstall even if already correct
#   ./ensure-propagate-fork.sh --check    # verify only; never install
#
# Exit codes: 0 = runtime has the fork, 1 = it does not (or install failed).

set -euo pipefail

FORK_USER="ProfessorPeregrine"
FORK_REPO="propagate"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LIB="$SCRIPT_DIR/r-mac/library"
RSCRIPT="$SCRIPT_DIR/r-mac/bin/Rscript"
DESC="$LIB/$FORK_REPO/DESCRIPTION"

MODE="ensure"
case "${1:-}" in
  --force) MODE="force" ;;
  --check) MODE="check" ;;
  "")      MODE="ensure" ;;
  *) echo "usage: $(basename "$0") [--force|--check]" >&2; exit 2 ;;
esac

# Read RemoteUsername straight from DESCRIPTION. Deliberately does not start R:
# this must still report correctly when the installed package is broken.
installed_fork_user() {
  [ -f "$DESC" ] || return 1
  awk -F':[[:space:]]*' '/^RemoteUsername:/ { print $2; exit }' "$DESC" | tr -d '\r\n'
}

installed_version() {
  [ -f "$DESC" ] || return 1
  awk -F':[[:space:]]*' '/^Version:/ { print $2; exit }' "$DESC" | tr -d '\r\n'
}

report_state() {
  if [ ! -f "$DESC" ]; then
    echo "propagate: NOT INSTALLED in $LIB"
  else
    echo "propagate: version $(installed_version), RemoteUsername='$(installed_fork_user || echo none)'"
  fi
}

have_fork() {
  [ "$(installed_fork_user 2>/dev/null || true)" = "$FORK_USER" ]
}

# --- preconditions -----------------------------------------------------------

if [ ! -x "$RSCRIPT" ]; then
  echo "ERROR: bundled Rscript not found at $RSCRIPT" >&2
  echo "       Run ./get-r-mac.sh first to assemble the runtime." >&2
  exit 1
fi

report_state

if [ "$MODE" = "check" ]; then
  if have_fork; then
    echo "OK: runtime has the $FORK_USER fork."
    exit 0
  fi
  echo "FAIL: runtime does NOT have the $FORK_USER fork." >&2
  exit 1
fi

if [ "$MODE" = "ensure" ] && have_fork; then
  echo "OK: runtime already has the $FORK_USER fork — nothing to do."
  echo "    (use --force to reinstall anyway)"
  exit 0
fi

# --- install -----------------------------------------------------------------

echo "Installing $FORK_USER/$FORK_REPO into $LIB ..."

# R restores the previous package if the compile fails, but a manual backup is
# cheap insurance against an install that "succeeds" with the wrong content.
BACKUP=""
if [ -d "$LIB/$FORK_REPO" ]; then
  BACKUP="$(mktemp -d)/propagate.bak"
  mkdir -p "$(dirname "$BACKUP")"
  cp -R "$LIB/$FORK_REPO" "$BACKUP"
  echo "Backed up existing propagate to $BACKUP"
fi

MAKEVARS="$(mktemp)"
printf 'FLIBS =\n' > "$MAKEVARS"

set +e
R_MAKEVARS_USER="$MAKEVARS" \
R_LIBS="$LIB" R_LIBS_USER="$LIB" R_LIBS_SITE="$LIB" \
"$RSCRIPT" --vanilla -e "
  .libPaths('$LIB')
  remotes::install_github(
    '$FORK_USER/$FORK_REPO',
    upgrade = 'never', force = TRUE, build = TRUE, lib = .libPaths()[1]
  )
"
INSTALL_RC=$?
set -e
rm -f "$MAKEVARS"

# --- verify ------------------------------------------------------------------
# The install's exit code is not sufficient: verify the artifact on disk.

if have_fork; then
  echo "VERIFIED: $(installed_version) / RemoteUsername=$(installed_fork_user)"
  [ -n "$BACKUP" ] && rm -rf "$(dirname "$BACKUP")"
  exit 0
fi

echo "ERROR: after install, propagate is still not the $FORK_USER fork." >&2
report_state >&2
echo "       install_github exit code was $INSTALL_RC" >&2

if [ -n "$BACKUP" ] && [ -d "$BACKUP" ]; then
  echo "       restoring the backed-up package ..." >&2
  rm -rf "${LIB:?}/$FORK_REPO"
  cp -R "$BACKUP" "$LIB/$FORK_REPO"
  rm -rf "$(dirname "$BACKUP")"
  echo "       restored." >&2
fi
exit 1
