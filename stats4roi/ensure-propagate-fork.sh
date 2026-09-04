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
# WHY THE PKG_LIBS OVERRIDE
#   The fork's src/Makevars builds PKG_LIBS from `$(R_HOME)/bin/Rscript -e
#   "Rcpp:::LdFlags()"`. That call returns nothing useful (Rcpp has been
#   header-only for years) but the bundled Rscript binary hops to the system R
#   wrapper, whose "WARNING: ignoring environment value of R_HOME" lands on the
#   link line and breaks it. The user Makevars is read after the package's, so
#   PKG_LIBS is pinned to the LAPACK/BLAS/FLIBS remainder here.
#
# WHY bin/R WITH R_HOME_DIR, NOT bin/Rscript
#   The bundled `Rscript` binary hardwires R_HOME to /Library/Frameworks and
#   silently runs the *system* R when one is installed, so the fork used to be
#   compiled under a different R version than the one it ships in (R warned
#   "package 'propagate' was built under R version 4.5.2" at every launch).
#   The `bin/R` shell wrapper honours R_HOME_DIR from the environment and sets
#   DYLD_LIBRARY_PATH itself, so it and its `R CMD INSTALL` children stay inside
#   r-mac/. --check also fails if the installed build's R version differs from
#   the bundled runtime's.
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
R_HOME_DIR="$SCRIPT_DIR/r-mac"
export R_HOME_DIR
RBIN="$R_HOME_DIR/bin/R"
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

# "Built: R 4.5.1; aarch64-apple-darwin20; ..." -> "4.5.1"
installed_built_r() {
  [ -f "$DESC" ] || return 1
  awk -F':[[:space:]]*' '/^Built:/ { print $2; exit }' "$DESC" \
    | sed -E 's/^R ([0-9.]+);.*/\1/' | tr -d '\r\n'
}

# Version of the bundled runtime, read from its base package (no R process).
bundled_r_version() {
  local base="$LIB/base/DESCRIPTION"
  [ -f "$base" ] || return 1
  awk -F':[[:space:]]*' '/^Version:/ { print $2; exit }' "$base" | tr -d '\r\n'
}

report_state() {
  if [ ! -f "$DESC" ]; then
    echo "propagate: NOT INSTALLED in $LIB"
  else
    echo "propagate: version $(installed_version), RemoteUsername='$(installed_fork_user || echo none)', built under R $(installed_built_r || echo '?') (bundled runtime is R $(bundled_r_version || echo '?'))"
  fi
}

have_fork() {
  [ "$(installed_fork_user 2>/dev/null || true)" = "$FORK_USER" ]
}

built_under_bundled_r() {
  local built bundled
  built="$(installed_built_r 2>/dev/null || true)"
  bundled="$(bundled_r_version 2>/dev/null || true)"
  [ -n "$built" ] && [ -n "$bundled" ] && [ "$built" = "$bundled" ]
}

# --- preconditions -----------------------------------------------------------

if [ ! -x "$RBIN" ] || [ ! -x "$R_HOME_DIR/bin/exec/R" ]; then
  echo "ERROR: bundled R not found at $RBIN" >&2
  echo "       Run ./get-r-mac.sh first to assemble the runtime." >&2
  exit 1
fi

report_state

if [ "$MODE" = "check" ]; then
  if ! have_fork; then
    echo "FAIL: runtime does NOT have the $FORK_USER fork." >&2
    exit 1
  fi
  if ! built_under_bundled_r; then
    echo "FAIL: the fork was built under a different R than the bundled runtime (run --force)." >&2
    exit 1
  fi
  echo "OK: runtime has the $FORK_USER fork, built under the bundled R."
  exit 0
fi

if [ "$MODE" = "ensure" ] && have_fork && built_under_bundled_r; then
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
printf 'FLIBS =\nPKG_LIBS = $(LAPACK_LIBS) $(BLAS_LIBS) $(FLIBS)\n' > "$MAKEVARS"

set +e
R_MAKEVARS_USER="$MAKEVARS" \
R_LIBS="$LIB" R_LIBS_USER="$LIB" R_LIBS_SITE="$LIB" \
"$RBIN" --vanilla --no-echo -e "
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

if have_fork && built_under_bundled_r; then
  echo "VERIFIED: $(installed_version) / RemoteUsername=$(installed_fork_user) / built under R $(installed_built_r)"
  [ -n "$BACKUP" ] && rm -rf "$(dirname "$BACKUP")"
  exit 0
fi

echo "ERROR: after install, propagate is not the $FORK_USER fork built under the bundled R." >&2
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
