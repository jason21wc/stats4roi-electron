# Learning Log

> Episodic memory — lessons learned, patterns discovered, mistakes to avoid.
> Append new entries; do not modify old ones.

## Format

```markdown
### [DATE] — [SHORT TITLE]

**Context:** What was happening
**Lesson:** What was learned
**Apply When:** Future situations where this applies
```

---

## Entries

### 2024-01-24 — Gatekeeper Extended Attributes

**Context:** DMG downloaded from GitHub showed "damaged" error on macOS.
**Lesson:** macOS Gatekeeper adds quarantine extended attributes to downloaded files. Users must either right-click → Open or strip attributes with `xattr -cr`.
**Apply When:** Any unsigned app distribution; document this in installation instructions.

### 2024-01-24 — Upstream Sync Pattern

**Context:** Needed to update from Steve's stats4ROI repository.
**Lesson:** One-way pull from `deployment/` directory works well. Don't modify upstream code; just package it.
**Apply When:** Any future updates from professorperegrine/stats4ROI.

### 2026-06-28 — propagate Must Be Steve's Shiny Fork, Not CRAN

**Context:** v4.2.0 (and the bundled `r-mac` runtime) shipped CRAN `propagate` 1.0-7. Steve's app requires the fork [ProfessorPeregrine/propagate](https://github.com/ProfessorPeregrine/propagate) — pinned in `deployment/DESCRIPTION` Remotes, the README, and two code comments. `app.R` warns at startup when `packageDescription("propagate")$RemoteUsername != "ProfessorPeregrine"`.
**Lesson:** The fork is CRAN **1.0-6 + a one-line patch**: `R/predictNLS.R` comments out the `message("...Propagating predictor value...")` that otherwise spams stdout inside Shiny reactive contexts and corrupts scatterplot confidence/prediction-interval rendering (`scatterplot_server.R` calls `predictNLS` 6+ times). It is an *older base* than CRAN 1.0-7 but is the configuration Steve develops/tests against, so it is correct, not a regression. The structural cause of the recurring miss: `r-mac` is assembled without a fork-install step, so it silently regresses to CRAN on every runtime rebuild; `app.R`'s warning only detects this downstream.
**Apply When:** Any time the `r-mac` runtime is rebuilt or refreshed. After assembling it, ALWAYS reinstall the fork (see PROJECT-MEMORY "R Runtime Assembly") and confirm the smoke-test log shows NO "does not appear to be the Shiny fork" warning.

### 2026-06-28 — Bundled R Compiles Packages Only With FLIBS Override

**Context:** Installing the propagate fork with the bundled `r-mac/bin/Rscript` failed at the LINK step (compile succeeded). `r-mac/etc/Makeconf` hardcodes `FLIBS = -L/opt/gfortran/... -lemutls_w -lheapt_w -lgfortran -lquadmath`, but `/opt/gfortran` does not exist on this Mac (gfortran is at `/usr/local/gfortran`). Error: `ld: library 'emutls_w' not found`.
**Lesson:** The bundled R expects the official mac.r-project.org gfortran toolchain at `/opt/gfortran`. For a **Fortran-free** package like propagate (its `src/` is pure C/C++/Rcpp), blank the flag — `R_MAKEVARS_USER=<file with 'FLIBS ='>` — and it links cleanly because the SHLIB recipe uses `-undefined dynamic_lookup` (BLAS/LAPACK symbols resolve against the R framework at load). R auto-restores the prior package on a failed install, so a failed compile does not corrupt the runtime (a manual backup is still cheap insurance).
**Apply When:** Compiling any R package into the bundled runtime. Fortran-free → blank FLIBS. If a package has real Fortran, instead point FLIBS at `/usr/local/gfortran/lib/...` or install the official toolchain at `/opt/gfortran`.

### 2026-07-25 — Pre-Flight Dependency Check Before Large Upstream Syncs

**Context:** The v4.3.0 sync pulled 98 upstream commits and 50 new files (Reliability,
Autocorrelation, SPC CUSUM/EWMA/PPA/Distribution-Fitting). Large new statistical
surface area is exactly where a new CRAN dependency would silently break the build —
the failure would only surface at runtime, in one tab, after shipping.
**Lesson:** Run a two-part pre-flight *before* copying files, not after:
(1) grep the whole upstream `deployment/` for `library|require|requireNamespace` and
`pkg::` usage, then check each name against `r-mac/library/`;
(2) because Steve's own `lolcat` supplies most stats primitives (151 call sites), also
diff the specific `lolcat::` functions used against `getNamespaceExports("lolcat")` in
the *bundled* runtime — a package being present does not mean it is new enough.
Both came back clean here (28 packages present, 34/34 lolcat functions available), which
turned a scary 98-commit sync into a routine one. `grep --include="*.R"` needs the glob
quoted in zsh or it dies with "no matches found".
**Apply When:** Every upstream sync, and especially any sync that adds whole new
top-level modules.

### 2026-07-25 — Upload Release Assets Separately, Never Via `gh release create`

**Context:** `gh release create v4.3.0 ... file.dmg file.zip` ran ~40 minutes on the
~730 MB of artifacts, then the zip upload hit `read: operation timed out`. gh treats a
failed asset upload as a failed release: it **deleted the draft release**, discarding the
362 MB DMG that had already uploaded successfully. All that transfer time was lost.
**Lesson:** For releases this large, split creation from upload:
`gh release create <tag> --notes-file <f>` (no assets) to publish, then
`gh release upload <tag> <asset>` once per file inside a retry loop that first checks
whether the asset is already present. Progress then survives a timeout. Two follow-on
traps seen in the same run: (1) a retry can get `HTTP 422 ReleaseAsset.name already
exists` even with `--clobber`, because gh's own internal retry already landed the file —
so verify presence and size instead of trusting the exit code; (2) piping gh through
`| tail -5` makes the shell report **tail's** exit status, so a hard failure looks like
`EXIT=0`. Always confirm a release by comparing `gh release view --json assets` sizes
against local `stat -f %z` and checking `state=uploaded`.
**Apply When:** Every release. Verify byte-exact asset sizes before marking a release
done in SESSION-STATE.

---
*Append new lessons below this line.*
