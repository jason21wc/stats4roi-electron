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

### 2026-07-25 — The Semantic Index Does Not Cover R; Use Grep Here

**Context:** Indexed this repo with context-engine expecting it to help answer "where
does X happen" across ~79,300 lines of R. Two checks killed that assumption: a
conceptual query ("Weibull life data analysis confidence bounds") and an exact-identifier
query (`create_growth_analysis_server`) each returned **zero** `.R` files — only
markdown, `package.json`, and Electron JS. The index holds 26 files / 132 chunks; the
169 R module files are absent. The indexer has no R support.
**Lesson:** For this repo, `query_project` is useful only for the docs and the Electron
shell. **Grep/Glob remain the primary search tools for anything in `shiny/`.** The
dangerous failure mode is a false negative: an empty semantic result here means "not
indexed," NOT "does not exist" — never conclude R code is missing on that basis.
Separately, the first index run swallowed 8,849 files (~97% vendored). Cause: the root
`.gitignore` did not list `node_modules/`, `r-mac/`, or `out/` — those live in the
nested `stats4roi/.gitignore`, and the indexer reads only the top-level ignore file.
Root `.gitignore` now lists them.
**Apply When:** Searching this codebase, or indexing any project whose real source is a
language the indexer may not parse — verify with one exact-identifier query before
trusting the index.

### 2026-07-25 — The propagate Regression Was a Presence Check, Not a Missing Step

**Context:** Automating the fork install (BACKLOG #1) turned up the actual mechanism of
the v4.2.0 regression, which the 2026-06-28 entry had characterized as "`r-mac` is
assembled without a fork-install step." There *was* a step —
`install_stats4roi_packages.R` called `install_github("ProfessorPeregrine/propagate")`.
It was guarded by `if (!require("propagate"))`. CRAN propagate arrives as a transitive
dependency of other packages, so `require()` succeeded, the guard skipped the fork
install, and the script printed "All packages installed successfully!"
**Lesson:** **Gate on WHICH artifact is installed, never on whether one is.** A presence
check cannot distinguish a correct dependency from a wrong one that occupies the same
name, and it fails *silently and positively* — the worst combination. The same shape
applies to the `lolcat` guard beside it, and to any "install if absent" logic where a
different build could satisfy the name. Verify the identifying attribute
(`RemoteUsername`, a version, a checksum) and hard-fail rather than skip.
Secondary lesson: read `DESCRIPTION` directly instead of starting R to check it — the
check must still work when the installed package is broken.
**Apply When:** Writing or reviewing any conditional install. If the condition is
"is it there," it is probably wrong.

---
*Append new lessons below this line.*
