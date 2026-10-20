# Project Memory

> Semantic memory — accumulated decisions, constraints, and project knowledge.
> Append-only; do not delete entries.

## Project Identity

| Field | Value |
|-------|-------|
| Name | stats4roi-electron |
| Purpose | Electron wrapper for stats4ROI R Shiny application |
| Target Platform | Apple Silicon Macs (ARM64) |
| Current Version | 4.3.6 |
| Status | Production |

## Specification Summary

Wrap Steven Ouellette's stats4ROI R Shiny application in Electron for standalone distribution on Apple Silicon Macs. The R runtime is bundled; users need no R installation.

## Architecture Decisions

| Decision | Rationale | Date |
|----------|-----------|------|
| Modular v4.0 architecture | Upstream switched from monolithic to modular; aligns with Steve's direction | 2024-01-24 |
| Bundle R 4.5.1 runtime | Self-contained app; no user R installation required | 2024-01-24 |
| Apple Silicon only | Simplifies build; Intel builds possible but not prioritized | 2024-01-24 |
| One-way sync from upstream | We package, Steve develops; no code modifications | 2024-01-24 |
| Ad-hoc sign the packaged .app inside-out in a Forge `postPackage` hook (`scripts/adhoc-sign.js`) | The unsealed bundle made quarantined downloads report "damaged — Move to Trash", with no Open Anyway. This was the likely cause of the Aug 2026 Mac student reports. A valid ad-hoc seal is free and gives the normal unidentified-developer prompt. It is still unnotarized (BACKLOG #2). The hook fails the build unless the seal verifies and `exec/R` keeps CRAN's signature plus `allow-dyld-environment-variables` | 2026-10-19 |
| Pin lolcat by commit SHA (`LOLCAT_SHA` in `install_stats4roi_packages.R`) | lolcat's 2.0.2 version bump (2026-10-03) predates the exact Pearson r functions v4.36 calls, so presence and version both pass on a build that fails in Sample Size/Power. Gate on `RemoteSha`, hard-fail after install. Pinned `burrm/lolcat@f0b8af6`, Mike's merge of Steve's fork; zero file diff from Steve's fork head `92a9910` | 2026-10-19 |

## Technical Stack

| Component | Technology |
|-----------|------------|
| Application Framework | Electron (via Electron Forge) |
| Statistical Engine | R 4.5.1 (bundled) |
| UI Framework | R Shiny (modular) |
| Package Format | DMG |
| Build Tool | npm / Electron Forge |

## Upstream Attribution

| Field | Value |
|-------|-------|
| Repository | [professorperegrine/stats4ROI](https://github.com/ProfessorPeregrine/stats4ROI) |
| Author | Steven Ouellette |
| Email | steve@roi-ally.com |
| Website | https://www.roi-ally.com |
| Sync Source | `deployment/` directory |

**All R/Shiny code is Steve's work.** This repository provides Electron packaging only.

**Distribution path:** Jason publishes the Mac build, then emails Steve, who re-hosts it on
roi-ally.com for his students. In July 2026 that went through a shared Dropbox link; since
then it has been a GitHub release on `jason21wc/stats4roi-electron`. Students download
through a browser, so every copy they get is quarantined. Gatekeeper behavior on a
quarantined copy is what they experience, and a local build never shows it.

## Constraints

- **Platform:** Apple Silicon only (M1/M2/M3/M4)
- **macOS:** 11 (Big Sur) or later
- **Code ownership:** Do not modify upstream R code; sync only
- **Push safety:** ONLY push to `jason21wc/stats4roi-electron`. NEVER push to `ProfessorPeregrine/stats4ROI`. Warn user if a request appears to target Steve's repo.
- **Git-ignored:** r-mac/ (~800MB), out/, node_modules/

## Known Gotchas

| Issue | Solution |
|-------|----------|
| Gatekeeper blocks first launch | Right-click → Open, or run `xattr -cr /Applications/stats4ROI.app` *(pre-4.3.6 advice; superseded by the rows below)* |
| "Damaged" warning on download | Same as above — extended attributes issue *(root cause was the missing bundle seal; see the 2026-10-19 rows)* |
| Slow first launch | Normal; R runtime initializing (~10-15 seconds) |
| "Damaged" on a 4.3.5-or-older download | Those builds had no bundle seal (see the 2026-10-19 decision). From 4.3.6 on, users get the unidentified-developer prompt instead: Open Anyway in Privacy & Security on macOS 15+, right-click → Open on 14 and earlier. A locally built app has no quarantine flag, so the maintainer never sees either prompt |
| Intel Mac: "not supported on this type of Mac" | The build is arm64-only by decision. Not a bug |
| `r-mac/bin/R -e '...'` reports `'\.' is an unrecognized escape` | The `bin/R` wrapper strips one level of backslashes from `-e` arguments. Put ad-hoc checks in a script file and run `bin/R -f <file>`, or write `[.]` instead of `\\.` |

## Phase Gates

| Phase | Status | Date |
|-------|--------|------|
| Initial Setup | Complete | 2024-01-24 |
| Architecture Migration (v3.2→v4.0) | Complete | 2024-01-24 |
| Build & Test | Complete | 2024-01-24 |
| GitHub Release | Complete | 2024-01-24 |
| Documentation | Complete | 2025-01-26 |
| Upstream Sync v4.2.0 (DOE module) | Complete | 2026-04-25 |
| Upstream Sync v4.2.1 (Taguchi optimization) | Complete | 2026-06-28 |
| Upstream Sync v4.3.0 (Reliability + SPC expansion) | Complete | 2026-07-25 |
| Upstream Sync v4.3.1 (distribution/EDA reliability fixes) | Superseded — blocked on upstream defects, never released | 2026-08-30 |
| Upstream Sync v4.3.3 (EDA factor fix, boxplot hover, seeded random columns) | Complete — released | 2026-09-03 |
| propagate fork rebuilt under bundled R; fork script uses bin/R + build-version check | Complete | 2026-09-03 |
| Build Node pinned and enforced (.nvmrc + engine-strict + premake gate) | Complete | 2026-09-03 |
| Upstream Sync v4.3.6 (v4.36: deferred module loading, exact Pearson r power, Wilcoxon CIs; lolcat 2.0.0 → 2.0.2 @ f0b8af6) | Built and verified — awaiting hand-test | 2026-10-19 |
| Ad-hoc inside-out signing step in Forge `postPackage` (`scripts/adhoc-sign.js`), with `npm run check-signature` | Built and verified — awaiting hand-test with v4.3.6 | 2026-10-19 |
| AI-context migrated to `_ai-context/` unified layout | Complete | 2026-07-25 |
| propagate fork install automated + presence-check bug fixed | Complete | 2026-07-25 |

## Source Documents Registry

| Document | Purpose | Location |
|----------|---------|----------|
| CLAUDE.md | AI loader; points into `_ai-context/` | `/CLAUDE.md` |
| README.md | User-facing installation and usage | `/README.md` |
| ARCHITECTURE.md | Technical component design | `/ARCHITECTURE.md` |
| LICENSE.md | MIT License | `/LICENSE.md` |
| package.json | Electron/npm configuration | `/stats4roi/package.json` |
| SESSION-STATE.md | Working memory | `/_ai-context/SESSION-STATE.md` |
| PROJECT-MEMORY.md | This file — semantic memory | `/_ai-context/PROJECT-MEMORY.md` |
| LEARNING-LOG.md | Episodic memory | `/_ai-context/LEARNING-LOG.md` |
| BACKLOG.md | Prospective memory (deferred work) | `/_ai-context/BACKLOG.md` |

## Sync Procedure (for future updates)

```bash
# Clone upstream temporarily
git clone --depth 1 https://github.com/ProfessorPeregrine/stats4ROI.git /tmp/upstream

# Copy deployment files
cp /tmp/upstream/deployment/stats4ROI_mod.R stats4roi/shiny/app.R
cp -r /tmp/upstream/deployment/modules/* stats4roi/shiny/modules/
cp -r /tmp/upstream/deployment/www/* stats4roi/shiny/www/

# Clean up
rm -rf /tmp/upstream

# Test and rebuild (Node 22 first on PATH — see Release Procedure step 1;
# `npm run make` blocks itself otherwise and prints the fix)
cd stats4roi && nvm use
npm start
npm run make
```

## Sync Pre-Flight (run BEFORE copying upstream files)

Large syncs are cheap to de-risk and expensive to debug after the fact. Both checks
must pass or the new modules will fail at runtime in a single tab, after shipping.

```bash
# 1. Any R package upstream needs that the bundled runtime lacks?
grep -rhoE "(library|require|requireNamespace)\(['\"]?[A-Za-z0-9._]+" "$UP" --include="*.R" \
  | sed -E "s/^(library|require|requireNamespace)\(['\"]?//" | sort -u
grep -rhoE "\b[A-Za-z][A-Za-z0-9._]*::" "$UP" --include="*.R" | sed 's/:://' | sort -u
# ...then confirm each name exists in stats4roi/r-mac/library/

# 2. lolcat supplies most stats primitives. Present != new enough, and version !=
#    new enough. Most calls are UNQUALIFIED (library(lolcat) is attached), so the
#    lolcat:: grep below misses them.
grep -rhoE "lolcat::[A-Za-z0-9._]+" "$UP" --include="*.R" | sed 's/lolcat:://' | sort -u
# ...diff against getNamespaceExports("lolcat") in the BUNDLED runtime, then:
#    install the candidate lolcat into a scratch lib with bundled `bin/R CMD INSTALL -l`,
#    diff getNamespaceExports() old vs new, and grep "$UP" for each ADDED name.
#    Never parse NAMESPACE text: lolcat uses exportPattern(), so a text parse is empty.
# 3. Which lolcat does Steve build on? Check his email and
#    `gh api repos/burrm/lolcat/compare/<pinned>...ProfessorPeregrine:lolcat:master`;
#    bump LOLCAT_SHA in install_stats4roi_packages.R if his tests need newer commits.
```

Quote the `--include="*.R"` glob or zsh fails with "no matches found".

**Deferred module loading (upstream v4.36+).** `app.R` loads only core packages and the
data pipeline. Feature modules and their packages (tidyverse, propagate, emmeans, nlme,
car, bayestestR, bayesboot, agop, ggh4x) are sourced on first tab visit or by idle
prefetch through `modules/config/deferred_module_loader.R`. Consequences for a sync:
a missing package or function now fails in one tab at first use, not at launch, so a
launch smoke test proves little on its own. The sync gate is (a) source every module
eagerly by setting `options(stats4roi.run_deferred_manifest = TRUE)` before the
`app.R` prefix (upstream's own switch), and (b) upstream `tools/smoke_deferred_load.R`,
which activates all 32 tabs under `testServer`. The propagate-fork warning moved from
`app.R` to `scatterplot_server.R`, so it appears only once that module loads.

**Where to run upstream tests:** first confirm `diff -rq <upstream>/modules
<upstream>/deployment/modules` reports only `Only in <root>` lines (no `differ`). Then
run testthat from upstream's root with bundled R. A harness built from `deployment/`
lacks fixtures and errors on roughly 20 files. To decide whether a failure is ours,
rerun it with the previous lolcat first on `R_LIBS`; identical results mean it predates
the sync. See LEARNING-LOG 2026-10-19.

## Release Procedure

1. Put the build Node's complete `bin` directory first in `PATH` (pinned to 22.18.0 in
   `stats4roi/.nvmrc`), then run `npm run make`. Calling the Node 22 `npm` executable by
   absolute path is insufficient: its `env node` shebang and child scripts still resolve
   `node` from `PATH`. Since 2026-09-03 this is enforced rather than remembered —
   `premake`/`prepackage` run `scripts/check-build-node.js`, which blocks the build and
   prints the exact `PATH` export; `.npmrc` `engine-strict` blocks `npm install` under
   the wrong Node. Forge emits the versioned DMG directly:
   `out/make/stats4ROI-<version>.dmg`.
2. Smoke-test headless, verify `hdiutil verify` on the DMG, and confirm the packaged
   `.app` bundles the propagate fork. Run `npm run check-signature`; it must pass. The
   `postPackage` hook already enforces it, so a failure here means `out/` was modified
   after the build. `spctl -a` will still say "rejected": that is expected for an
   unnotarized app and is not a failure.
3. For upstream statistical changes, independently verify at least one expected result
   against the UI/math contract and exercise data-damage paths with the types produced
   by the real import pipeline. Passing upstream tests is not sufficient when those
   tests duplicate an existing implementation or bypass factor conversion.
4. **Wait for Jason's hand-test before pushing or releasing.** See
   `_ai-context/SESSION-STATE.md` for the tabs to exercise.
5. Push commit + tag to `origin` (jason21wc ONLY).
6. **Create the release empty, then upload assets separately:**

```bash
gh release create v<version> --repo jason21wc/stats4roi-electron \
  --title "stats4ROI v<version>" --notes-file <notes>
gh release upload v<version> <asset> --repo jason21wc/stats4roi-electron   # one per asset
```

   Do NOT pass assets to `gh release create`. On a failed upload it **deletes the
   whole release**, discarding assets that already transferred — ~730 MB over a slow
   link means that costs the better part of an hour. See LEARNING-LOG 2026-07-25.
7. Verify: compare `gh release view --json assets` sizes against local
   `stat -f %z` and confirm `state=uploaded`. Exit codes are unreliable here —
   piping gh through `tail` reports tail's status, and gh's own internal retry can
   surface `HTTP 422 ReleaseAsset.name already exists` for a file that landed fine.

**macOS build-locus check:** the DMG maker depends on DiskArbitration, not just file
write access. Treat DiskArbitration and localhost binding as process capabilities: after
changing permissions, run direct `diskutil` and socket-bind probes. If they still fail,
start a fresh process with the required sandbox at launch; terminal brand and filesystem
labels are not capability evidence. Changing Electron or DMG format does not address the
host boundary. A failed concurrent maker can leave partial ZIP files, so accept only the
final versioned filename after `unzip -tq` passes.

## R Runtime Assembly — propagate Fork (CRITICAL)

The bundled `r-mac/` runtime MUST contain Steve's Shiny fork of `propagate`
([ProfessorPeregrine/propagate](https://github.com/ProfessorPeregrine/propagate)),
**not** CRAN propagate. CRAN's `predictNLS` prints to stdout inside Shiny reactive
contexts and corrupts scatterplot confidence/prediction-interval rendering.
`app.R` warns at startup if the fork is missing. The fork is CRAN 1.0-6 + a one-line
patch (older base than CRAN 1.0-7, but it is the config Steve tests against).

Any fresh `r-mac/` install pulls CRAN propagate from the default repo and silently
regresses. **After assembling or refreshing `r-mac/`, run:**

```bash
cd stats4roi
./ensure-propagate-fork.sh          # installs the fork only if it is missing
./ensure-propagate-fork.sh --check  # verify only, never installs (exit 1 = wrong)
./ensure-propagate-fork.sh --force  # reinstall even if already correct
```

The script encapsulates the whole procedure: it blanks `FLIBS` (the bundled R
hardcodes `FLIBS=/opt/gfortran`, usually absent, so the link fails with
`ld: library 'emutls_w' not found`; propagate is Fortran-free so blanking is safe),
pins `PKG_LIBS` so the fork's Makevars does not shell out to `Rscript` for the
long-empty `Rcpp:::LdFlags()`, installs with `force=TRUE` into `r-mac/library` using
`r-mac/bin/R` with `R_HOME_DIR` exported (**never `r-mac/bin/Rscript`, which hardwires
the system framework and silently builds under system R** — see LEARNING-LOG
2026-09-03), then **verifies the artifact on disk and restores its backup if the result
is wrong.** It reads `RemoteUsername` and the `Built:` R version straight from
`DESCRIPTION` rather than starting R, so it still reports correctly when the installed
package is broken, and `--check` fails if the fork was built under a different R than
the bundled runtime. Requires Xcode CLT clang.

**Gate on WHICH propagate is installed, never on whether one is.** CRAN propagate
often arrives as a transitive dependency, so a `!require("propagate")` guard sees a
package, skips the fork install, and reports success — that is precisely how v4.2.0
regressed. `install_stats4roi_packages.R` now checks `RemoteUsername` and hard-fails
rather than skipping.

**Verify:** `./ensure-propagate-fork.sh --check` exits 0, and the `npm start` log
must NOT contain "does not appear to be the Shiny fork" or "was built under R version".
Since v4.36 the fork warning lives in `scatterplot_server.R` and fires only once that
module is sourced (first Correlation tab visit, or idle prefetch after a browser
session connects), so a bare HTTP probe never triggers it — rely on `--check`.

## R Runtime Dynamic-Library Containment (CRITICAL)

The macOS R executable retains absolute install names pointing at
`/Library/Frameworks/R.framework`. If the build Mac has a system R installed, dyld can
silently load that framework instead of `r-mac/lib`; a smoke test can therefore pass
against the wrong runtime. Parent-provided `DYLD_LIBRARY_PATH` is not sufficient because
macOS strips `DYLD_*` variables before executing a shell script. The Electron launcher
therefore invokes native `r-mac/bin/exec/R` directly and supplies `R_HOME`, the R content
directories, and `DYLD_LIBRARY_PATH`. `get-r-mac.sh` separately patches the bundled
`bin/R` wrapper to set the loader path inside the script for standalone runtime commands.
Verify a packaged launch with `lsof -p <R-pid>`: `libR.dylib`, BLAS, and gfortran must
map from inside `stats4ROI.app`, never `/Library/Frameworks/R.framework`.

---
*Last Updated: 2026-10-19*
