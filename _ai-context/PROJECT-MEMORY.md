# Project Memory

> Semantic memory — accumulated decisions, constraints, and project knowledge.
> Append-only; do not delete entries.

## Project Identity

| Field | Value |
|-------|-------|
| Name | stats4roi-electron |
| Purpose | Electron wrapper for stats4ROI R Shiny application |
| Target Platform | Apple Silicon Macs (ARM64) |
| Current Version | 4.3.0 |
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

## Constraints

- **Platform:** Apple Silicon only (M1/M2/M3/M4)
- **macOS:** 11 (Big Sur) or later
- **Code ownership:** Do not modify upstream R code; sync only
- **Push safety:** ONLY push to `jason21wc/stats4roi-electron`. NEVER push to `ProfessorPeregrine/stats4ROI`. Warn user if a request appears to target Steve's repo.
- **Git-ignored:** r-mac/ (~800MB), out/, node_modules/

## Known Gotchas

| Issue | Solution |
|-------|----------|
| Gatekeeper blocks first launch | Right-click → Open, or run `xattr -cr /Applications/stats4ROI.app` |
| "Damaged" warning on download | Same as above — extended attributes issue |
| Slow first launch | Normal; R runtime initializing (~10-15 seconds) |

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

# Test and rebuild
cd stats4roi && npm start
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

# 2. lolcat supplies most stats primitives. Present != new enough.
grep -rhoE "lolcat::[A-Za-z0-9._]+" "$UP" --include="*.R" | sed 's/lolcat:://' | sort -u
# ...diff against getNamespaceExports("lolcat") in the BUNDLED runtime
```

Quote the `--include="*.R"` glob or zsh fails with "no matches found".

## Release Procedure

1. `npm run make`, then rename `out/make/stats4ROI.dmg` →
   `stats4ROI-<version>.dmg` (the maker emits an unversioned name; releases use
   versioned asset names).
2. Smoke-test headless, verify `hdiutil verify` on the DMG, and confirm the packaged
   `.app` bundles the propagate fork.
3. **Wait for Jason's hand-test before pushing or releasing.** See
   `_ai-context/SESSION-STATE.md` for the tabs to exercise.
4. Push commit + tag to `origin` (jason21wc ONLY).
5. **Create the release empty, then upload assets separately:**

```bash
gh release create v<version> --repo jason21wc/stats4roi-electron \
  --title "stats4ROI v<version>" --notes-file <notes>
gh release upload v<version> <asset> --repo jason21wc/stats4roi-electron   # one per asset
```

   Do NOT pass assets to `gh release create`. On a failed upload it **deletes the
   whole release**, discarding assets that already transferred — ~730 MB over a slow
   link means that costs the better part of an hour. See LEARNING-LOG 2026-07-25.
6. Verify: compare `gh release view --json assets` sizes against local
   `stat -f %z` and confirm `state=uploaded`. Exit codes are unreliable here —
   piping gh through `tail` reports tail's status, and gh's own internal retry can
   surface `HTTP 422 ReleaseAsset.name already exists` for a file that landed fine.

## R Runtime Assembly — propagate Fork (CRITICAL)

The bundled `r-mac/` runtime MUST contain Steve's Shiny fork of `propagate`
([ProfessorPeregrine/propagate](https://github.com/ProfessorPeregrine/propagate)),
**not** CRAN propagate. CRAN's `predictNLS` prints to stdout inside Shiny reactive
contexts and corrupts scatterplot confidence/prediction-interval rendering.
`app.R` warns at startup if the fork is missing. The fork is CRAN 1.0-6 + a one-line
patch (older base than CRAN 1.0-7, but it is the config Steve tests against).

**This is not automatic** — any fresh `r-mac/` install pulls CRAN propagate from
the default repo and silently regresses. After assembling/refreshing `r-mac/`, run:

```bash
cd stats4roi
ROOT=$(pwd); LIB="$ROOT/r-mac/library"
# Bundled R hardcodes FLIBS=/opt/gfortran (absent here). propagate is Fortran-free,
# so blank FLIBS or the link fails: "ld: library 'emutls_w' not found".
printf 'FLIBS =\n' > /tmp/Makevars-nofortran
R_MAKEVARS_USER=/tmp/Makevars-nofortran R_LIBS="$LIB" R_LIBS_USER="$LIB" R_LIBS_SITE="$LIB" \
  "$ROOT/r-mac/bin/Rscript" --vanilla -e \
  '.libPaths(file.path(getwd(),"r-mac","library")); remotes::install_github("ProfessorPeregrine/propagate", upgrade="never", force=TRUE, lib=.libPaths()[1], build=TRUE)'
```

**Verify:** `packageDescription("propagate")$RemoteUsername` must equal
`"ProfessorPeregrine"`, and the `npm start` log must NOT contain
"does not appear to be the Shiny fork". (Requires Xcode CLT clang; R auto-restores
the prior package if the compile fails, so the runtime is never left corrupted.)

---
*Last Updated: 2026-07-25*
