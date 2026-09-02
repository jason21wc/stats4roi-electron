# Session State

> Working memory — current position and immediate next actions only.
> Reset when context changes significantly.

## Current Position

| Field | Value |
|-------|-------|
| Phase | Upstream Sync / Release Blocked |
| Mode | Checkpoint — resume from `checkpoint/v4.3.1-upstream-review` |
| Active Task | Await Steve's response and upstream correction, then resync v4.3.1 |
| Blocked By | Factor level codes still enter numeric EDA calculations upstream; lower-boundary lookup also remains unresolved |

## Context

**SESSION CLOSE-OUT (Sep 1, 2026):** the in-progress candidate and all handoff material
are checkpointed on branch `checkpoint/v4.3.1-upstream-review`; `main` remains at
`40a9a08` / `origin/main`. The checkpoint is deliberately not a release candidate and
must not be merged to `main`, tagged, or released in its current state. A copy-ready
email was prepared for Jason to send to Steve, focused only on the unresolved
factor-as-group versus factor-as-numeric-measurement distinction. Codex did not send
the email. Upstream `c6bd6d5` has not been copied into this repository or rebuilt.

Resume sequence:
1. Check Steve's reply and fetch current upstream `main`.
2. Confirm an EDA change exists and independently reproduce factor-as-group and
   factor-as-measurement behavior; confirm the lower-zero boundary if Steve addresses it.
3. Only when blockers are resolved, one-way resync `deployment/`, rerun upstream and
   semantic tests, rebuild/validate DMG + ZIP, then request Jason's hand-test.
4. After Jason approves, finish the real release on `main`; do not treat the checkpoint
   branch itself as the integration decision.

**UPSTREAM RECHECK (Sep 1, 2026):** latest upstream `main` is
`c6bd6d5ca97408c4ccd21dcc8271f045eb801428`, two commits after the previously synced
`812eef60`. The discrete upper-tail defect is fixed: an excluded upper endpoint now
moves from `X` to `X+1`, Poisson and hypergeometric use the shared helper, and updated
tests assert the mathematical tail-membership contract. Independent bundled-R
reproduction for binomial `n=10`, `p=.5`, excluded upper endpoint 7 now adjusts to 8
and returns the correct `0.0546875`.

The upstream fix is incomplete. There are no changes to `deployment/stats4ROI_mod.R`
or `deployment/modules/statistical/eda/` since `812eef60`. The production import path
still converts characters to factors, and `pool_data_frame_columns()` plus the other
EDA paths still coerce factors directly with `as.numeric()`. The original factor input
`10, 20, bad` still reproduces as `1, 2, 3`. Excluding the lower support boundary at
zero also still adjusts to `-1` and returns three zero-length probability fields.

The corrected upstream tree has therefore been fetched and inspected in a temporary
clone but has **not** been copied into this packaging repository or rebuilt. Preserve
the current blocked candidate until Steve lands the remaining correction; partial
resync/rebuild would create another known-bad artifact without advancing the release.

Steve's response correctly notes that grouping/category columns must remain factors so
their labels survive. That does not resolve the defect: the EDA UI separately allows a
column to be selected as measurement/dependent data, and those paths copy
`as.numeric(factor)` results into statistical calculations. Against upstream `c6bd6d5`,
the app-equivalent descriptives path reports `n=3, mean=2, sd=1` for both
`factor(c("10", "20", "bad"))` and `factor(c("Pass", "Fail", "Pending"))`. The existing
damage-tolerance test says a text column should instead have `n=0` and `mean=NA`, but
the test uses characters and bypasses the production import conversion. The required
fix is role-sensitive: retain factors for grouping; parse their labels or reject them
when used as numeric measurements.

**v4.3.1 CANDIDATE — NOT RELEASABLE (Aug 30, 2026):** Electron packaging, build,
and runtime validation are complete, but independent review found two upstream
statistical-correctness defects. Both were reproduced directly against the synced
source and bundled R runtime. Do not hand-test as a release candidate, commit, tag,
push, or release until upstream corrects them and the corrected source is resynced.

Release blockers:
- **Discrete upper-tail exclusion is reversed.**
  `modules/distributions/discrete_x_of_interest.R` subtracts one from an excluded
  upper endpoint. For a binomial `n=10, p=.5`, entering upper endpoint 7 with `>`
  returns `P(X >= 6) = 0.3769531`; the UI contract requires `P(X > 7) = P(X >= 8) =
  0.0546875`. Upstream commit `1dfd651` changed the initially correct `X+1` adjustment
  to `X-1` to match Poisson's existing behavior. The regression test asserts that
  implementation convention rather than the mathematical/UI contract. Excluding the
  lower support boundary at zero also produces a zero-length lookup.
- **EDA silently converts factor levels to invented numbers.** `app.R` converts
  imported character columns to factors, while the new damage-tolerance paths call
  `as.numeric(factor)`. Values `c("10", "20", "bad")` therefore become `c(1, 2, 3)`
  instead of `c(10, 20, NA)`. The pattern affects descriptives, intervals, boxplots,
  histograms, natural tolerance, normality, pooling, and quantiles. Upstream tests use
  character columns and bypass the app's actual factor-conversion pipeline.

The correct ownership path is an upstream fix by Steve followed by a clean one-way
resync. A local R patch would violate this repository's packaging-only boundary and
create a downstream statistical-code fork.

Pinned and synced from upstream `deployment/` at upstream HEAD `812eef60`
("deployment"), dated 2026-08-28: 11 commits after the v4.3.0 sync point
(`5237793c`). Local `app.R`, `modules/`, and `www/` are byte-identical to that
upstream deployment. The sync modifies 20 existing R files and adds
`modules/distributions/discrete_x_of_interest.R`; the tree now contains 170 module R
files / 186 module + static files. Upstream displays `v4.31`; the Electron candidate
uses semver `4.3.1`.

Changes concentrate on binomial/hypergeometric include-exclude behavior, missing/invalid
data handling in EDA and scatterplots, and duplicate Shiny input/output IDs in ANOVA,
one/two-sample tests, and SPC PPA.

Packaging verification completed:
- No missing R dependencies in the 239-package bundled library; all 34/34 referenced
  `lolcat::` exports are available.
- `propagate` is version 1.0-6 / `RemoteUsername: ProfessorPeregrine` in both the source
  runtime and packaged `.app`.
- All 170 R modules source cleanly. Upstream regression tests pass: binomial 37
  assertions, duplicate-ID 30 assertions, and all 6 EDA damage-tolerance cases (the EDA
  file must be sourced from repository root because its helper uses relative paths).
- Node 22.18.0 / ABI 127 packages the ARM64 `.app`. Default Homebrew Node 25.2.0 / ABI
  141 cannot load the existing `macos-alias` native module.
- Packaged source mirrors the working tree; Electron launcher and bundled R payload are
  both Mach-O ARM64; Info.plist and packaged package.json report 4.3.1.
- Final ZIP complete and verified:
  `stats4roi/out/make/zip/darwin/arm64/stats4ROI-darwin-arm64-4.3.1.zip`
  (386,279,206 bytes; SHA-256
  `870c4b182511e973537422e9a36a9421a36364359d15cbbf87206f2009563c55`).
- Final DMG complete and verified:
  `stats4roi/out/make/stats4ROI-4.3.1.dmg` (379,987,900 bytes; SHA-256
  `5874c3ac8d26e948d1384f3b3c6ce527cf61d63d823c3de74b564b2675ce1308`).
  `hdiutil verify` reports a valid checksum; a read-only mount contains the ARM64
  v4.3.1 app, ARM64 R executable, Applications link, and correct propagate fork.
- The full Electron shell launches, spawns bundled R 4.5.1, listens on localhost, and
  returns HTTP 200. `lsof` confirms `libR`, BLAS, and Fortran libraries are mapped from
  inside `stats4ROI.app`, even though system R 4.5.2 is installed.

The execution blocker is resolved. Clean `/quit` released the thread's exclusive writer,
and the same conversation resumed from Warp with `--sandbox danger-full-access`.
DiskManagement and localhost probes then passed, proving the earlier failures were the
old process sandbox rather than Warp, Electron, or macOS configuration.

Two build-root causes were corrected:
- Invoking Node 22's `npm` by absolute path still let its `env node` shebang and child
  processes resolve Node 25 from `PATH`. Prefixing Node 22's complete `bin` directory
  pins Forge and its native-module ABI to 127.
- R's executable has absolute framework install names. On the developer Mac it silently
  loaded system R 4.5.2 instead of bundled R 4.5.1. macOS strips parent `DYLD_*`
  variables before running a shell script. The tracked Electron launcher now bypasses
  that boundary by invoking native `bin/exec/R` directly with complete `R_*` and dyld
  environment variables. `get-r-mac.sh` also patches `bin/R` internally for standalone
  runtime commands and future assembly.

No commit, tag, push, or release has occurred.

**v4.3.0 RELEASED (Jul 25, 2026):** https://github.com/jason21wc/stats4roi-electron/releases/tag/v4.3.0
Hand-tested by user — confirmed working. DMG + zip attached to the release.

Synced from upstream `deployment/` at upstream HEAD
`5237793c` ("distributions"), dated 2026-07-20. **98 upstream commits** since the v4.2.1
sync point (`4e2fcf64`). Local `shiny/` (app.R + 184 module/www files) is byte-identical
to upstream deployment.

Upstream UI version string bumped v4.2 → **v4.3**; package.json bumped to 4.3.0.

New since v4.2.1 (Jun 28):
- **Reliability module (NEW top-level navbar menu)** — `modules/reliability/`:
  Reliability Calculator (series-of-parallels), Growth Analysis (Crow-AMSAA),
  Weibull life-data analysis. Utils: `growth_amsaa`, `growth_tables`,
  `reliability_calc`, `weibull_bounds`, `weibull_life`.
- **Autocorrelation module (NEW)** — `modules/statistical/autocorrelation/`, ACF/PACF
  analysis + plots.
- **SPC major expansion** — CUSUM charts, EWMA charts (smoothing weight renamed
  λ→α), Process Performance Analysis (PPA) with stream/nested-factor breakdowns,
  and Distribution Fitting (`dfit_*`: capability, conformance, GOF, tail-focused
  fitting, graphics). Plus attribute-chart limit summaries, axis labeling,
  sigma-from-limits, zone classification override.
- **Runs tests** — `one_two_sample_tests/utils/runs_test.R`; fixes to sign test
  and Wilcoxon from data, and empty nonparametric results.
- 50 new files, 0 deletions; 13 existing files modified (ANOVA, DOE, SPC, OTS).

Verification done:
- **No new R package deps** — all 28 packages used by upstream present in bundled
  `r-mac/library` (239 packages installed).
- **All 34 `lolcat::` functions** used upstream verified against the bundled
  lolcat 2.0.0 exports — no gaps.
- **propagate fork intact** — `r-mac/library/propagate` is 1.0-6 /
  RemoteUsername `ProfessorPeregrine`; confirmed also inside the packaged `.app`.
- Dev smoke test green: Shiny boots, all modules source clean,
  `Listening on 127.0.0.1`, NO fork warning, no errors.
- Packaged-bundle smoke test run against the app's own bundled R runtime.
- DMG integrity verified (`hdiutil verify` → checksum VALID).

Local build artifacts at `stats4roi/out/make/` (git-ignored):
`stats4ROI-4.3.0.dmg` (362 MB) + `zip/darwin/arm64/stats4ROI-darwin-arm64-4.3.0.zip`
(368 MB).

Shipped commit `e4d7b7d` (source sync + docs) + tag `v4.3.0` pushed to `origin` (jason21wc).

**AI-context layout migrated (Jul 25, 2026):** memory files moved from the repo root
into `_ai-context/` (unified layout, v2.62.0) and `BACKLOG.md` added. `CLAUDE.md` and
`ARCHITECTURE.md` stay at the root as loaders/technical docs. Root `.gitignore` now
also excludes `node_modules/`, `r-mac/`, and `out/` — the context-engine indexer reads
only the top-level ignore file, and without them it indexed ~8.8k vendored files
instead of the ~215 real source files.

**Search caveat:** the context-engine index covers only docs + Electron JS (26 files).
It has **no R support** — all 170 R module files are absent. Use Grep/Glob for anything
under `shiny/`; an empty `query_project` result here means "not indexed," NOT "does not
exist." See LEARNING-LOG 2026-07-25.

**propagate-fork automation shipped (Jul 25, 2026)** — BACKLOG #1 closed. New
`stats4roi/ensure-propagate-fork.sh` (idempotent, self-verifying, restores its backup
on a bad result). The root cause was not a missing step but a presence check:
`install_stats4roi_packages.R` gated the fork install on `!require("propagate")`, so a
transitively-installed CRAN propagate satisfied the guard and the fork was skipped
while the script reported success. Now gated on `RemoteUsername`, hard-fails instead of
skipping. Tested end-to-end against a simulated CRAN state; runtime re-verified and
Shiny boots clean.

## Next Actions

1. Jason sends the prepared email explaining the factor-role distinction and waits for
   Steve's response/upstream correction.
2. After upstream lands the remaining fixes, resync `deployment/`, rerun the semantic
   reproductions and automated/package validation, then rebuild the DMG and ZIP.
3. Jason hand-tests only the corrected candidate. After approval: commit, tag `v4.3.1`,
   push to `jason21wc`, create the GitHub release empty, and upload the assets separately.

## Open Questions

Whether Steve will distinguish factor-as-group from factor-as-measurement and correct
the latter. A fresh fetch confirms upstream `main` remains `c6bd6d5`; his "new version"
is the already-inspected distribution update and contains no EDA change.

---
*Last Updated: 2026-09-01*
