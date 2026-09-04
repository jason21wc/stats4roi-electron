# Session State

> Working memory — current position and immediate next actions only.
> Reset when context changes significantly.

## Current Position

| Field | Value |
|-------|-------|
| Phase | v4.3.3 Released / Maintenance |
| Mode | `main` at `c166718` = `origin/main`, tag `v4.3.3` pushed |
| Active Task | None — next upstream sync when Steve ships changes |
| Blocked By | Nothing |

## Context

**v4.3.3 RELEASED (Sep 3, 2026):** https://github.com/jason21wc/stats4roi-electron/releases/tag/v4.3.3
Hand-tested and approved by Jason. `main` fast-forwarded from `sync/v4.3.3` to
`c166718`, tag `v4.3.3` pushed to jason21wc, release created empty, DMG and ZIP uploaded
separately and verified byte-for-byte against local sizes (state `uploaded`). Stale
4.3.1 artifacts deleted from `out/make/`.

**Build detail:** Steve landed the EDA factor-coercion fix
(`e97195d`, "fixing factor coercion in EDA"): a shared `eda_safe_numeric()` coerces
factors through `as.character()` at every measurement-column site in descriptives,
intervals, normality, natural tolerance, boxplots, histograms, quantiles, and pooling.
Grouping-index `as.numeric()` calls are deliberately untouched. Synced one-way from
upstream `deployment/` at upstream HEAD `ee8379a` ("Version increment and deployment",
2026-09-03), 8 commits after the previously inspected `c6bd6d5`. Local `app.R`,
`modules/`, `www/` are byte-identical to that deployment tree (18 files modified, no
adds/deletes, 170 module R files). Upstream displays `v4.33`; Electron semver `4.3.3`
(4.3.1 and 4.3.2 were never released).

Also new since 4.3.1: boxplot hover tooltips (median/quartiles/fences/outliers),
histogram frequency y-axis toggle, optional seed for Transform Data random columns,
random-only columns without an imported file appear in Current Working Data and
download as CSV, and the binomial include/exclude radio labels were reworded.

Independent reproductions against the bundled R 4.5.1 (all pass):
- `factor(c("10","20","bad"))` as measurement → n=2, median 15; `Pass/Fail/Pending`
  → n=0, NA; pooled-all strips non-numeric labels; factor-as-group labels survive.
- Binomial n=10, p=.5: exclude lower 2 → cutoff 1, P=0.0107422; exclude upper 7 →
  cutoff 8, P(X≥8)=0.0546875; P(between)=0.9345703. All match `pbinom`.
- Residual nit, not a blocker (BACKLOG #7): excluding R=0 lower or R=n upper moves the
  cutoff outside the support and the UI renders blank cells, not 0/1.

Upstream tests run in a scratch harness against the synced tree with bundled R:
EDA damage tolerance 43/43 (12 tests incl. 6 new factor-path tests), EDA parity 29/29,
EDA output parity 17/17, distributions parity 28/28, binomial include-R 52/52,
assemble-transformed-data 19/19. The EDA helper needs upstream's root-only
`quantile_type6.R` (not shipped, not referenced by any module) — copied into the
harness only.

Pre-flight: all 32 referenced packages present in the 239-package bundled library;
34/34 `lolcat::` exports present; all 170 modules + app.R parse clean.

**Packaging defect found and fixed (ours, not Steve's):** `ensure-propagate-fork.sh`
built the fork with `r-mac/bin/Rscript`, which hardwires the system framework, so the
shipped fork was compiled under system R 4.5.2 ("built under R version 4.5.2" warning
at every launch, including released v4.3.0). Script now uses `bin/R` with
`R_HOME_DIR`, pins `PKG_LIBS` past the Makevars `Rscript` call, and `--check` fails on
a build-version mismatch. Fork rebuilt under 4.5.1; warning gone. BACKLOG #6 closed.

Build (Node 22.18.0 / ABI 127, `npm run make`, exit 0):
- `stats4roi/out/make/stats4ROI-4.3.3.dmg` — 380,020,251 bytes, SHA-256
  `d406293bd98257fab0b0100b34eeb661d4a5aedd1e1182dbc3f5aa67b543f33a`;
  `hdiutil verify` VALID; read-only mount holds the 4.3.3 app + Applications link.
- `stats4roi/out/make/zip/darwin/arm64/stats4ROI-darwin-arm64-4.3.3.zip` —
  386,282,256 bytes, SHA-256
  `5a8281e6a772875f5552d3ad46492ebca00ad640bae3e04a50a3439de7176fde`; `unzip -tq` clean.
- Packaged `shiny/` mirrors the working tree; Info.plist + packaged package.json 4.3.3;
  launcher and R both Mach-O arm64; packaged propagate is the fork, built under 4.5.1.
- Packaged app launched headless: R listening + HTTP 200 in ~7 s, serves v4.33, `lsof`
  shows libR/BLAS/gfortran/propagate.so all mapped from inside `stats4ROI.app`, zero
  `/Library/Frameworks` mappings, no warnings.

**Hand-test focus for Jason:** EDA with a text column selected as *data* (expect n=0 or
parsed numbers, never 1,2,3); EDA factor mode with a text grouping column (labels
intact); boxplot hover tooltip; histogram y-axis toggle; Transform Data → random column
with and without a seed, with no file imported; Binomial two-tail with "No" on upper
tail (P(X > 7) for n=10, p=.5 should read 0.0547); Scatterplot CI/PI bands (propagate
fork rebuilt).

**HISTORY:** v4.3.1 candidate (Aug 30) was blocked on two upstream defects (reversed
upper-tail exclusion; `as.numeric(factor)` in EDA) and checkpointed on
`checkpoint/v4.3.1-upstream-review` (`c46050a`) — never released. That checkpoint also
carries the launcher fix (native `bin/exec/R` + dyld env), the Node 22 pin, and the
versioned DMG name, all of which v4.3.3 builds on. v4.3.0 released Jul 25, 2026:
https://github.com/jason21wc/stats4roi-electron/releases/tag/v4.3.0

**Search caveat:** the context-engine index covers only docs + Electron JS. It has no R
support — use Grep/Glob for anything under `shiny/`. See LEARNING-LOG 2026-07-25.

## Next Actions

1. Send Steve BACKLOG #7 (R=0 / R=n exclusion renders blank cells; test helper depends on
   root-only `quantile_type6.R`) with thanks for the EDA fix.
2. Optional: BACKLOG #5 (Node 22 pin) before the next build.
3. Branches `sync/v4.3.3` (merged) and `checkpoint/v4.3.1-upstream-review` (superseded)
   can be deleted locally and on origin.

## Open Questions

None blocking. Whether Steve wants the R=0 / R=n exclusion edge to show 0 and 1.

---
*Last Updated: 2026-09-03*
