# Session State

> Working memory — current position and immediate next actions only.
> Reset when context changes significantly.

## Current Position

| Field | Value |
|-------|-------|
| Phase | Maintenance |
| Mode | Standard |
| Active Task | None |
| Blocked By | — |

## Context

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

## Next Actions

No pending actions on the release. v4.3.0 released and distributed.
See `_ai-context/BACKLOG.md` for deferred pipeline improvements (#1 propagate-fork
automation is the one with a history of recurring).

## Open Questions

None.

---
*Last Updated: 2026-07-25*
