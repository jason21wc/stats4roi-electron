# Session State

> Working memory — current position and immediate next actions only.
> Reset when context changes significantly.

## Current Position

| Field | Value |
|-------|-------|
| Phase | Maintenance |
| Mode | Standard |
| Active Task | v4.3.0 built — awaiting user hand-test before release |
| Blocked By | — |

## Context

**v4.3.0 BUILT (Jul 25, 2026)** — synced from upstream `deployment/` at upstream HEAD
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

## Next Actions

1. **User hand-test** the built app (`stats4roi/out/make/stats4ROI-4.3.0.dmg`) —
   especially the new Reliability and Autocorrelation tabs and the SPC CUSUM/EWMA/PPA/
   Distribution Fitting tabs.
2. On approval: commit the sync, tag `v4.3.0`, push to `origin` (jason21wc ONLY),
   and create the GitHub release with both artifacts attached.

## Open Questions

None.

---
*Last Updated: 2026-07-25*
