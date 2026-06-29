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

**v4.2.1 RELEASED (Jun 28, 2026):** https://github.com/jason21wc/stats4roi-electron/releases/tag/v4.2.1
Hand-tested by user — confirmed working. DMG + zip attached to the release.

Synced from upstream `deployment/` at upstream HEAD `4e2fcf64` ("built distribution directory and 4.2").
Local `shiny/` (app.R + 119 module files) now byte-identical to upstream deployment.

New since v4.2.0 (Apr 25):
- **ANOVA Taguchi loss optimization** — major feature. New `anova/server/optimization/*`,
  `anova/server/loss/*`, `anova/utils/optimization/*` (taguchi_loss, dispersion metrics,
  multiresponse, search_strategy), `mf_optimization_readiness.R`.
- **New module:** `learning/simulators/` — CLT, power, ANOVA concept simulators (teaching demos).
- **SPC capability** tab (`spc/server/spc_capability_server.R` + UI).
- **EDA quantile-algorithm picker** (`eda/ui/quantile_type_ui.R`, `eda/utils/quantile_types.R`, pooled_all_row).
- Misc: `one_two_sample_tests/ots_group_utils.R`, sample_size_power pearson-r utils, many bug fixes.

Verification done:
- No new R package deps — bundled R 4.5.1 has all 25 required packages.
- **propagate fork installed** into `r-mac/` (was CRAN 1.0-7 → now ProfessorPeregrine 1.0-6 fork).
  Fixes scatterplot CI/PI rendering. See PROJECT-MEMORY "R Runtime Assembly". Governance audit `gov-494fabfd3c32` (PROCEED).
- Smoke test green: Shiny boots, all modules source clean, `Listening on 127.0.0.1`, NO fork warning.
- Build verified: packaged `.app` bundles the fork (RemoteUsername ProfessorPeregrine).

Local build artifacts at `stats4roi/out/make/` (git-ignored): `stats4ROI-4.2.1.dmg`
(362 MB) + `zip/darwin/arm64/stats4ROI-darwin-arm64-4.2.1.zip` (368 MB) — both
attached to the GitHub release.

Shipped commit `6b7d06f` (source sync + docs) + tag `v4.2.1` pushed to `origin` (jason21wc).

## Next Actions

No pending actions. v4.2.1 released and distributed.

## Open Questions

None.

---
*Last Updated: 2026-06-28*
