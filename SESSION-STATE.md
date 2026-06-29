# Session State

> Working memory — current position and immediate next actions only.
> Reset when context changes significantly.

## Current Position

| Field | Value |
|-------|-------|
| Phase | Maintenance |
| Mode | Standard |
| Active Task | v4.2.1 built + DMG verified/opened for hand-test — running completion sequence (commit/tag/release) |
| Blocked By | — |

## Context

**v4.2.1 synced from upstream + built (Jun 28, 2026).** Upstream `deployment/`
pulled at upstream HEAD `4e2fcf64` ("built distribution directory and 4.2").
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

Artifacts at `stats4roi/out/make/`:
- `stats4ROI.dmg` (362 MB)
- `zip/darwin/arm64/stats4ROI-darwin-arm64-4.2.1.zip` (369 MB)

## Next Actions

DMG verified (only DMG in project; internal `CFBundleShortVersionString` = 4.2.1;
fork bundled) and opened for hand-test on Jun 28, 2026.

1. **Hand-test the DMG** (in progress) — spot-check the new ANOVA optimization tab +
   a scatterplot fit (confirms propagate fork works).
2. **Commit + tag `v4.2.1`** the synced sources + doc updates (sync staged in working
   tree; r-mac/ and out/ git-ignored).
3. **GitHub release** v4.2.1 on `jason21wc/stats4roi-electron` (NEVER upstream) —
   attach DMG + zip. Awaiting user go-ahead.

## Open Questions

None.

---
*Last Updated: 2026-06-28*
