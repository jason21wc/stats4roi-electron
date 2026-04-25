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

Production-ready application. **v4.2.0 released:** https://github.com/jason21wc/stats4roi-electron/releases/tag/v4.2.0

Synced with upstream Apr 25, 2026 (37 commits since Feb 22 sync):
- **New module:** DOE / Orthogonal Array (`modules/statistical/doe_orthogonal/` — 3 R files + 9 OA design files: L4, L8, L9, L12, L16, L18, L27, L32, L64)
- **New module:** Data Transformation (`modules/data/data_transformation_module.R`)
- **New UI:** initializing spinner overlay, DataTable tab-show redraw fix
- **New UI:** "enter np" checkbox for binomial tests
- Bug fixes: ANOVA (oneway/multifactor, three-level factor), EDA (descriptives, normality), screening/DOE resolution calc, dynamic filtering, global data invalidation
- Smoke-tested in dev mode + DMG hand-tested by user — confirmed working
- DMG + zip artifacts built at `stats4roi/out/make/` and attached to GitHub release

Prior sync (Feb 22, 2026): metadata only — DESCRIPTION license → MIT, added `Remotes: ProfessorPeregrine/propagate`, LICENSE whitespace.

## Next Actions

No pending actions. v4.2.0 ready for distribution.

## Open Questions

None.

---
*Last Updated: 2026-04-25*
