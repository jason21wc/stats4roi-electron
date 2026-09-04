# stats4roi-electron

Electron wrapper for stats4ROI R Shiny application (Apple Silicon).

## Memory Files

Memory lives in `_ai-context/` (unified layout, v2.62.0). Technical docs stay at the root.

| File | Purpose | Read |
|------|---------|------|
| [_ai-context/SESSION-STATE.md](_ai-context/SESSION-STATE.md) | Current position, next actions | Always |
| [_ai-context/PROJECT-MEMORY.md](_ai-context/PROJECT-MEMORY.md) | Decisions, constraints, procedures | On context questions |
| [_ai-context/LEARNING-LOG.md](_ai-context/LEARNING-LOG.md) | Lessons learned | When encountering issues |
| [_ai-context/BACKLOG.md](_ai-context/BACKLOG.md) | Deferred work | When planning or when an item's trigger fires |
| [ARCHITECTURE.md](ARCHITECTURE.md) | Component design, data flow | On technical questions |

## On Session Start

1. Read `_ai-context/SESSION-STATE.md` for current position
2. Check for active tasks or blockers
3. Continue from last state or await instructions

## Key Commands

```bash
cd stats4roi
npm start          # Dev mode
npm run make       # Build DMG

./ensure-propagate-fork.sh --check   # MUST exit 0 before any build
./ensure-propagate-fork.sh           # repair: install the fork if missing
```

**Run `--check` after any `r-mac/` rebuild.** The runtime silently reverts to CRAN
`propagate`, which corrupts scatterplot CI/PI rendering. See PROJECT-MEMORY
"R Runtime Assembly".

## Critical: Push Safety

- **ONLY push to `jason21wc/stats4roi-electron`** (Jason's Electron wrapper repo)
- **NEVER push to `ProfessorPeregrine/stats4ROI`** (Steve's upstream repo)
- If a request looks like it would push to Steve's repo, **stop and warn the user**
- Upstream is read-only: clone to /tmp for sync, then delete

## Quick Reference

- **Version:** 4.3.3
- **Status:** Production
- **Upstream:** [professorperegrine/stats4ROI](https://github.com/ProfessorPeregrine/stats4ROI) (READ ONLY)
- **Owner:** Steven Ouellette (all R/Shiny code)
