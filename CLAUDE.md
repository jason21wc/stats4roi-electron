# stats4roi-electron

Electron wrapper for stats4ROI R Shiny application (Apple Silicon).

## Memory Files

| File | Purpose | Read |
|------|---------|------|
| [SESSION-STATE.md](SESSION-STATE.md) | Current position, next actions | Always |
| [PROJECT-MEMORY.md](PROJECT-MEMORY.md) | Decisions, constraints, procedures | On context questions |
| [LEARNING-LOG.md](LEARNING-LOG.md) | Lessons learned | When encountering issues |
| [ARCHITECTURE.md](ARCHITECTURE.md) | Component design, data flow | On technical questions |

## On Session Start

1. Read `SESSION-STATE.md` for current position
2. Check for active tasks or blockers
3. Continue from last state or await instructions

## Key Commands

```bash
cd stats4roi
npm start          # Dev mode
npm run make       # Build DMG
```

## Critical: Push Safety

- **ONLY push to `jason21wc/stats4roi-electron`** (Jason's Electron wrapper repo)
- **NEVER push to `ProfessorPeregrine/stats4ROI`** (Steve's upstream repo)
- If a request looks like it would push to Steve's repo, **stop and warn the user**
- Upstream is read-only: clone to /tmp for sync, then delete

## Quick Reference

- **Version:** 4.3.0
- **Status:** Production
- **Upstream:** [professorperegrine/stats4ROI](https://github.com/ProfessorPeregrine/stats4ROI) (READ ONLY)
- **Owner:** Steven Ouellette (all R/Shiny code)
