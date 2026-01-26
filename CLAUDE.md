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

## Quick Reference

- **Version:** 4.1.0
- **Status:** Production
- **Upstream:** [professorperegrine/stats4ROI](https://github.com/ProfessorPeregrine/stats4ROI)
- **Owner:** Steven Ouellette (all R/Shiny code)
