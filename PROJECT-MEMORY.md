# Project Memory

> Semantic memory — accumulated decisions, constraints, and project knowledge.
> Append-only; do not delete entries.

## Project Identity

| Field | Value |
|-------|-------|
| Name | stats4roi-electron |
| Purpose | Electron wrapper for stats4ROI R Shiny application |
| Target Platform | Apple Silicon Macs (ARM64) |
| Current Version | 4.1.2 |
| Status | Production |

## Specification Summary

Wrap Steven Ouellette's stats4ROI R Shiny application in Electron for standalone distribution on Apple Silicon Macs. The R runtime is bundled; users need no R installation.

## Architecture Decisions

| Decision | Rationale | Date |
|----------|-----------|------|
| Modular v4.0 architecture | Upstream switched from monolithic to modular; aligns with Steve's direction | 2024-01-24 |
| Bundle R 4.5.1 runtime | Self-contained app; no user R installation required | 2024-01-24 |
| Apple Silicon only | Simplifies build; Intel builds possible but not prioritized | 2024-01-24 |
| One-way sync from upstream | We package, Steve develops; no code modifications | 2024-01-24 |

## Technical Stack

| Component | Technology |
|-----------|------------|
| Application Framework | Electron (via Electron Forge) |
| Statistical Engine | R 4.5.1 (bundled) |
| UI Framework | R Shiny (modular) |
| Package Format | DMG |
| Build Tool | npm / Electron Forge |

## Upstream Attribution

| Field | Value |
|-------|-------|
| Repository | [professorperegrine/stats4ROI](https://github.com/ProfessorPeregrine/stats4ROI) |
| Author | Steven Ouellette |
| Email | steve@roi-ally.com |
| Website | https://www.roi-ally.com |
| Sync Source | `deployment/` directory |

**All R/Shiny code is Steve's work.** This repository provides Electron packaging only.

## Constraints

- **Platform:** Apple Silicon only (M1/M2/M3/M4)
- **macOS:** 11 (Big Sur) or later
- **Code ownership:** Do not modify upstream R code; sync only
- **Push safety:** ONLY push to `jason21wc/stats4roi-electron`. NEVER push to `ProfessorPeregrine/stats4ROI`. Warn user if a request appears to target Steve's repo.
- **Git-ignored:** r-mac/ (~800MB), out/, node_modules/

## Known Gotchas

| Issue | Solution |
|-------|----------|
| Gatekeeper blocks first launch | Right-click → Open, or run `xattr -cr /Applications/stats4ROI.app` |
| "Damaged" warning on download | Same as above — extended attributes issue |
| Slow first launch | Normal; R runtime initializing (~10-15 seconds) |

## Phase Gates

| Phase | Status | Date |
|-------|--------|------|
| Initial Setup | Complete | 2024-01-24 |
| Architecture Migration (v3.2→v4.0) | Complete | 2024-01-24 |
| Build & Test | Complete | 2024-01-24 |
| GitHub Release | Complete | 2024-01-24 |
| Documentation | Complete | 2025-01-26 |

## Source Documents Registry

| Document | Purpose | Location |
|----------|---------|----------|
| README.md | User-facing installation and usage | `/README.md` |
| ARCHITECTURE.md | Technical component design | `/ARCHITECTURE.md` |
| LICENSE.md | MIT License | `/LICENSE.md` |
| package.json | Electron/npm configuration | `/stats4roi/package.json` |

## Sync Procedure (for future updates)

```bash
# Clone upstream temporarily
git clone --depth 1 https://github.com/ProfessorPeregrine/stats4ROI.git /tmp/upstream

# Copy deployment files
cp /tmp/upstream/deployment/stats4ROI_mod.R stats4roi/shiny/app.R
cp -r /tmp/upstream/deployment/modules/* stats4roi/shiny/modules/
cp -r /tmp/upstream/deployment/www/* stats4roi/shiny/www/

# Clean up
rm -rf /tmp/upstream

# Test and rebuild
cd stats4roi && npm start
npm run make
```

---
*Last Updated: 2026-01-30*
