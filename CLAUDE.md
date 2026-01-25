# stats4roi-electron

## Project Overview

Electron-wrapped R Shiny application for statistical analysis, built for Apple Silicon (ARM64) Macs.

**Current Version:** 4.1.0 (UI displays "stats4ROI v4.0")
**Last Updated:** 2024-01-24
**Status:** Production-ready DMG built and tested

## Architecture

```
stats4roi-electron/
├── stats4roi/                    # Main application
│   ├── shiny/                    # R Shiny app (modular v4.0)
│   │   ├── app.R                 # Entry point (632 lines)
│   │   ├── modules/              # 85 R module files (40,894 lines total)
│   │   │   ├── config/           # Global config, data invalidation
│   │   │   ├── data/             # Import, modification, filtering
│   │   │   ├── distributions/    # Binomial, Normal, Poisson, etc.
│   │   │   └── statistical/      # EDA, ANOVA, Crosstabs, SPC, MSA
│   │   ├── www/                  # Static assets (icons, images)
│   │   └── _archive/             # Old monolithic app.R (33,033 lines)
│   ├── src/                      # Electron source
│   ├── r-mac/                    # Bundled R 4.5.1 runtime (not in git)
│   ├── out/make/                 # Build output (not in git)
│   │   └── stats4ROI.dmg         # Distribution (~362MB)
│   ├── package.json              # Electron config (v4.1.0)
│   └── forge.config.js           # Electron Forge build config
├── README.md                     # Template setup instructions
└── CLAUDE.md                     # This file
```

## Key Information

### Upstream Source
- **Repository:** `professorperegrine/stats4ROI`
- **Author:** Steven Ouellette (steve@roi-ally.com)
- **Sync Pattern:** One-way pull from upstream `deployment/` directory

### Version History
| Version | Type | Lines | Files | Notes |
|---------|------|-------|-------|-------|
| 3.2 | Monolithic | 33,033 | 1 | Archived in `_archive/` |
| 4.0/4.1 | Modular | 40,894 | 85 | Current production |

### Build Commands
```bash
cd stats4roi
npm start          # Dev mode
npm run make       # Build DMG (outputs to out/make/)
```

### Git Branches
- `main` - Production (v4.1.0 modular)
- `backup/pre-modular-update` - Pre-update snapshot
- `feature/modular-update` - Feature branch (merged)

### Files Excluded from Git
- `stats4roi/r-mac/` - Bundled R runtime (~800MB)
- `stats4roi/out/` - Build artifacts
- `stats4roi/node_modules/` - npm dependencies

## Recent Changes (2024-01-24)

1. Updated from monolithic v3.2 to modular v4.0 architecture
2. Pulled latest code from `professorperegrine/stats4ROI` deployment folder
3. Archived old `app.R` for reference
4. Cleaned up template files from root directory
5. Built and tested DMG successfully

## Testing Checklist

- [x] App launches without R errors
- [x] Welcome page displays with images
- [x] Version shows "stats4ROI v4.0"
- [x] DMG installs correctly
- [x] Installed app launches from Applications

## Distribution

**DMG Location:** `stats4roi/out/make/stats4ROI.dmg` (362MB)

Ready to distribute to Steve/users.
