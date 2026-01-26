# stats4roi-electron

## Project Overview

Electron-wrapped R Shiny application for statistical analysis, built for Apple Silicon (ARM64) Macs.

**Current Version:** 4.1.0 (UI displays "stats4ROI v4.0")
**Repository:** https://github.com/jason21wc/stats4roi-electron
**Last Updated:** 2025-01-26
**Status:** Production-ready, DMG built and tested

## Project Structure

```
stats4roi-electron/
├── .gitignore
├── CLAUDE.md               # This file - dev notes & session state
├── LICENSE.md
├── README.md               # User-facing documentation
└── stats4roi/              # Self-contained Electron application
    ├── shiny/              # R Shiny app (modular v4.0)
    │   ├── app.R           # Entry point (632 lines)
    │   ├── modules/        # 85 R module files (40,894 lines total)
    │   │   ├── config/     # Global config, data invalidation
    │   │   ├── data/       # Import, modification, filtering
    │   │   ├── distributions/  # Binomial, Normal, Poisson, etc.
    │   │   └── statistical/    # EDA, ANOVA, Crosstabs, SPC, MSA
    │   ├── www/            # Static assets (icons, images)
    │   └── _archive/       # Old monolithic app.R (33,033 lines)
    ├── src/                # Electron source (main.js, etc.)
    ├── r-mac/              # Bundled R 4.5.1 runtime (git-ignored)
    ├── out/make/           # Build output (git-ignored)
    │   └── stats4ROI.dmg   # Distribution (~362MB)
    ├── node_modules/       # npm dependencies (git-ignored)
    ├── package.json        # Electron config (v4.1.0)
    └── forge.config.js     # Electron Forge build config
```

## Upstream Source

- **Repository:** `professorperegrine/stats4ROI`
- **Author:** Steven Ouellette (steve@roi-ally.com)
- **Website:** https://www.roi-ally.com
- **Sync Pattern:** One-way pull from upstream `deployment/` directory

## Version History

| Version | Type | Lines | Files | Notes |
|---------|------|-------|-------|-------|
| 3.2 | Monolithic | 33,033 | 1 | Archived in `_archive/` |
| 4.0/4.1 | Modular | 40,894 | 85 | Current production |

## Build Commands

```bash
cd stats4roi
npm start          # Dev mode
npm run make       # Build DMG (outputs to out/make/)
```

## Git Configuration

- **Remote:** `origin` → https://github.com/jason21wc/stats4roi-electron.git
- **Branch:** `main` (only branch)

### Files Excluded from Git
- `stats4roi/r-mac/` - Bundled R runtime (~800MB)
- `stats4roi/out/` - Build artifacts
- `stats4roi/node_modules/` - npm dependencies

## Session History

### 2025-01-26
- Added detailed installation instructions to README (requirements, steps, troubleshooting)

### 2024-01-24 — Initial Release

#### Completed Tasks
1. Updated from monolithic v3.2 to modular v4.0 architecture
2. Pulled latest code from `professorperegrine/stats4ROI` deployment folder
3. Archived old `app.R` for reference
4. Built and tested DMG successfully
5. Installed and verified app launches correctly
6. Created GitHub repository under `jason21wc` account
7. Cleaned up root directory (removed template leftovers)
8. Updated README with project-specific documentation
9. Created GitHub Release v4.1.0 with DMG download
10. Improved attribution - clarified Steve Ouellette owns all stats4ROI code, this repo is just Electron packaging

#### Testing Results
- [x] App launches without R errors
- [x] Welcome page displays with images
- [x] Version shows "stats4ROI v4.0"
- [x] DMG installs correctly
- [x] Installed app launches from Applications

## Distribution

**GitHub Release:** https://github.com/jason21wc/stats4roi-electron/releases/tag/v4.1.0
**DMG Location:** `stats4roi/out/make/stats4ROI.dmg` (362MB)

Release v4.1.0 published and ready for distribution.

## Future Sync from Upstream

To pull updates from Steve's repo:

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
