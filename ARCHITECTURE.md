# Architecture

> System design and component responsibilities.

## Component Diagram

```
┌─────────────────────────────────────────────────────────────┐
│                     stats4ROI.app                           │
│  ┌───────────────────────────────────────────────────────┐  │
│  │                    Electron Shell                     │  │
│  │  ┌─────────────┐  ┌────────────────────────────────┐  │  │
│  │  │   main.js   │  │        BrowserWindow           │  │  │
│  │  │  (Node.js)  │  │   (Chromium → localhost:X)     │  │  │
│  │  └──────┬──────┘  └────────────────────────────────┘  │  │
│  │         │                        ▲                    │  │
│  │         │ spawns                 │ HTTP               │  │
│  │         ▼                        │                    │  │
│  │  ┌─────────────────────────────────────────────────┐  │  │
│  │  │              R Runtime (r-mac/)                 │  │  │
│  │  │  ┌───────────────────────────────────────────┐  │  │  │
│  │  │  │            Shiny Server                   │  │  │  │
│  │  │  │  ┌─────────────────────────────────────┐  │  │  │  │
│  │  │  │  │           app.R (entry)             │  │  │  │  │
│  │  │  │  │  ┌───────────────────────────────┐  │  │  │  │  │
│  │  │  │  │  │         modules/ (85 files)   │  │  │  │  │  │
│  │  │  │  │  │  config/  data/  statistical/ │  │  │  │  │  │
│  │  │  │  │  │  distributions/               │  │  │  │  │  │
│  │  │  │  │  └───────────────────────────────┘  │  │  │  │  │
│  │  │  │  └─────────────────────────────────────┘  │  │  │  │
│  │  │  └───────────────────────────────────────────┘  │  │  │
│  │  └─────────────────────────────────────────────────┘  │  │
│  └───────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────┘
```

## Component Responsibilities

### Electron Shell (`stats4roi/src/`)

| File | Responsibility |
|------|----------------|
| `main.js` | Spawns R process, creates BrowserWindow, handles app lifecycle |
| `preload.js` | Bridge between renderer and main process (if used) |

**Behavior:**
1. On launch, spawns R with `shiny/app.R`
2. Waits for Shiny to bind to a local port
3. Opens BrowserWindow pointing to `localhost:<port>`
4. On close, terminates R process

### R Runtime (`stats4roi/r-mac/`)

Bundled R 4.5.1 installation for ARM64 macOS. Contains:
- R executable
- Required packages (shiny, tidyverse, etc.)
- Package library

**Not tracked in git** due to size (~800MB).

### Shiny Application (`stats4roi/shiny/`)

| Path | Responsibility |
|------|----------------|
| `app.R` | Entry point; loads modules, defines UI/server |
| `modules/config/` | Global configuration, data invalidation |
| `modules/data/` | Import, modification, filtering |
| `modules/distributions/` | Probability distributions (Binomial, Normal, etc.) |
| `modules/statistical/` | Analysis tools (EDA, ANOVA, SPC, MSA, etc.) |
| `www/` | Static assets (icons, images) |

**Codebase stats:** 85 module files, ~41,000 lines of R code.

## Data Flow

```
User Action → BrowserWindow → HTTP → Shiny Server → R computation → HTTP → BrowserWindow → Display
```

1. User interacts with UI in Chromium-based window
2. Shiny's JavaScript sends requests to R server
3. R processes data, runs statistical computations
4. Results return as HTML/JSON to browser
5. UI updates reactively

## Build Process

```
npm run make
     │
     ▼
┌─────────────────────────┐
│   Electron Forge        │
│   (forge.config.js)     │
└───────────┬─────────────┘
            │
            ▼
┌─────────────────────────┐
│   Package Application   │
│   - Bundle src/         │
│   - Bundle shiny/       │
│   - Bundle r-mac/       │
└───────────┬─────────────┘
            │
            ▼
┌─────────────────────────┐
│   Create DMG            │
│   out/make/stats4ROI.dmg│
└─────────────────────────┘
```

## File Size Breakdown

| Component | Size | In Git |
|-----------|------|--------|
| R runtime (r-mac/) | ~800MB | No |
| Shiny app (shiny/) | ~2MB | Yes |
| Electron (src/) | ~10KB | Yes |
| Final DMG | ~362MB | No |

---
*Last Updated: 2025-01-26*
