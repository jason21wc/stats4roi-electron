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
│  │  │  │  │  │        modules/ (169 files)   │  │  │  │  │  │
│  │  │  │  │  │  config/  data/  statistical/ │  │  │  │  │  │
│  │  │  │  │  │  distributions/  reliability/ │  │  │  │  │  │
│  │  │  │  │  │  learning/                    │  │  │  │  │  │
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
| `index.js` | Entry stub; loads `main.js` via `esm` |
| `main.js` | Spawns R via `execa`, creates BrowserWindow, handles app lifecycle |
| `loading.html` / `loading.css` | Splash screen shown while the R runtime boots |

`main.js` sets `R_HOME_DIR` to the bundled `r-mac/` path, plus `RE_SHINY_PORT` and
`RE_SHINY_PATH`, then runs `start-shiny.R`. That override matters: `r-mac/bin/R`
hardcodes `R_HOME_DIR` to `/Library/Frameworks/...`, so without it the app would
fall back to a system R installation that may not exist on a user's machine.

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
| `modules/statistical/` | Analysis tools (see submodules below) |
| `modules/reliability/` | Reliability Calculator, Growth Analysis (Crow-AMSAA), Weibull |
| `modules/learning/` | Teaching simulators (CLT, power, ANOVA) |
| `www/` | Static assets (icons, images) |

**`modules/statistical/` submodules:** `anova`, `autocorrelation`,
`correlation_association`, `crosstabs`, `doe_orthogonal`, `eda`, `msa`,
`one_two_sample_tests`, `sample_size_power`, `spc`.

Most submodules follow a `<name>_module.R` entry point plus `ui/`, `server/`, and
`utils/` subdirectories. `spc` is the largest, covering variables/attributes charts,
CUSUM, EWMA, Process Performance Analysis, capability, and distribution fitting.

**Codebase stats:** 169 module files, ~79,300 lines of R code (app.R + modules).

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
| Shiny app (shiny/) | ~5MB | Yes |
| Electron (src/) | ~10KB | Yes |
| Final DMG | ~362MB | No |
| Final zip | ~368MB | No |

---
*Last Updated: 2026-07-25 (v4.3.0)*
