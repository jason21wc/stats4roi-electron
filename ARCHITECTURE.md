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
│  │  │  │  │  │        modules/ (170 files)   │  │  │  │  │  │
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

`main.js` launches native `r-mac/bin/exec/R` directly, supplies the complete bundled
`R_HOME`/content-directory environment plus `RE_SHINY_PORT` and `RE_SHINY_PATH`, then
runs `start-shiny.R`. It also puts `r-mac/lib` first in `DYLD_LIBRARY_PATH`. Direct native
launch matters on macOS: loader variables supplied to a shell script are stripped, and
R's absolute framework install names could otherwise prefer an installed system R over
the libraries bundled inside the app. The patched `r-mac/bin/R` wrapper applies the same
loader containment for standalone runtime commands.

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

**Codebase stats:** 170 module files, ~79,500 lines of R code (app.R + modules).

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

The build requires Node 22 (pinned in `stats4roi/.nvmrc`). The DMG maker reaches the
compiled `macos-alias` module through `ds-store`, and a compiled module is locked to a
single Node major version. Under any other version the build packages the app and writes
the ZIP, then fails at the final DMG step. `npm run make` and `npm run package` therefore
run `scripts/check-build-node.js` first, which refuses to start and prints the exact
`PATH` line to fix it. `.npmrc` sets `engine-strict` so `npm install` under the wrong Node
is refused too, since installing there would compile the module for the wrong version.

```
npm run make
     │
     ▼
┌─────────────────────────┐
│  Preflight (premake)    │
│  check-build-node.js    │
│  - Node major = .nvmrc  │
│  - macos-alias loads    │
└───────────┬─────────────┘
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
│ out/make/stats4ROI-<version>.dmg │
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

The DMG maker calls macOS `hdiutil`, so this final step requires a build process with
working DiskArbitration in addition to workspace write access.

---
*Last Updated: 2026-09-03 (v4.3.3 released)*
