# stats4ROI Electron

Electron-wrapped R Shiny statistical analysis application for Apple Silicon Macs.

## About

**stats4ROI** is a statistical analysis tool created by [Steven Ouellette](https://www.roi-ally.com) of [The ROI Alliance](https://www.roi-ally.com). It provides a friendly graphical interface to R's powerful statistical capabilities without requiring any coding knowledge.

This repository packages Steve's stats4ROI application as a standalone Electron app for **Apple Silicon Macs** (M1/M2/M3/M4). The R Shiny application code is entirely Steve's work — we simply wrapped it in Electron for easy distribution.

**Version:** 4.2.1 (UI displays "stats4ROI v4.2")
**Upstream:** [professorperegrine/stats4ROI](https://github.com/ProfessorPeregrine/stats4ROI)

## Features

- **Distributions:** Binomial, Normal, Poisson, Hypergeometric, Geometric, Exponential, Weibull, F Distribution
- **Statistical Analysis:** EDA, ANOVA (incl. Taguchi loss optimization), Crosstabs, Correlation/Association
- **Design of Experiments:** DOE with Orthogonal Arrays (L4–L64)
- **Quality Tools:** SPC (Statistical Process Control), MSA (Measurement System Analysis)
- **Learning Simulators:** CLT, Power, and ANOVA teaching demos
- **Data Tools:** Import, filtering, variable transformation
- **Sample Size/Power** calculations

## Installation

### Requirements
- Mac with Apple Silicon (M1, M2, M3, or M4 chip)
- macOS 11 (Big Sur) or later

### Steps

1. **Download** the DMG file from the [Releases page](https://github.com/jason21wc/stats4roi-electron/releases)

2. **Open** the downloaded `stats4ROI.dmg` file

3. **Drag** the stats4ROI app to your Applications folder

4. **First launch** — Right-click (or Control-click) the app and select **Open**
   - You'll see a warning that the app is from an unidentified developer
   - Click **Open** to confirm

5. The app will start — this may take 10-15 seconds on first launch while R initializes

### Troubleshooting

**"stats4ROI is damaged and can't be opened"**

Run this command in Terminal, then try opening again:
```bash
xattr -cr /Applications/stats4ROI.app
```

**App won't open at all**

Go to **System Settings → Privacy & Security**, scroll down, and click **Open Anyway** next to the stats4ROI message.

## Development

### Prerequisites

- Node.js 18+
- npm 10+

### Build from Source

```bash
cd stats4roi
npm install
npm start        # Run in dev mode
npm run make     # Build DMG
```

Build output: `stats4roi/out/make/stats4ROI.dmg`

### Project Structure

```
stats4roi-electron/
├── stats4roi/              # Electron application
│   ├── shiny/              # R Shiny app (modular architecture)
│   │   ├── app.R           # Entry point
│   │   └── modules/        # 85 R module files
│   ├── src/                # Electron source
│   ├── r-mac/              # Bundled R runtime (not in repo)
│   └── package.json        # Electron config
├── CLAUDE.md               # Development notes
├── LICENSE.md
└── README.md
```

## Credits

- **stats4ROI Application:** [Steven Ouellette](https://www.roi-ally.com) — all R/Shiny code, statistical modules, and UI
- **Electron Packaging:** This repo wraps Steve's app for Apple Silicon distribution
- **Electron Template:** Based on [shiny-electron-template-m1](https://github.com/zarathucorp/shiny-electron-template-m1)

## License

MIT License - See [LICENSE.md](LICENSE.md)
