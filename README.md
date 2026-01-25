# stats4ROI Electron

Electron-wrapped R Shiny statistical analysis application for Apple Silicon Macs.

## About

stats4ROI is a statistical analysis tool created by [Steven Ouellette](https://www.roi-ally.com) that provides a friendly graphical interface to R's powerful statistical capabilities without requiring any coding knowledge.

**Version:** 4.1.0 (UI displays "stats4ROI v4.0")

## Features

- **Distributions:** Binomial, Normal, Poisson, Hypergeometric, Geometric, Exponential, Weibull, F Distribution
- **Statistical Analysis:** EDA, ANOVA, Crosstabs, Correlation/Association
- **Quality Tools:** SPC (Statistical Process Control), MSA (Measurement System Analysis)
- **Data Tools:** Import, filtering, variable transformation
- **Sample Size/Power** calculations

## Installation

Download the latest `stats4ROI.dmg` from [Releases](https://github.com/jason21wc/stats4roi-electron/releases), open it, and drag stats4ROI to your Applications folder.

**Requirements:** macOS on Apple Silicon (M1/M2/M3)

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

- **stats4ROI:** Steven Ouellette ([ROI Alliance](https://www.roi-ally.com))
- **Electron Template:** Based on [shiny-electron-template-m1](https://github.com/zarathucorp/shiny-electron-template-m1)

## License

MIT License - See [LICENSE.md](LICENSE.md)
