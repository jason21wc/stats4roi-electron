# Backlog

**Memory Type:** Prospective (intentions)
**Lifecycle:** Items are added when discovered, removed when implemented or abandoned.
Git history is the archive (`git log --grep="backlog #N"`).

> **Scope.** This is a packaging repo in maintenance mode — the recurring work is
> "sync upstream → verify → build → release," a few times a year. Items here are
> improvements to *that* pipeline. Statistical/R features belong upstream with Steve
> and are explicitly out of scope.
>
> **Anticipatory items are valid** — an item does not need an active failure to earn
> a place here.

---

## Open

### #1 — Automate the propagate fork install into r-mac assembly

**Status:** Active — structural fix for a defect that has recurred once.
**Trigger:** Any rebuild or refresh of the `r-mac/` runtime.

`r-mac/` is assembled without a fork-install step, so it silently reverts to CRAN
`propagate` on every rebuild, which corrupts scatterplot CI/PI rendering. Today the
only detection is `app.R`'s startup warning — downstream of the damage. The manual
procedure is in PROJECT-MEMORY "R Runtime Assembly"; folding it into
`install_stats4roi_packages.R` (or a wrapper around `get-r-mac.sh`) removes the
recurring footgun. See LEARNING-LOG 2026-06-28.

### #2 — Code signing and notarization

**Status:** Discussion — needs a cost/benefit decision.
**Trigger:** If Gatekeeper friction generates real user support load.

`forge.config.js` sets `osxSign: false` / `osxNotarize: false`. Every user must
right-click → Open or run `xattr -cr`, and downloads can present as "damaged."
Signing requires a paid Apple Developer account and a notarization step in the build.
Decide whether the distribution volume justifies it.

### #3 — Emit a versioned DMG filename from the maker

**Status:** Active — small, mechanical.
**Trigger:** Next release.

`@electron-forge/maker-dmg` emits `stats4ROI.dmg`; releases use
`stats4ROI-<version>.dmg`, so every release needs a manual rename (documented in
PROJECT-MEMORY "Release Procedure"). The zip maker already versions its output.
Setting the dmg maker's `name` to include the version removes the manual step and the
chance of shipping an unversioned asset.

### #4 — Intel (x86_64) builds

**Status:** Deferred — deliberately out of scope.
**Trigger:** A user on Intel hardware asks for it.

Apple Silicon only, by decision (PROJECT-MEMORY, 2024-01-24). Would require a second
bundled R runtime and a universal or separate build. Revisit only on real demand.

---
*Last Updated: 2026-07-25*
