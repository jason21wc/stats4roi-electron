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

## Closed

### #1 — Automate the propagate fork install into r-mac assembly — DONE 2026-07-25

Shipped `stats4roi/ensure-propagate-fork.sh` (idempotent, self-verifying, restores its
backup on a bad result) and fixed the real root cause in
`install_stats4roi_packages.R`: the fork install was gated on `!require("propagate")`,
so a transitively-installed CRAN propagate made `require()` succeed and skipped the
fork entirely while reporting success. Now gated on `RemoteUsername`, and it hard-fails
instead of skipping. Verified end-to-end against a simulated CRAN state.

## Open

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
