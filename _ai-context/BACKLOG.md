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

### #3 — Emit a versioned DMG filename from the maker — DONE 2026-08-30

`forge.config.js` derives the DMG name from package.json. The v4.3.1 build emitted and
verified `out/make/stats4ROI-4.3.1.dmg`; no manual rename is required.

### #6 — Fork script built propagate under system R via `r-mac/bin/Rscript` — DONE 2026-09-03

`r-mac/bin/Rscript` hardwires the system framework path, so the fork shipped compiled
under R 4.5.2 while running in bundled 4.5.1 (startup warning in v4.3.0 and the v4.3.1
candidate). `ensure-propagate-fork.sh` now uses `bin/R` with `R_HOME_DIR`, pins
`PKG_LIBS` past the Makevars `Rscript` call, and `--check` fails on a build-version
mismatch. See LEARNING-LOG 2026-09-03.

## Open

### #7 — Report residual discrete-distribution boundary display to Steve

**Status:** Open — upstream nit, not a release blocker.
**Trigger:** Next message to Steve.

Excluding R=0 in the lower tail or R=n in the upper tail of binomial/Poisson/
hypergeometric moves the cutoff outside the support (-1 or n+1) and the table lookup
returns empty/NA, so the UI renders `p(-1) = ` with blank cells instead of 0 and 1.
Reproduced against upstream `ee8379a`. No wrong number is shown; the cells are blank.
Also mention: `tests/testthat/helpers/extract_app_outputs.R` sources
`modules/statistical/eda/utils/quantile_type6.R`, which is absent from `deployment/`.

### #2 — Code signing and notarization

**Status:** Discussion — needs a cost/benefit decision.
**Trigger:** If Gatekeeper friction generates real user support load.

`forge.config.js` sets `osxSign: false` / `osxNotarize: false`. Every user must
right-click → Open or run `xattr -cr`, and downloads can present as "damaged."
Signing requires a paid Apple Developer account and a notarization step in the build.
Decide whether the distribution volume justifies it.

### #4 — Intel (x86_64) builds

**Status:** Deferred — deliberately out of scope.
**Trigger:** A user on Intel hardware asks for it.

Apple Silicon only, by decision (PROJECT-MEMORY, 2024-01-24). Would require a second
bundled R runtime and a universal or separate build. Revisit only on real demand.

### #5 — Pin the supported Node build version

**Status:** Active — small build-reproducibility improvement.
**Trigger:** After the v4.3.1 candidate is built and hand-tested.

The installed `macos-alias` native module targets Node ABI 127 (Node 22), while the
default Homebrew PATH can select Node 25 / ABI 141 and fail only at DMG creation. Add a
single canonical Node pin (`.nvmrc` plus package metadata or equivalent) and document
the intentional dependency refresh path when changing it. The build entry point must
put Node 22's complete `bin` directory first in `PATH` for npm and all children, then
verify ABI 127; `.nvmrc` or `engines` alone does not enforce the child process tree.

---
*Last Updated: 2026-09-03*
