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

### #5 — Pin the supported Node build version — DONE 2026-09-03

`stats4roi/.nvmrc` pins Node 22.18.0 as the single source of truth; `engines` and
`.npmrc` `engine-strict=true` refuse `npm install` under another major (which would
compile `macos-alias` for the wrong ABI); `scripts/check-build-node.js` runs as
`premake`/`prepackage` and blocks the build, printing the exact `PATH` export to fix it.
The gate loads the native module rather than only comparing versions, so it also catches
dependencies installed under the wrong Node. Verified: passes under Node 22, blocks
`npm run make` under Node 25 before packaging starts, and both failure branches print
actionable remediation.

### #6 — Fork script built propagate under system R via `r-mac/bin/Rscript` — DONE 2026-09-03

`r-mac/bin/Rscript` hardwires the system framework path, so the fork shipped compiled
under R 4.5.2 while running in bundled 4.5.1 (startup warning in v4.3.0 and the v4.3.1
candidate). `ensure-propagate-fork.sh` now uses `bin/R` with `R_HOME_DIR`, pins
`PKG_LIBS` past the Makevars `Rscript` call, and `--check` fails on a build-version
mismatch. See LEARNING-LOG 2026-09-03.

## Open

### #7 — Report residual discrete-distribution boundary display to Steve

**Status:** Reported to Steve 2026-09-03 — awaiting his call. Upstream nit, not a
release blocker; nothing for us to do but resync if he changes it. Checked at the v4.3.6
sync (2026-10-19): no file under `modules/distributions/` changed, so the behavior is
unchanged.
**Trigger:** Next upstream sync — check whether the boundary lookup changed.

Excluding R=0 in the lower tail or R=n in the upper tail of binomial/Poisson/
hypergeometric moves the cutoff outside the support (-1 or n+1) and the table lookup
returns empty/NA, so the UI renders `p(-1) = ` with blank cells instead of 0 and 1.
The upper case also propagates NA into the two-tail `p(between)` and `p(tails)` rows.
Reproduced against upstream `ee8379a`. No wrong number is shown; the cells are blank.
Also mention: `tests/testthat/helpers/extract_app_outputs.R` sources
`modules/statistical/eda/utils/quantile_type6.R`, which is absent from `deployment/`.

### #8 — `package.json` repository URL points at a repo that does not exist

**Status:** Open — needs your call, no functional effect.
**Trigger:** Next time `stats4roi/package.json` is edited for any other reason.

`stats4roi/package.json` declares
`"repository": "https://github.com/roi-ally/stats4roi"`. Verified 2026-09-03: that
repository does not resolve (`Could not resolve to a Repository`). It is neither the
push target `jason21wc/stats4roi-electron` nor upstream `ProfessorPeregrine/stats4ROI`,
so it looks like an inherited placeholder from Steve's original template. Nothing reads
this field here (the package is never published to npm, and npm does not use it to
resolve push targets), so it is cosmetic.

Left unchanged deliberately: repository identity is push-safety-relevant in this
project, and the `author` field is correctly Steve. Decide whether the field should name
the packaging repo, name Steve's upstream, or be dropped. Flagged by the coherence audit
of 2026-09-03.

### #9 — Tell Steve which of his own tests fail in his repo layout

**Status:** Open. Optional courtesy note; not a release blocker, and nothing for us to change.
**Trigger:** Next time Jason writes to Steve, e.g. the v4.3.6 release email.

At upstream `0ad0cf3`, run from upstream's root with bundled R 4.5.1, 16 of 62 testthat
files fail or error. Results are identical under lolcat 2.0.0 and 2.0.2, so the lolcat
upgrade did not cause them. Upstream's deferred-load smoke test passes.
- Missing fixtures (`cannot open the connection`): `test_anova_r_squared`,
  `test_correlation_parity`, `test_one_two_sample_parity`, `test_ppa_*` (3 files),
  `test-crosstab-module`, `test-msa-utils`, `test-spc-cusum`, `test-spc-ewma`.
- Stale assertions after refactors: `test_shiny_duplicate_ids` expects
  `downloadServer("ppa_run_chart_plot"` in a file that no longer has it; `test-spc-utils`
  expects 4 dependencies and finds 2.
- Test does not load its helper: `test_quantiles_type6` cannot find `eda_safe_numeric`.
- Possibly real, worth Steve's eyes: `test_doe_orthogonal_assign` gets "No valid
  assignment" for a design it expects to be resolution V. `test_taguchi_loss_mvp`
  predicts NA, and coefficient `c` is missing from the model.

### #2 — Code signing and notarization

**Status:** Discussion — needs a cost/benefit decision.
**Trigger:** If Gatekeeper friction generates real user support load.

`forge.config.js` sets `osxSign: false` / `osxNotarize: false`. Every user must
right-click → Open or run `xattr -cr`, and downloads can present as "damaged."
Signing requires a paid Apple Developer account and a notarization step in the build.
Decide whether the distribution volume justifies it.

**Finding 2026-10-19 (likely cause of the Aug 2026 student reports).** The bundle's
signature is *structurally broken*, not just unsigned. Only the main executable has
the linker's ad-hoc signature, and there is no bundle seal, so
`codesign --verify --deep --strict` fails with "code has no resources but signature
indicates they must be present". For a quarantined (browser-downloaded) app on Apple
Silicon, that produces "damaged and can't be opened — Move to Trash". The only way past
that is `xattr -cr` in Terminal, and Privacy & Security offers no "Open Anyway". Students
who skipped the README's Terminal step would have been stuck; others got through.
**Free fix, tested on a scratch copy:** ad-hoc sign inside-out, needing no Apple
account. Sign each `Contents/Frameworks/*.framework` and `*.app` helper with
`codesign --force --sign -`, then the outer `.app`. The result is "valid on disk,
satisfies its Designated Requirement", and the CRAN signature and entitlements on
`r-mac/bin/exec/R` are untouched. Users would then get the ordinary unidentified-developer
prompt, with Open Anyway in Settings. Hook it into Forge `postPackage` before the makers,
and verify with `codesign --verify --deep --strict` in the release checklist.
*Correction, same day:* an earlier draft of this note said `--deep` would re-sign
`exec/R`. Tested, it does not, because codesign does not descend into `Resources/`. The
real hazard is a signer that re-signs every Mach-O file it finds, such as Forge's own
`osxSign`. **Implemented 2026-10-19:** `stats4roi/scripts/adhoc-sign.js`, called from the
`postPackage` hook. It also verifies that `exec/R` keeps CRAN's TeamIdentifier and its
`allow-dyld-environment-variables` entitlement, and fails the build otherwise.
`npm run check-signature` reruns the check.

### #4 — Intel (x86_64) builds

**Status:** Deferred — deliberately out of scope.
**Trigger:** A user on Intel hardware asks for it.

Apple Silicon only, by decision (PROJECT-MEMORY, 2024-01-24). Would require a second
bundled R runtime and a universal or separate build. Revisit only on real demand.

---
*Last Updated: 2026-10-19*
