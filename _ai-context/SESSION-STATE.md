# Session State

> Working memory — current position and immediate next actions only.
> Reset when context changes significantly.

## Current Position

| Field | Value |
|-------|-------|
| Phase | v4.3.6 sync — built and verified, **awaiting Jason's hand-test** |
| Mode | branch `sync/v4.3.6` (local, unpushed); `main` = `origin/main` at v4.3.3 docs |
| Active Task | Hand-test of `stats4roi/out/make/stats4ROI-4.3.6.dmg` |
| Blocked By | Jason's hand-test — do NOT push, tag, or release before he approves |

## Context

**Request (Oct 19, 2026):** Steve's email ("Re: New version of stats4ROI!") said v4.36
is on GitHub and asked us to build the Mac version. He was using his lolcat fork until
Mike accepted his PRs. Mike merged PR #15 into `burrm/lolcat` the same day, so
`burrm/lolcat@f0b8af6` has zero file diff from Steve's fork head `92a9910`.

**Synced:** upstream `deployment/` at `0ad0cf3` ("increment version and deployment",
2026-10-19), 32 commits after `ee8379a`. `shiny/` is byte-identical to it: 37 files
modified, 2 added (`modules/config/deferred_module_loader.R`,
`one_two_sample_tests/utils/np_location_proportion_cis.R`), 0 deleted, `www/`
unchanged, 172 module files. UI shows v4.36; Electron semver 4.3.6 (4.3.4 and 4.3.5
never existed on our side).

New in 4.35/4.36: modules load lazily (first tab visit or idle prefetch); Wilcoxon
location test CIs above and below the hypothesized median; the two-tailed power/sample
size option is relabelled "Not equal to null"; exact one-sample Pearson r power and
sample size; Greek letters and better graphics on power curves.

**lolcat 2.0.0 → 2.0.2 @ f0b8af6 (bundled runtime).** Required: the app calls two
functions that 2.0.0 lacks. Installed with bundled `bin/R` (built under 4.5.1). It is
the only package changed in `r-mac/library`, and the old one is backed up in this
session's scratchpad. `install_stats4roi_packages.R` now gates on `RemoteSha` (see
PROJECT-MEMORY decision). Upstream overrides of `spc.controlviolation.zones.classify`
and `cor.bowker.symmetry.1948` are safe, because neither file changed between the two
lolcat versions.

**Verification (all pass unless noted):**
- 173 R files parse. Every deferred module sources eagerly through upstream's manifest
  switch, with no warnings. Upstream `tools/smoke_deferred_load.R`: 26 modules, 32 UIs,
  32 tabs activate.
- Pre-flight: all referenced packages present; 35/35 `lolcat::` calls exported.
- Independent math: one- and two-sample t power equal `stats::power.t.test(strict=TRUE)`
  to ~1e-16. Exact Pearson r power matches 400k-rep Monte Carlo within about 1 SE in
  four configurations (two-sided, greater, less, nonzero nulls), and simulated size is
  ≈ .05. The critical r at ρ0=0 equals the t closed form. Sample size n=44 is minimal
  (power .7918 at 43, .8002 at 44).
- Upstream tests, from upstream's root with bundled R: new power tests pass
  (`test_power_curve` 167/167, `test_power_lolcat_parity` 6 + 1 skip,
  `test_sample_size_power_parity` 18/18, `test_np_location_proportion_cis` 10/10), as
  do EDA damage/parity, distributions, binomial include-R, and others. 16 files
  fail or error identically under lolcat 2.0.0, so they are pre-existing upstream test
  debt (BACKLOG #9).
- `./ensure-propagate-fork.sh --check` exit 0. Node 22.18.0 gate passes.
- Build: `npm run make` exit 0.
  - `stats4roi/out/make/stats4ROI-4.3.6.dmg`: 379,944,135 bytes, SHA-256
    `ec97377754fb7591ba279c8050cff1d9bef0bb1f05ef0a8ed33eea9cfab7f349`;
    `hdiutil verify` VALID.
  - `stats4roi/out/make/zip/darwin/arm64/stats4ROI-darwin-arm64-4.3.6.zip`:
    386,316,769 bytes, SHA-256
    `695bcb42d8475bd81999e6210cc6ef7a0f9b16e231fdd1743148c964575f8bde`;
    `unzip -tq` clean.
  - Info.plist 4.3.6. The packaged `shiny/` equals the tree. Packaged lolcat is 2.0.2 @
    f0b8af6, and packaged propagate is the fork, built under 4.5.1.
- Packaged R launched exactly as `main.js` does: HTTP 200 in **4.2 s** (4.3.3 took
  ~7 s); serves v4.36 with 32 deferred placeholders. libR, BLAS, LAPACK and gfortran
  map from inside the `.app`, with zero `/Library/Frameworks/R.framework` mappings.
  The only log warning is Shiny's `warn_if_app_dir_is_package`, which predates this
  sync (the NAMESPACE/DESCRIPTION files have been in `shiny/` since v4.1.0) and is
  harmless because there is no `R/` dir.

**Hand-test focus for Jason:**
1. Cold launch: time to the Welcome page; first visit to a heavy tab (SPC, ANOVA
   Multi-Factor) shows a spinner, then loads; revisiting is instant.
2. Sample Size/Power → one-sample Pearson r **exact**: power and sample size, each
   alternative. Spot check: ρ0=.3, ρ1=.6, greater, α=.05, power .8 → n=44.
3. Sample Size/Power: the two-tailed option now reads "Not equal to null"; power curves
   show Greek letters.
4. One- and Two-Sample Tests → Nonparametric → Wilcoxon location: CIs above and below
   the hypothesized median.
5. Correlation → Scatterplot CI/PI bands (propagate now loads lazily in this module).
6. The One- and Two-Sample Tests, MSA, and ANOVA menus are now built in `app.R` as
   deferred tabs (previously inside each module). Click every entry once.

**Search caveat:** the context-engine index covers only docs + Electron JS. It has no R
support — use Grep/Glob for anything under `shiny/`. See LEARNING-LOG 2026-07-25.

## Next Actions

1. Jason hand-tests the 4.3.6 DMG.
2. On approval: fast-forward `main` from `sync/v4.3.6`, tag `v4.3.6`, push to `origin`
   (jason21wc ONLY), then create the release empty and upload the DMG and ZIP
   separately (PROJECT-MEMORY Release Procedure).
3. Optional: when replying to Steve, include the test-debt note (BACKLOG #9).

## Open Questions

None blocking. BACKLOG #7 (discrete boundary display) is still awaiting Steve; the
distributions code was unchanged in v4.36.

---
*Last Updated: 2026-10-19*
