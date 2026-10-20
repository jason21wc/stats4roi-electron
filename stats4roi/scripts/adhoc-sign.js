#!/usr/bin/env node
'use strict';

/**
 * Ad-hoc sign the packaged .app inside-out, then prove the result.
 *
 * WHY THIS EXISTS
 *   With `osxSign: false`, the packaged bundle carries only the linker's ad-hoc
 *   signature on the main executable and no bundle seal, so
 *   `codesign --verify --deep --strict` fails ("code has no resources but
 *   signature indicates they must be present"). On Apple Silicon a quarantined
 *   (browser-downloaded) app in that state is reported as "damaged and can't be
 *   opened — Move to Trash", with no Open Anyway in System Settings. Only
 *   `xattr -cr` in Terminal gets past it, which is where Mac students got stuck.
 *
 *   A valid ad-hoc seal costs nothing (no Apple Developer account) and turns that
 *   dead end into the ordinary unidentified-developer prompt, which Privacy &
 *   Security can approve. It is not notarization; Gatekeeper still asks once.
 *
 * WHY INSIDE-OUT, AND WHY THE R CHECK
 *   codesign seals a bundle only if its nested code is already validly signed,
 *   so Electron's frameworks and helper apps are signed first, then the outer
 *   app. (Sealing the outer app alone fails "In subcomponent: Electron
 *   Framework.framework".)
 *
 *   r-mac/bin/exec/R must keep CRAN's signature and entitlements — in
 *   particular allow-dyld-environment-variables, without which the launcher's
 *   DYLD_LIBRARY_PATH is ignored and R loads the system framework (or nothing).
 *   The R binaries live under Resources/, which codesign hashes as resources;
 *   even `--deep` does not descend there (tested 2026-10-19). Any step that
 *   re-signs every Mach-O file it finds, such as switching on Forge's
 *   `osxSign`, would replace that signature, so this script verifies it after
 *   signing instead of assuming it.
 *
 * Usage
 *   Forge postPackage hook (forge.config.js) calls signApp() for darwin builds.
 *   node scripts/adhoc-sign.js <path/to/stats4ROI.app>          sign + verify
 *   node scripts/adhoc-sign.js --check <path/to/stats4ROI.app>  verify only
 */

const fs = require('fs');
const path = require('path');
const { spawnSync } = require('child_process');

// CRAN's Developer ID team on the bundled R; see PROJECT-MEMORY R runtime notes.
const R_TEAM_ID = 'VZLD955F6P';
const R_REQUIRED_ENTITLEMENT = 'com.apple.security.cs.allow-dyld-environment-variables';

const red = (s) => `\x1b[31m${s}\x1b[0m`;
const bold = (s) => `\x1b[1m${s}\x1b[0m`;
const dim = (s) => `\x1b[2m${s}\x1b[0m`;

// codesign writes most output to stderr even on success (`-dv` prints only
// there), so always capture both streams.
function codesign(args) {
  const res = spawnSync('codesign', args, { encoding: 'utf8' });
  return { ok: res.status === 0, out: `${res.stdout || ''}${res.stderr || ''}` };
}

function nestedCode(appPath) {
  const frameworks = path.join(appPath, 'Contents', 'Frameworks');
  if (!fs.existsSync(frameworks)) return [];
  return fs
    .readdirSync(frameworks)
    .filter((name) => name.endsWith('.framework') || name.endsWith('.app'))
    .map((name) => path.join(frameworks, name));
}

function sign(target) {
  const res = codesign(['--force', '--sign', '-', target]);
  if (!res.ok) {
    throw new Error(`codesign failed for ${target}:\n${res.out.trim()}`);
  }
}

// Returns a list of problems; empty means the bundle is good to ship.
function verifyApp(appPath) {
  const problems = [];

  const seal = codesign(['--verify', '--deep', '--strict', appPath]);
  if (!seal.ok) {
    problems.push(`bundle seal invalid: ${seal.out.trim().split('\n').pop()}`);
  }

  const rBin = path.join(appPath, 'Contents', 'Resources', 'app', 'r-mac', 'bin', 'exec', 'R');
  if (!fs.existsSync(rBin)) {
    problems.push(`bundled R not found at ${rBin}`);
    return problems;
  }

  const info = codesign(['-dv', rBin]);
  if (!info.out.includes(`TeamIdentifier=${R_TEAM_ID}`)) {
    problems.push(`exec/R is no longer CRAN-signed (expected TeamIdentifier=${R_TEAM_ID})`);
  }

  const ent = codesign(['-d', '--entitlements', '-', '--xml', rBin]);
  if (!ent.out.includes(R_REQUIRED_ENTITLEMENT)) {
    problems.push(`exec/R lost ${R_REQUIRED_ENTITLEMENT}; the launcher's DYLD_LIBRARY_PATH would be ignored`);
  }

  return problems;
}

function signApp(appPath) {
  for (const nested of nestedCode(appPath)) sign(nested);
  sign(appPath);

  const problems = verifyApp(appPath);
  if (problems.length) {
    throw new Error(`ad-hoc signing did not produce a shippable bundle:\n  - ${problems.join('\n  - ')}`);
  }
}

// Forge hands postPackage the output directories, not the .app itself.
function findApps(outputPaths) {
  return outputPaths.flatMap((dir) =>
    fs
      .readdirSync(dir)
      .filter((name) => name.endsWith('.app'))
      .map((name) => path.join(dir, name))
  );
}

module.exports = { signApp, verifyApp, findApps };

if (require.main === module) {
  const args = process.argv.slice(2);
  const checkOnly = args[0] === '--check';
  const appPath = checkOnly ? args[1] : args[0];

  if (!appPath || !fs.existsSync(appPath)) {
    console.error(red(bold('  ✖ No .app given')));
    console.error(`  Usage: node scripts/adhoc-sign.js [--check] ${dim('<path/to/stats4ROI.app>')}`);
    process.exit(1);
  }

  try {
    if (checkOnly) {
      const problems = verifyApp(appPath);
      if (problems.length) throw new Error(problems.join('\n  - '));
    } else {
      signApp(appPath);
    }
  } catch (err) {
    console.error('');
    console.error(red(bold('  ✖ Signature check failed')));
    console.error(`  - ${err.message}`);
    console.error('');
    process.exit(1);
  }

  console.log(dim(`  ✓ bundle seal valid; exec/R still CRAN-signed with ${R_REQUIRED_ENTITLEMENT}`));
}
