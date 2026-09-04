#!/usr/bin/env node
'use strict';

/**
 * Build preflight: refuse to start a build that cannot finish.
 *
 * WHY THIS EXISTS
 *   The DMG maker reaches `macos-alias` (a compiled module) through `ds-store`.
 *   A compiled module is locked to one Node major version. Under the wrong one
 *   the build still packages the app and writes the ZIP, then dies at the very
 *   last step with "compiled against a different Node.js version" — minutes of
 *   work discarded for a reason the error text does not explain.
 *
 *   This script converts that late, cryptic failure into an immediate, specific
 *   one. It runs as a `pre` script, so it resolves `node` from PATH exactly the
 *   way Forge's own child processes will: if this passes, they inherit the same
 *   interpreter.
 *
 * WHY IT LOADS THE MODULE INSTEAD OF COMPARING VERSION NUMBERS
 *   Comparing versions only catches "running under the wrong Node". Loading the
 *   module also catches the reverse trap: dependencies installed under the wrong
 *   Node, which compiles the binary for that version and breaks the build even
 *   when the correct Node is running.
 *
 * The required version has one home: .nvmrc.
 */

const fs = require('fs');
const path = require('path');

const REPO = path.resolve(__dirname, '..');
const NATIVE_DEP = 'macos-alias';

const red = (s) => `\x1b[31m${s}\x1b[0m`;
const bold = (s) => `\x1b[1m${s}\x1b[0m`;
const dim = (s) => `\x1b[2m${s}\x1b[0m`;

function fail(title, body) {
  console.error('');
  console.error(red(bold(`  ✖ ${title}`)));
  console.error('');
  for (const line of body) console.error(`  ${line}`);
  console.error('');
  process.exit(1);
}

// --- required version -------------------------------------------------------

const nvmrcPath = path.join(REPO, '.nvmrc');
let required;
try {
  required = fs.readFileSync(nvmrcPath, 'utf8').trim().replace(/^v/, '');
} catch {
  fail('Cannot determine the required Node version', [
    `Expected a version in ${dim(nvmrcPath)}`,
    'Restore that file; it is the single source of truth for the build Node.',
  ]);
}

const requiredMajor = required.split('.')[0];
const runningMajor = process.versions.node.split('.')[0];

// A concrete, copyable remediation line, pointing at a real install when we
// can find one rather than telling the reader to go hunt for it.
function remediation() {
  const nvmDir = path.join(
    process.env.NVM_DIR || path.join(process.env.HOME || '', '.nvm'),
    'versions',
    'node'
  );
  let match = null;
  try {
    match = fs
      .readdirSync(nvmDir)
      .filter((d) => d.replace(/^v/, '').split('.')[0] === requiredMajor)
      .sort()
      .pop();
  } catch {
    /* nvm not present; fall through to the generic instruction */
  }

  if (match) {
    return [
      `export PATH="${path.join(nvmDir, match, 'bin')}:$PATH"`,
      'npm run make',
    ];
  }
  return [
    `nvm install ${required} && nvm use ${required}`,
    'npm run make',
  ];
}

if (runningMajor !== requiredMajor) {
  fail('Wrong Node version for this build', [
    `Required   Node ${requiredMajor}.x        ${dim('(from .nvmrc: ' + required + ')')}`,
    `Running    Node ${process.versions.node}  ${dim('ABI ' + process.versions.modules)}`,
    `Executable ${dim(process.execPath)}`,
    '',
    `The DMG maker loads ${bold(NATIVE_DEP)}, a compiled module locked to one Node`,
    'major version. Building under any other version fails at the final DMG',
    'step, after packaging has already run.',
    '',
    bold('Fix — put the right Node first on PATH, then rebuild:'),
    ...remediation().map((c) => `  ${bold(c)}`),
    '',
    dim('Calling npm by absolute path is not enough: npm\'s shebang and its'),
    dim('child processes re-resolve `node` from PATH.'),
  ]);
}

// --- the module the build actually depends on -------------------------------

let resolved;
try {
  resolved = require.resolve(NATIVE_DEP, { paths: [REPO] });
} catch {
  fail(`${NATIVE_DEP} is not installed`, [
    'The DMG maker needs it. Install dependencies with the build Node active:',
    `  ${bold('npm install')}`,
  ]);
}

try {
  require(resolved);
} catch (err) {
  fail(`${NATIVE_DEP} was compiled for a different Node`, [
    `Running    Node ${process.versions.node}  ${dim('ABI ' + process.versions.modules)}`,
    `Module     ${dim(resolved)}`,
    '',
    'The Node version matches .nvmrc, so the dependencies were most likely',
    'installed under a different one. Rebuild the module with this Node:',
    `  ${bold('npm rebuild ' + NATIVE_DEP)}`,
    '',
    dim(String(err.message).split('\n')[0]),
  ]);
}

console.log(
  dim(
    `  ✓ build Node ${process.versions.node} (ABI ${process.versions.modules}), ${NATIVE_DEP} loads`
  )
);
