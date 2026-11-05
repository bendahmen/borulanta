import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { mkdirSync, mkdtempSync, readFileSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { after, test } from 'node:test';
import { fileURLToPath } from 'node:url';

import { checkDataHash, computeDataHash } from '../src/lib/data-hash.js';

const sha256 = (text) => createHash('sha256').update(text).digest('hex');
const made = [];
after(() => made.forEach((folder) => rmSync(folder, { recursive: true, force: true })));

// A throwaway repo. The real data/ is only ever read.
function fixtureRepo() {
  const root = mkdtempSync(join(tmpdir(), 'borulanta-hash-'));
  made.push(root);
  mkdirSync(join(root, 'data', 'archive'), { recursive: true });
  writeFileSync(join(root, 'data', 'a.csv'), 'date,x\n01/01/2026,1\n');
  writeFileSync(join(root, 'data', 'B.csv'), 'y\n2\n');
  writeFileSync(join(root, 'data', 'archive', 'charges_s1.csv'), 'z\n3\n');
  return root;
}

function writeMeta(root, hash) {
  const metaPath = join(root, 'meta.json');
  writeFileSync(metaPath, JSON.stringify({ data_hash: hash }));
  return metaPath;
}

test('follows the definition in scripts/build-site-data.R', () => {
  const root = fixtureRepo();
  // Bytewise order puts capitals first, and the archive after data/a.csv.
  const manifest =
    `data/B.csv\n${sha256('y\n2\n')}\n` +
    `data/a.csv\n${sha256('date,x\n01/01/2026,1\n')}\n` +
    `data/archive/charges_s1.csv\n${sha256('z\n3\n')}\n`;
  assert.equal(computeDataHash(root), sha256(manifest));
});

test('agrees with R on a known input', () => {
  // Produced by data_hash() in scripts/build-site-data.R on this same fixture.
  assert.equal(
    computeDataHash(fixtureRepo()),
    'ff96010be40717ef1abe4acc95ab960ac4e1d5b6ab3d21e1059520f6cc2fbf12',
  );
});

test('ignores hidden files, other extensions and subfolders', () => {
  const root = fixtureRepo();
  const before = computeDataHash(root);
  writeFileSync(join(root, 'data', '.~lock.a.csv'), 'x');
  writeFileSync(join(root, 'data', 'notes.txt'), 'x');
  mkdirSync(join(root, 'data', 'folder.csv'));
  assert.equal(computeDataHash(root), before);
});

test('a match passes', () => {
  const root = fixtureRepo();
  const metaPath = writeMeta(root, computeDataHash(root));
  assert.doesNotThrow(() => checkDataHash({ repoRoot: root, metaPath }));
});

test('a changed CSV throws and says what to run', () => {
  const root = fixtureRepo();
  const metaPath = writeMeta(root, computeDataHash(root));
  writeFileSync(join(root, 'data', 'a.csv'), 'date,x\n01/01/2026,2\n');
  assert.throws(() => checkDataHash({ repoRoot: root, metaPath }), /Rscript scripts\/build-site-data\.R/);
});

test('a changed archive ledger throws too', () => {
  const root = fixtureRepo();
  const metaPath = writeMeta(root, computeDataHash(root));
  writeFileSync(join(root, 'data', 'archive', 'charges_s1.csv'), 'z\n4\n');
  assert.throws(() => checkDataHash({ repoRoot: root, metaPath }));
});

test('a new CSV throws', () => {
  const root = fixtureRepo();
  const metaPath = writeMeta(root, computeDataHash(root));
  writeFileSync(join(root, 'data', 'new.csv'), 'n\n');
  assert.throws(() => checkDataHash({ repoRoot: root, metaPath }));
});

test('the committed JSON matches the committed CSVs', () => {
  const repoRoot = fileURLToPath(new URL('../..', import.meta.url));
  const metaPath = fileURLToPath(new URL('../src/generated/meta.json', import.meta.url));
  assert.equal(computeDataHash(repoRoot), JSON.parse(readFileSync(metaPath)).data_hash);
});
