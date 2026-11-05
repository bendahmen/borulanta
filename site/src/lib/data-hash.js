// The stale-deploy check. A CSV commit without regenerated JSON would silently
// publish old numbers, so the build recomputes data_hash and fails on a mismatch.
// scripts/build-site-data.R defines the hash. This has to agree with it byte for
// byte and uses only node:fs and node:crypto, so it runs on the build host.

import { createHash } from 'node:crypto';
import { readdirSync, readFileSync } from 'node:fs';
import { join } from 'node:path';

const sha256 = (bytes) => createHash('sha256').update(bytes).digest('hex');

// Visible files only, as R's list.files() gives them, so .~lock.foo.csv from a
// spreadsheet left open does not fail the build.
function visibleCsvs(folder) {
  return readdirSync(folder, { withFileTypes: true })
    .filter((entry) => entry.isFile() && entry.name.endsWith('.csv') && !entry.name.startsWith('.'))
    .map((entry) => entry.name);
}

export function computeDataHash(repoRoot) {
  const paths = [
    ...visibleCsvs(join(repoRoot, 'data')).map((name) => `data/${name}`),
    ...visibleCsvs(join(repoRoot, 'data', 'archive')).map((name) => `data/archive/${name}`),
  ];
  // Bytewise, as R's sort(method = "radix"). A plain sort() compares UTF-16 units.
  paths.sort((a, b) => Buffer.compare(Buffer.from(a), Buffer.from(b)));

  const manifest = paths
    .map((path) => `${path}\n${sha256(readFileSync(join(repoRoot, path)))}\n`)
    .join('');
  return sha256(Buffer.from(manifest, 'utf8'));
}

// Throws when the exported JSON does not describe the CSVs on disk.
export function checkDataHash({ repoRoot, metaPath }) {
  const exported = JSON.parse(readFileSync(metaPath, 'utf8')).data_hash;
  const actual = computeDataHash(repoRoot);
  if (exported !== actual) {
    throw new Error(
      'The JSON in site/src/generated does not match the CSVs in data/ ' +
        `(JSON ${exported}, CSVs ${actual}). Run Rscript scripts/build-site-data.R and commit the result.`,
    );
  }
}
