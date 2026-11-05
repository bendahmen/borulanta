// A season's file, read from disk for a page built at build time. It is the same file the
// browser fetches, so a lineup on a match page and on Results cannot disagree. The build and
// the dev server both run from site/.
import { readFileSync } from 'node:fs';
import { join } from 'node:path';

const cache = new Map();

export function readSeasonFile(seasonId) {
  if (!cache.has(seasonId)) {
    const path = join(process.cwd(), 'public', 'data', 'seasons', `${seasonId}.json`);
    cache.set(seasonId, JSON.parse(readFileSync(path, 'utf8')));
  }
  return cache.get(seasonId);
}
