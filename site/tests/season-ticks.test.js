import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { test } from 'node:test';

import { moduleToInline } from '../src/lib/inline.js';
import {
  cleanTicks,
  formatTicks,
  parseTicks,
  readStoredTicks,
  resolveTicks,
  searchWithTicks,
  subsetFile,
  ticksFromSearch,
  writeStoredTicks,
} from '../src/lib/season-ticks.js';

// Newest first, as meta.seasons gives them.
const known = ['s3', 's2', 's1'];

const storageWith = (value) => () => ({
  getItem: () => value,
  setItem() {},
});

const blocked = () => {
  throw new Error('SecurityError');
};

test('parsing keeps known ids, once each, in sort order', () => {
  assert.deepEqual(parseTicks('s3,s1', known), ['s1', 's3']);
  assert.deepEqual(parseTicks('s1,s1,s3,s1', known), ['s1', 's3']);
  assert.deepEqual(parseTicks('s9,s1,nonsense', known), ['s1']);
  assert.deepEqual(parseTicks(' s2 , s1 ', known), ['s1', 's2']);
});

test('parsing nothing gives no ticks', () => {
  assert.deepEqual(parseTicks('', known), []);
  assert.deepEqual(parseTicks(',,', known), []);
  assert.deepEqual(parseTicks('s9', known), []);
});

test('writing then parsing gives the same ticks', () => {
  const ticks = cleanTicks(['s3', 's1'], known);
  assert.equal(formatTicks(ticks), 's1,s3');
  assert.deepEqual(parseTicks(formatTicks(ticks), known), ticks);
  assert.equal(formatTicks([]), '');
});

test('the URL says nothing without the param, and none ticked with an empty one', () => {
  assert.equal(ticksFromSearch('', known), null);
  assert.equal(ticksFromSearch('?other=1', known), null);
  assert.deepEqual(ticksFromSearch('?seasons=', known), []);
  assert.deepEqual(ticksFromSearch('?seasons', known), []);
  assert.deepEqual(ticksFromSearch('?seasons=s1,s3', known), ['s1', 's3']);
  // A browser or a chat app may have encoded the comma.
  assert.deepEqual(ticksFromSearch('?seasons=s1%2Cs3', known), ['s1', 's3']);
});

test('only unknown ids in the URL mean none ticked, as the URL wins', () => {
  assert.deepEqual(ticksFromSearch('?seasons=s9', known), []);
  assert.deepEqual(resolveTicks('?seasons=s9', storageWith('s2'), known), []);
});

test('the URL wins, then storage, then all', () => {
  assert.deepEqual(resolveTicks('?seasons=s1', storageWith('s2'), known), ['s1']);
  assert.deepEqual(resolveTicks('?seasons=', storageWith('s2'), known), []);
  assert.deepEqual(resolveTicks('', storageWith('s2,s3'), known), ['s2', 's3']);
  assert.deepEqual(resolveTicks('', storageWith(null), known), ['s1', 's2', 's3']);
});

test('nothing usable in storage falls through to all', () => {
  assert.deepEqual(resolveTicks('', storageWith(''), known), ['s1', 's2', 's3']);
  assert.deepEqual(resolveTicks('', storageWith('s9'), known), ['s1', 's2', 's3']);
  assert.equal(readStoredTicks(storageWith('s9'), known), null);
});

test('blocked storage reads as nothing and never throws', () => {
  assert.equal(readStoredTicks(blocked, known), null);
  assert.doesNotThrow(() => writeStoredTicks(blocked, ['s1']));
  assert.deepEqual(resolveTicks('', blocked, known), ['s1', 's2', 's3']);

  const throwing = () => ({
    getItem() {
      throw new Error('denied');
    },
    setItem() {
      throw new Error('quota');
    },
  });
  assert.equal(readStoredTicks(throwing, known), null);
  assert.doesNotThrow(() => writeStoredTicks(throwing, ['s1']));
});

test('storage holds what the URL holds', () => {
  let saved = null;
  const storage = () => ({
    setItem(key, value) {
      saved = [key, value];
    },
  });
  writeStoredTicks(storage, ['s1', 's3']);
  assert.deepEqual(saved, ['borulanta.seasons', 's1,s3']);
});

test('the URL keeps its commas and any other params', () => {
  assert.equal(searchWithTicks('', ['s1', 's3']), '?seasons=s1,s3');
  assert.equal(searchWithTicks('', []), '?seasons=');
  assert.equal(searchWithTicks('?seasons=s2', ['s1']), '?seasons=s1');
  assert.equal(searchWithTicks('?utm=x&seasons=s2&b=1', ['s1']), '?utm=x&b=1&seasons=s1');
});

test('the file name is the sorted ids', () => {
  assert.equal(subsetFile(['s1']), '/data/seasons/s1.json');
  assert.equal(subsetFile(['s1', 's2', 's3']), '/data/seasons/s1-s2-s3.json');
  assert.equal(subsetFile(['s3', 's1']), '/data/seasons/s1-s3.json');
  assert.equal(subsetFile(['s2', 's3', 's1']), '/data/seasons/s1-s2-s3.json');
});

test('no ticks has no file', () => {
  assert.equal(subsetFile([]), null);
});

test('the sort matches the C locale, where s10 comes before s2', () => {
  assert.equal(subsetFile(['s2', 's10']), '/data/seasons/s10-s2.json');
});

test('every subset file the export wrote can be reached from ticks', async () => {
  const { default: meta } = await import('../src/generated/meta.json', { with: { type: 'json' } });
  const ids = meta.seasons.map((season) => season.season_id);
  const subsets = [];
  for (let mask = 1; mask < 1 << ids.length; mask += 1) {
    subsets.push(ids.filter((_, bit) => mask & (1 << bit)));
  }
  const reached = subsets.map((ticks) => subsetFile(ticks).replace('/data/seasons/', '')).sort();
  assert.deepEqual(reached, [...meta.subset_files].sort());
});

test('runs as a classic inline script, as the page uses it', () => {
  const source = readFileSync(new URL('../src/lib/season-ticks.js', import.meta.url), 'utf8');
  const inline = moduleToInline(source);
  assert.ok(!/^export /m.test(inline));
  const run = new Function(`${inline}\nreturn subsetFile(resolveTicks('?seasons=s3,s1', function () { return null; }, ['s3', 's2', 's1']));`);
  assert.equal(run(), '/data/seasons/s1-s3.json');
});
