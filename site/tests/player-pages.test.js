import assert from 'node:assert/strict';
import { readdirSync, readFileSync } from 'node:fs';
import { test } from 'node:test';

import { PLAYERS } from '../src/lib/labels.js';
import { playerPages } from '../src/lib/player-pages.js';
import { playerSlug } from '../src/lib/slug.js';

const files = {
  s1: { profiles: [{ player: 'Anton' }, { player: 'Ben' }] },
  s2: { profiles: [{ player: 'Ben' }, { player: 'Émile' }] },
};
const read = (id) => files[id];

test('a page for everyone with a profile in any file, once each', () => {
  assert.deepEqual(playerPages(['s1', 's2'], read), [
    { player: 'Anton', slug: 'anton' },
    { player: 'Ben', slug: 'ben' },
    { player: 'Émile', slug: 'emile' },
  ]);
});

test('a guest with no profile has no page, and their name still counts against a clash', () => {
  const pages = playerPages(['s1'], read, ['Akash']);
  assert.ok(!pages.some((page) => page.player === 'Akash'));
  assert.throws(() => playerPages(['s1'], read, ['BEN']), /"Ben" and "BEN"/);
});

test('a clash between two profiles fails the build, and names them', () => {
  const clash = { s1: { profiles: [{ player: 'Jo Ann' }] }, s2: { profiles: [{ player: 'jo-ann' }] } };
  assert.throws(() => playerPages(['s1', 's2'], (id) => clash[id]), /"Jo Ann" and "jo-ann"/);
});

test('a player cannot take the Roster or Effects address', () => {
  assert.throws(() => playerPages(['s1'], () => ({ profiles: [{ player: 'Roster' }] })), /roster page/);
  assert.throws(() => playerPages(['s1'], () => ({ profiles: [{ player: 'Effects' }] })), /effects page/);
});

test('a description names the player and states no number', () => {
  const description = PLAYERS.description('Boris');
  assert.ok(description.includes('Boris'));
  assert.ok(!/\d/.test(description));
});

// ---- The real files ----

const seasonsFolder = new URL('../public/data/seasons/', import.meta.url);
const ids = readdirSync(seasonsFolder).map((name) => name.replace(/\.json$/, ''));
const readReal = (id) => JSON.parse(readFileSync(new URL(`${id}.json`, seasonsFolder)));

test('every name in every lineup has a page to link to', () => {
  const pages = new Set(playerPages(ids, readReal).map((page) => page.slug));
  let names = 0;
  for (const id of ids) {
    for (const detail of readReal(id).match_details) {
      for (const row of detail.lineup) {
        assert.ok(pages.has(playerSlug(row.player)), `${row.player} on ${detail.date}`);
        names += 1;
      }
    }
  }
  assert.ok(names > 100);
});

test('the guests who only scored have no page', () => {
  const pages = playerPages(ids, readReal).map((page) => page.player);
  assert.ok(!pages.includes('Akash'));
  assert.ok(!pages.includes('Ameen'));
  assert.ok(pages.includes('Boris'));
});
