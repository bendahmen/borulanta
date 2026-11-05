import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { test } from 'node:test';

import { playerSlug, playerSlugs } from '../src/lib/slug.js';

test('a slug is lower case, plain and made only of letters, digits and hyphens', () => {
  assert.equal(playerSlug('Ben'), 'ben');
  assert.equal(playerSlug('Jean-Luc'), 'jean-luc');
  assert.equal(playerSlug('  Mary  Ann '), 'mary-ann');
  assert.equal(playerSlug("O'Brien"), 'o-brien');
  assert.equal(playerSlug('José'), 'jose');
  assert.equal(playerSlug('Müller 2'), 'muller-2');
});

test('a name with nothing to make an address from fails the build', () => {
  assert.throws(() => playerSlug('???'), /no letters or digits/);
  assert.throws(() => playerSlug(''), /no letters or digits/);
});

test('a slug depends on the name alone, not on who else is on the list', () => {
  assert.equal(playerSlugs(['Ben']).get('Ben'), playerSlugs(['Anton', 'Ben', 'Yelong']).get('Ben'));
});

test('two players on one address fail the build, and the message names them', () => {
  assert.throws(() => playerSlugs(['Ben', 'Anton', 'BEN']), /"Ben" and "BEN".*"ben"/);
  assert.throws(() => playerSlugs(['José', 'Jose']), /"José" and "Jose".*"jose"/);
  assert.throws(() => playerSlugs(['Mary Ann', 'Mary-Ann']), /share the address "mary-ann"/);
});

test('the same name twice is one player, not a collision', () => {
  assert.deepEqual([...playerSlugs(['Ben', 'Ben'])], [['Ben', 'ben']]);
});

test('a player cannot take the address of a page under /players/', () => {
  assert.throws(() => playerSlugs(['Roster']), /roster page/);
  assert.throws(() => playerSlugs(['effects']), /effects page/);
});

test('the real players all have an address of their own', () => {
  const fees = JSON.parse(readFileSync(new URL('../src/generated/fees.json', import.meta.url)));
  const names = fees.overviews.map((overview) => overview.player);
  const slugs = playerSlugs(names);
  assert.equal(new Set(slugs.values()).size, names.length);
  for (const slug of slugs.values()) assert.match(slug, /^[a-z0-9]+(-[a-z0-9]+)*$/);
});
