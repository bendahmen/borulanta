import assert from 'node:assert/strict';
import { test } from 'node:test';

import { matchDescription, matchPages, matchTitle } from '../src/lib/match-pages.js';

const match = (date, season_id = 's3', extra = {}) => ({ date, season_id, date_label: `Wed ${date}`, result: '2-3', opponent: 'The Dirty Boys', meta_line: 'line', details: [{ label: 'Squad', value: '9 players' }], ...extra });
const season = (...dates) => ({ match_details: dates.map((date) => ({ date, lineup: [{ player: `P${date}` }] })) });
const files = { s3: season('2026-10-28', '2026-10-21', '2026-10-14'), s1: season('2025-09-10') };
const read = (id) => files[id];

test('neighbours are the next match each way, across seasons, and absent at the ends', () => {
  const pages = matchPages([match('2026-10-21'), match('2026-10-28'), match('2025-09-10', 's1')], read);
  assert.deepEqual(pages.map((page) => page.match.date), ['2026-10-28', '2026-10-21', '2025-09-10']);

  const [newest, middle, oldest] = pages;
  assert.equal(newest.next, null);
  assert.equal(newest.previous.date, '2026-10-21');
  assert.equal(middle.next.date, '2026-10-28');
  assert.equal(middle.previous.date, '2025-09-10');
  assert.equal(oldest.previous, null);
  assert.equal(oldest.next.date, '2026-10-21');
});

test('one match has no neighbours', () => {
  const [only] = matchPages([match('2026-10-28')], read);
  assert.equal(only.next, null);
  assert.equal(only.previous, null);
});

test('a lineup comes from the file of that match own season', () => {
  const asked = [];
  const pages = matchPages([match('2026-10-28'), match('2025-09-10', 's1')], (id) => {
    asked.push(id);
    return files[id];
  });
  assert.deepEqual(asked, ['s3', 's1']);
  assert.deepEqual(pages[1].lineup, [{ player: 'P2025-09-10' }]);
});

test('two matches on one date fail the build', () => {
  assert.throws(() => matchPages([match('2026-10-28'), match('2026-10-28')], read), /2026-10-28/);
});

test('a match with no lineup entry fails the build, and names it', () => {
  assert.throws(() => matchPages([match('2026-10-07')], read), /2026-10-07.*s3\.json/);
});

test('a lineup entry with nobody in it is a match waiting for its attendance, not a fault', () => {
  const waiting = { s3: { match_details: [{ date: '2026-11-04', lineup: [] }] } };
  const [page] = matchPages([match('2026-11-04')], (id) => waiting[id]);
  assert.deepEqual(page.lineup, []);
});

test('two people who would share an address fail the build, guests included', () => {
  const clash = { s3: { match_details: [{ date: '2026-10-28', lineup: [{ player: 'Jo Ann' }, { player: 'jo-ann' }] }] } };
  assert.throws(() => matchPages([match('2026-10-28')], (id) => clash[id]), /Jo Ann/);
  const guest = { s3: { match_details: [{ date: '2026-10-28', lineup: [{ player: 'Jo Ann' }] }] } };
  assert.throws(() => matchPages([match('2026-10-28')], (id) => guest[id], ['jo-ann']), /jo-ann/);
});

test('a name that would take a page of the site fails the build', () => {
  const clash = { s3: { match_details: [{ date: '2026-10-28', lineup: [{ player: 'Roster' }] }] } };
  assert.throws(() => matchPages([match('2026-10-28')], (id) => clash[id]), /roster/);
});

test('the title says who played, or when where the opponent is not on file', () => {
  assert.equal(matchTitle(match('2026-10-28'), 'Borulanta'), 'Borulanta 2-3 The Dirty Boys');
  assert.equal(matchTitle(match('2025-09-10', 's1', { opponent: null, date_label: 'Wed 10 Sep 2025' }), 'Borulanta'), 'Borulanta 2-3, Wed 10 Sep 2025');
});

test('the description is R line and the squad, and it names nobody', () => {
  const played = match('2026-10-28', 's3', {
    meta_line: 'Wed 28 Oct 2026 · v The Dirty Boys · Season 3',
    details: [
      { label: 'Squad', value: '9 players' },
      { label: 'Scorers', value: 'Emiliano (2)' },
      { label: 'Man of the match', value: 'Emiliano' },
    ],
  });
  assert.equal(matchDescription(played), 'Wed 28 Oct 2026 · v The Dirty Boys · Season 3 · 9 players');
  assert.equal(matchDescription(match('2026-10-28', 's3', { details: [] })), 'line');
});
