import assert from 'node:assert/strict';
import { readdirSync, readFileSync } from 'node:fs';
import { test } from 'node:test';

import { needsReveal, revealElement } from '../src/lib/match-data.js';
import { FIRST_ROSTER, ROSTER_SORT, findProfile, pickPlayers, playerPageView, profileNeedsReveal, selectedPlayer, shownRoster, stripModel, topThree, withWithoutRows } from '../src/lib/player-data.js';
import { createSeasonScope, scopeView } from '../src/lib/season-scope.js';
import { sortOnField } from '../src/lib/table-sort.js';

const roster = (...names) => names.map((player) => ({ player }));
const table = roster('Boris', 'Vitto', 'Ben', 'Mateus');

test('it opens on the stored player where they turned out in the window', () => {
  assert.equal(selectedPlayer(table, null, 'Ben'), 'Ben');
});

test('it opens on the top of the table where the stored player did not turn out', () => {
  assert.equal(selectedPlayer(table, null, 'Akash'), 'Boris');
});

test('it opens on the top of the table with nobody stored, or with the placeholder stored', () => {
  assert.equal(selectedPlayer(table, null, null), 'Boris');
  assert.equal(selectedPlayer(table, null, ''), 'Boris');
});

test('a pick beats the stored player, who stays where they are', () => {
  assert.equal(selectedPlayer(table, 'Vitto', 'Ben'), 'Vitto');
});

test('a tick change that drops the picked player falls back the same way as arriving', () => {
  const window = roster('Vitto', 'Ben');
  assert.equal(selectedPlayer(window, 'Boris', 'Ben'), 'Ben');
  assert.equal(selectedPlayer(window, 'Boris', 'Akash'), 'Vitto');
  assert.equal(selectedPlayer(window, 'Boris', null), 'Vitto');
});

test('a tick change that keeps the picked player stays on them', () => {
  assert.equal(selectedPlayer(roster('Vitto', 'Boris'), 'Boris', 'Vitto'), 'Boris');
});

test('ticking the season back brings the picked player back', () => {
  assert.equal(selectedPlayer(table, 'Mateus', 'Ben'), 'Mateus');
  assert.equal(selectedPlayer(roster('Boris', 'Ben'), 'Mateus', 'Ben'), 'Ben');
  assert.equal(selectedPlayer(table, 'Mateus', 'Ben'), 'Mateus');
});

test('a window with nobody in it selects nobody', () => {
  assert.equal(selectedPlayer([], 'Boris', 'Ben'), null);
  assert.equal(findProfile({ profiles: [] }, null), null);
});

test('the top three come from R order and stay on the same people through a sort', () => {
  const rows = [
    { player: 'Boris', attendance_rate: 0.8, avg_points: 1 },
    { player: 'Vitto', attendance_rate: 0.7, avg_points: 2 },
    { player: 'Ben', attendance_rate: 0.7, avg_points: 3 },
    { player: 'Mateus', attendance_rate: 0.7, avg_points: 4 },
  ];
  const top = topThree(rows);
  assert.deepEqual([...top], ['Boris', 'Vitto', 'Ben']);
  const sorted = sortOnField(rows, { key: 'avg_points', direction: 'descending' });
  assert.equal(sorted[0].player, 'Mateus');
  assert.equal(top.has('Mateus'), false);
});

test('a table of two has two in its top three', () => {
  assert.equal(topThree(roster('A', 'B')).size, 2);
});

test('ties on Turnout stay in R order, in both directions', () => {
  const rows = [{ player: 'Z', attendance_rate: 0.5 }, { player: 'A', attendance_rate: 0.5 }, { player: 'M', attendance_rate: 0.9 }];
  assert.deepEqual(sortOnField(rows, ROSTER_SORT).map((row) => row.player), ['M', 'Z', 'A']);
  assert.deepEqual(sortOnField(rows, { ...ROSTER_SORT, direction: 'ascending' }).map((row) => row.player), ['Z', 'A', 'M']);
});

const many = roster(...Array.from({ length: 20 }, (_, index) => `P${index}`));

test('the table shows as many as attendance_table() pages at, until asked for all', () => {
  assert.equal(FIRST_ROSTER, 15);
  assert.equal(shownRoster(many, 'P0', false).length, 15);
  assert.equal(shownRoster(many, 'P0', true).length, 20);
});

test('a selected player past the first rows keeps their row, after those', () => {
  const shown = shownRoster(many, 'P17', false);
  assert.equal(shown.length, 16);
  assert.equal(shown.at(-1).player, 'P17');
  assert.equal(shown[14].player, 'P14');
  assert.equal(shownRoster(many, 'P17', true).length, 20);
});

test('a short table is all there, and no selection adds nothing', () => {
  assert.equal(shownRoster(roster('A', 'B'), 'B', false).length, 2);
  assert.equal(shownRoster(many, null, false).length, 15);
  assert.deepEqual(shownRoster([], null, false), []);
});

const window = { has_matches: true, profiles: [{ player: 'Boris' }] };
const state = (changes) => ({ ticks: ['s1'], failed: false, subset: window, ...changes });

test('a player page shows them where they turned out', () => {
  assert.equal(playerPageView(state(), 'Boris').view, 'ready');
});

test('a player page shows that player, not the first profile in the file', () => {
  const subset = { has_matches: true, profiles: [{ player: 'Anton', appearances: 1 }, { player: 'Boris', appearances: 2 }, { player: 'Oleg', appearances: 3 }] };
  assert.equal(playerPageView(state({ subset }), 'Oleg').profile.appearances, 3);
  assert.equal(playerPageView(state({ subset }), 'Boris').profile.appearances, 2);
});

test('a player page says so where they did not turn out, and shows nobody else', () => {
  const page = playerPageView(state(), 'Oleg');
  assert.equal(page.view, 'did-not-play');
  assert.equal(page.profile, null);
});

test('a player page with zero ticks or no matches has the empty state, not the other message', () => {
  assert.equal(playerPageView(state({ ticks: [], subset: null }), 'Boris').view, 'no-matches');
  assert.equal(playerPageView(state({ subset: { has_matches: false, profiles: [] } }), 'Boris').view, 'no-matches');
});

test('a player page waits for the file, and says when it failed', () => {
  assert.equal(playerPageView(state({ subset: null }), 'Boris').view, 'loading');
  assert.equal(playerPageView(state({ failed: true }), 'Boris').view, 'failed');
});

test('matches with nobody on file yet read as nobody having turned out', () => {
  assert.equal(playerPageView(state({ subset: { has_matches: true, profiles: [] } }), 'Boris').view, 'did-not-play');
});

test('pickPlayers keeps only what these pages use', () => {
  const kept = pickPlayers({ has_matches: true, matches: [], attendance_list: [], profiles: [], regression: {}, scorers: {} });
  assert.deepEqual(Object.keys(kept).sort(), ['attendance_list', 'has_matches', 'matches', 'profiles']);
});

const sides = { played: 'Played', missed: 'Missed' };

test('with and without is two rows, played first, each from its own side', () => {
  const rows = withWithoutRows({ present: { played: 7, points_per_match_rounded: 1.5 }, absent: { played: 2, points_per_match_rounded: 0.5 } }, sides);
  assert.deepEqual(rows, [
    { side: 'Played', played: 7, points_per_match_rounded: 1.5 },
    { side: 'Missed', played: 2, points_per_match_rounded: 0.5 },
  ]);
});

const keyWords = { played: 'Played', missed: 'Missed' };

test('the strip keeps R order and says which squares were played', () => {
  const timeline = [{ date: '2026-01-01', points: 3, played: true }, { date: '2026-01-08', points: 0, played: false }, { date: '2026-01-15', points: 1, played: true }];
  const matches = [{ date: '2026-01-15', date_short: '15 Jan' }, { date: '2026-01-08', date_short: '8 Jan' }, { date: '2026-01-01', date_short: '1 Jan' }];
  const strip = stripModel(timeline, matches, keyWords);
  assert.deepEqual(strip.squares, [
    { date: '2026-01-01', played: true, dateShort: '1 Jan', label: '1 Jan: Played' },
    { date: '2026-01-08', played: false, dateShort: '8 Jan', label: '8 Jan: Missed' },
    { date: '2026-01-15', played: true, dateShort: '15 Jan', label: '15 Jan: Played' },
  ]);
  assert.equal(strip.first, '1 Jan');
  assert.equal(strip.last, '15 Jan');
  assert.deepEqual(strip.playedDates, ['1 Jan', '15 Jan']);
  assert.deepEqual(strip.missedDates, ['8 Jan']);
});

test('an empty strip has no dates', () => {
  assert.deepEqual(stripModel([], [], keyWords), { squares: [], first: null, last: null, playedDates: [], missedDates: [] });
});

test('a profile well down the screen, or above it, is brought up', () => {
  assert.equal(profileNeedsReveal(-1, 800), true);
  assert.equal(profileNeedsReveal(700, 800), true);
  assert.equal(profileNeedsReveal(2000, 800), true);
});

test('a profile with most of the screen left for it stays put', () => {
  assert.equal(profileNeedsReveal(0, 800), false);
  assert.equal(profileNeedsReveal(480, 800), false);
});

const fakeTarget = (top) => ({
  scrolled: null,
  getBoundingClientRect: () => ({ top }),
  scrollIntoView(options) {
    this.scrolled = options;
  },
});
const fakeWin = (reduced) => ({ innerHeight: 800, matchMedia: () => ({ matches: reduced }) });

test('a pick scrolls smoothly, or not at all smoothly with reduced motion', () => {
  const smooth = fakeTarget(700);
  revealElement(smooth, fakeWin(false), profileNeedsReveal);
  assert.deepEqual(smooth.scrolled, { behavior: 'smooth', block: 'start' });

  const still = fakeTarget(700);
  revealElement(still, fakeWin(true), profileNeedsReveal);
  assert.deepEqual(still.scrolled, { behavior: 'auto', block: 'start' });
});

test('a pick leaves a profile already in view alone, and Results keeps its own rule', () => {
  const inView = fakeTarget(300);
  revealElement(inView, fakeWin(false), profileNeedsReveal);
  assert.equal(inView.scrolled, null);

  const peeking = fakeTarget(700);
  assert.equal(needsReveal(700, 800), false);
  revealElement(peeking, fakeWin(false));
  assert.equal(peeking.scrolled, null);
});

// The opening position and the fallback together, through the same scope the page uses.
test('from arriving, through a pick and every tick change, the selection follows the window', async () => {
  const seasonFile = (...names) => ({ has_matches: true, matches: [], attendance_list: roster(...names), profiles: roster(...names) });
  const files = { '/data/seasons/s1-s2.json': seasonFile('Boris', 'Anton', 'Ben'), '/data/seasons/s2.json': seasonFile('Boris', 'Ben') };
  const asked = [];
  const win = {
    location: { search: '', hash: '' },
    localStorage: { getItem: () => null, setItem() {} },
    history: {
      state: null,
      replaceState(_state, _title, url) {
        win.location.search = url;
      },
    },
  };
  let state;
  const scope = createSeasonScope({
    knownIds: ['s2', 's1'],
    pick: pickPlayers,
    fetchFile: async (file) => {
      asked.push(file);
      return { ok: true, status: 200, json: async () => files[file] };
    },
    onchange: (next) => {
      state = next;
    },
    win,
  });
  const selected = (picked, stored) => selectedPlayer(state.subset?.attendance_list ?? [], picked, stored);

  await scope.start(null);
  assert.equal(selected(null, 'Ben'), 'Ben');
  assert.equal(selected('Anton', 'Ben'), 'Anton');

  await scope.changeTick('s1', false);
  assert.equal(selected('Anton', 'Ben'), 'Ben');
  assert.equal(selected('Anton', null), 'Boris');

  const before = asked.length;
  await scope.changeTick('s2', false);
  assert.equal(asked.length, before);
  assert.equal(scopeView(state), 'no-matches');
  assert.equal(selected('Anton', 'Ben'), null);

  await scope.changeTick('s1', true);
  await scope.changeTick('s2', true);
  assert.equal(selected('Anton', 'Ben'), 'Anton');
});

// ---- The real files ----

const seasonsFolder = new URL('../public/data/seasons/', import.meta.url);
const files = readdirSync(seasonsFolder).map((name) => JSON.parse(readFileSync(new URL(name, seasonsFolder))));

test('every profile strip agrees with its headline, in date order, in all seven files', () => {
  assert.equal(files.length, 7);
  let checked = 0;
  for (const file of files) {
    for (const profile of file.profiles) {
      const played = profile.timeline.filter((square) => square.played).length;
      assert.equal(played, profile.appearances, profile.player);
      assert.equal(profile.timeline.length, profile.matches, profile.player);
      assert.equal(profile.headline[0].value, `${played} of ${profile.timeline.length}`, profile.player);
      const dates = profile.timeline.map((square) => square.date);
      assert.deepEqual(dates, [...dates].sort(), profile.player);
      const strip = stripModel(profile.timeline, file.matches, keyWords);
      assert.ok(strip.squares.every((square) => square.dateShort !== square.date), profile.player);
      assert.equal(strip.playedDates.length, profile.appearances, profile.player);
      assert.equal(strip.missedDates.length, profile.absent.played, profile.player);
      checked += 1;
    }
  }
  assert.ok(checked > 100);
});

test('every side with no matches has no rate, and only those', () => {
  for (const file of files) {
    for (const profile of file.profiles) {
      for (const side of [profile.present, profile.absent]) {
        assert.equal(side.points_per_match_rounded === null, side.played === 0, profile.player);
      }
    }
  }
});

test('everyone in the table has a profile, and nobody else does', () => {
  for (const file of files) {
    const inTable = file.attendance_list.map((row) => row.player).sort();
    assert.deepEqual(file.profiles.map((profile) => profile.player).sort(), inTable);
  }
});
