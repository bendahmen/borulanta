import assert from 'node:assert/strict';
import { test } from 'node:test';

import { FIRST_RESULTS, FIRST_SCORERS, firstRows, matchDetail, matchHref, needsReveal, pickMatches, revealElement, selectedMatchDate } from '../src/lib/match-data.js';

const matches = [{ date: '2026-10-28' }, { date: '2026-10-21' }, { date: '2026-10-14' }];

test('opens on the newest result', () => {
  assert.equal(selectedMatchDate(matches, null), '2026-10-28');
});

test('a picked row holds while it is in the window', () => {
  assert.equal(selectedMatchDate(matches, '2026-10-14'), '2026-10-14');
});

test('a pick the new window does not hold falls back to the newest', () => {
  assert.equal(selectedMatchDate(matches, '2025-09-10'), '2026-10-28');
});

test('a window with no matches has nothing selected', () => {
  assert.equal(selectedMatchDate([], '2026-10-28'), null);
});

test('the detail is the row and the lineup for that date', () => {
  const lineup = [{ player: 'Boris' }];
  const subset = { matches, match_details: [{ date: '2026-10-21', summary_note: 'note', lineup }] };
  assert.deepEqual(matchDetail(subset, '2026-10-21'), { match: { date: '2026-10-21' }, summaryNote: 'note', lineup });
});

test('no file yet, no date, or a date with no entry has no detail', () => {
  assert.equal(matchDetail(null, '2026-10-21'), null);
  assert.equal(matchDetail({ matches, match_details: [] }, '2026-10-21'), null);
  assert.equal(matchDetail({ matches: [], match_details: [] }, null), null);
});

test('only the parts these pages use are kept in memory', () => {
  const file = { has_matches: true, matches: 1, season_form: 2, match_details: 3, scorers: 4, opponent_record: 5, regression: 6, profiles: 7, attendance_list: 8 };
  assert.deepEqual(Object.keys(pickMatches(file)).sort(), ['has_matches', 'match_details', 'matches', 'opponent_record', 'scorers', 'season_form']);
});

test('the first rows, or all of them', () => {
  assert.deepEqual(firstRows([1, 2, 3], 2, false), [1, 2]);
  assert.deepEqual(firstRows([1, 2, 3], 2, true), [1, 2, 3]);
  assert.deepEqual(firstRows([1], 2, false), [1]);
});

test('Every result shows the 10 rows of a page of match_table()', () => {
  assert.equal(FIRST_RESULTS, 10);
});

test('Scorers shows the 15 rows of a page of scorer_display_table()', () => {
  assert.equal(FIRST_SCORERS, 15);
});

test('a match links to its own page', () => {
  assert.equal(matchHref('2026-10-28'), '/matches/2026-10-28/');
});

test('the detail is brought into view only when it is off the screen', () => {
  assert.equal(needsReveal(-1, 800), true);
  assert.equal(needsReveal(0, 800), false);
  assert.equal(needsReveal(799, 800), false);
  assert.equal(needsReveal(800, 800), true);
});

function fakeReveal({ top, reduced }) {
  const calls = [];
  const element = {
    getBoundingClientRect: () => ({ top }),
    scrollIntoView: (options) => calls.push(options),
  };
  const win = {
    innerHeight: 800,
    matchMedia: (query) => {
      assert.equal(query, '(prefers-reduced-motion: reduce)');
      return { matches: reduced };
    },
  };
  return { calls, element, win };
}

test('bringing it into view glides, unless motion is reduced', () => {
  const moving = fakeReveal({ top: -300, reduced: false });
  revealElement(moving.element, moving.win);
  assert.deepEqual(moving.calls, [{ behavior: 'smooth', block: 'start' }]);

  const still = fakeReveal({ top: -300, reduced: true });
  revealElement(still.element, still.win);
  assert.deepEqual(still.calls, [{ behavior: 'auto', block: 'start' }]);
});

test('a card already on the screen stays where it is', () => {
  const { calls, element, win } = fakeReveal({ top: 40, reduced: false });
  revealElement(element, win);
  assert.deepEqual(calls, []);
});
