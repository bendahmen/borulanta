import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { test } from 'node:test';

import { cellHtml, tableHtml } from '../src/lib/data-table.js';
import { PLAYERS } from '../src/lib/labels.js';
import { withWithoutRows } from '../src/lib/player-data.js';
import { rosterColumns, withWithoutColumns } from '../src/lib/player-tables.js';

const { players_cards: cards } = JSON.parse(readFileSync(new URL('../src/generated/wording.json', import.meta.url)));
const labels = (columns) => columns.map((column) => column.label);
const shownBy = (columns, row) => Object.fromEntries(columns.map((column) => [column.key, column.value(row)]));

const roster = rosterColumns(cards.who_turns_up.headings, PLAYERS, new Set(['Boris']));
const withWithout = withWithoutColumns(cards.with_without.headings, PLAYERS);

test('the headings and their order are R tables', () => {
  assert.deepEqual(labels(roster).slice(0, 5), ['Player', 'Turnout', 'Avg points', 'Avg goals scored', 'Avg goals conceded']);
  assert.deepEqual(labels(withWithout).slice(1), ['P', 'W', 'D', 'L', 'GF', 'GA', 'Pts/match']);
});

test('each column of Who turns up reads its own field', () => {
  const row = { player: 'name', attendance_rate: 0.5, avg_points: 1, avg_goals_scored: 2, avg_goals_conceded: 3 };
  assert.deepEqual(shownBy(roster, row), {
    player: 'name',
    attendance_rate: '50%',
    avg_points: 1,
    avg_goals_scored: 2,
    avg_goals_conceded: 3,
    page: 'name',
  });
});

test('each sortable column keys on the field it shows, so a sort means what the heading says', () => {
  const row = { player: 'name', attendance_rate: 0.5, avg_points: 1, avg_goals_scored: 2, avg_goals_conceded: 3 };
  for (const column of roster.filter((candidate) => !candidate.unsorted)) {
    assert.ok(column.key in row, column.key);
  }
  assert.deepEqual(roster.filter((column) => column.unsorted).map((column) => column.key), ['page']);
});

test('each column of With and without reads its own field, and the rate is the rounded one', () => {
  const row = { side: 'word', played: 1, won: 2, drawn: 3, lost: 4, goals_for: 5, goals_against: 6, points_per_match: 7, points_per_match_rounded: 8 };
  assert.deepEqual(shownBy(withWithout, row), {
    side: 'word',
    played: 1,
    won: 2,
    drawn: 3,
    lost: 4,
    goals_for: 5,
    goals_against: 6,
    points_per_match: 8,
  });
});

test('Turnout rounds as DT does and the averages are not rounded again', () => {
  const row = { player: 'Oleg', attendance_rate: 0.125, avg_points: 1.5, avg_goals_scored: 4.61, avg_goals_conceded: 2 };
  const shown = shownBy(roster, row);
  assert.equal(shown.attendance_rate, '13%');
  assert.equal(shown.avg_points, 1.5);
  assert.equal(shown.avg_goals_scored, 4.61);
  assert.equal(shown.avg_goals_conceded, 2);
});

test('on a phone the goals fold, and Turnout and Avg points do not', () => {
  assert.deepEqual(roster.filter((column) => column.folds).map((column) => column.key), ['avg_goals_scored', 'avg_goals_conceded']);
});

test('the top three carry a word as well as the tint', () => {
  const [lead] = roster;
  assert.equal(lead.cue({ player: 'Boris' }), PLAYERS.topThree);
  assert.equal(lead.cue({ player: 'Oleg' }), '');
});

test('a row links to its player page, and says whose in words', () => {
  const html = roster.at(-1).html({ player: 'Émile Zola' });
  assert.ok(html.includes('href="/players/emile-zola/"'));
  assert.ok(html.includes('aria-label="Player page for Émile Zola"'));
  assert.equal(roster.at(-1).hideLabel, true);
});

test('nothing in a row can write markup', () => {
  const html = roster.at(-1).html({ player: '"><script>' });
  assert.ok(!html.includes('<script>'));
});

const emptySide = { played: 0, won: 0, drawn: 0, lost: 0, goals_for: 0, goals_against: 0, points_per_match: null, points_per_match_rounded: null };
const fullSide = { played: 7, won: 3, drawn: 1, lost: 3, goals_for: 20, goals_against: 18, points_per_match: 1.4285714, points_per_match_rounded: 1.43 };

test('an empty side shows a blank rate, not 0 or NaN, and its counts stay', () => {
  const [, missed] = withWithoutRows({ present: fullSide, absent: emptySide }, cards.with_without.sides);
  const rate = withWithout.at(-1);
  assert.equal(cellHtml(rate, missed), '');
  assert.equal(cellHtml(withWithout[1], missed), '0');
  const html = tableHtml({ caption: 'c', columns: withWithout, rows: [missed] });
  assert.ok(!html.includes('NaN'));
  assert.match(html, /col-points_per_match dt-num"><\/td>/);
});

test('a rate of nought shows as nought: it is a rate, not a gap', () => {
  const [played] = withWithoutRows({ present: { ...fullSide, points_per_match_rounded: 0 }, absent: emptySide }, cards.with_without.sides);
  assert.equal(cellHtml(withWithout.at(-1), played), '0');
});

test('the blank heading is still named for a screen reader', () => {
  assert.equal(cards.with_without.headings.side, ' ');
  assert.equal(withWithout[0].label, PLAYERS.sideHeading);
  assert.equal(withWithout[0].hideLabel, true);
});
