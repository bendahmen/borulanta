import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { test } from 'node:test';

import { tableHtml } from '../src/lib/data-table.js';
import { MATCHES } from '../src/lib/labels.js';
import { badgeClass, everyResultColumns, lineupColumns, opponentColumns, scorerColumns } from '../src/lib/match-tables.js';

const { matches_cards: cards } = JSON.parse(readFileSync(new URL('../src/generated/wording.json', import.meta.url)));
const labels = (columns) => columns.map((column) => column.label);

test('the headings and their order are R tables', () => {
  assert.deepEqual(labels(scorerColumns(cards.scorers.headings)), ['Player', 'Goals', 'MOM', 'Appearances', 'Goals per game']);
  assert.deepEqual(labels(opponentColumns(cards.head_to_head.headings)), ['Opponent', 'P', 'W', 'D', 'L', 'GF', 'GA', 'GD', 'Pts/match']);
  assert.deepEqual(labels(lineupColumns(cards.match_detail.headings)), ['Player', 'Season appearances', 'Turnout']);
  assert.deepEqual(labels(everyResultColumns(cards.every_result, MATCHES)).slice(0, 4), ['Date', 'Opponent', 'Result', 'Outcome']);
});

test('scorers show R rounded rate and sort on the full one', () => {
  const column = scorerColumns(cards.scorers.headings).at(-1);
  assert.equal(column.key, 'goals_per_appearance');
  assert.equal(column.value({ goals_per_appearance: 0.142857, goals_per_appearance_rounded: 0.14 }), 0.14);
});

test('a scorer with no appearances has an empty rate rather than a nought', () => {
  const html = tableHtml({ caption: 'c', columns: scorerColumns(cards.scorers.headings), rows: [{ player: 'Akash', goals: 2, mom: 0, appearances: 0, goals_per_appearance_rounded: null }] });
  assert.match(html, /col-goals_per_appearance dt-num"><\/td>/);
});

test('head to head shows the rounded points and folds only the goals', () => {
  const columns = opponentColumns(cards.head_to_head.headings);
  assert.deepEqual(columns.filter((column) => column.folds).map((column) => column.label), ['GF', 'GA', 'GD']);
  assert.equal(columns.at(-1).value({ points_per_match: 1.333333, points_per_match_rounded: 1.33 }), 1.33);
});

const lineupRow = { player: 'Émile Zola', season_appearances: 2, attendance_rate: 0.125 };

test('a lineup shows Turnout as DT does and links a name by its slug', () => {
  const html = tableHtml({ caption: 'c', columns: lineupColumns(cards.match_detail.headings), rows: [lineupRow] });
  assert.ok(html.includes('13%'));
  assert.ok(html.includes('<a href="/players/emile-zola/">Émile Zola</a>'));
});

test('a result with no opponent shows the mark R gives it', () => {
  const opponent = everyResultColumns(cards.every_result, MATCHES)[1];
  assert.equal(opponent.value({ opponent: null }), '—');
  assert.equal(opponent.value({ opponent: 'Ball FC' }), 'Ball FC');
});

test('a row links to its match page, and says which in words', () => {
  const page = everyResultColumns(cards.every_result, MATCHES).at(-1);
  const html = page.html({ date: '2026-10-28', date_short: '28 Oct 2026' });
  assert.ok(html.includes('href="/matches/2026-10-28/"'));
  assert.ok(html.includes('aria-label="Match page for 28 Oct 2026"'));
  assert.equal(page.hideLabel, true);
});

test('nothing in a row can write markup', () => {
  const rows = [{ date: '<i>', date_short: '<b>', opponent: '<script>', result: '<u>', badge: '<em>' }];
  const html = tableHtml({ caption: 'c', columns: everyResultColumns(cards.every_result, MATCHES), rows });
  assert.ok(!/<(i|b|script|u|em)>/.test(html));
});

test('the badge class is R word in lower case', () => {
  assert.equal(badgeClass('LOSS'), 'badge badge-loss');
  assert.equal(badgeClass('WIN'), 'badge badge-win');
  assert.equal(badgeClass('DRAW'), 'badge badge-draw');
});

// Every field a different value, and the near-misses too, so that a column reading the wrong
// field fails here rather than showing a plausible number on the page.
const shownBy = (columns, row) => Object.fromEntries(columns.map((column) => [column.key, column.value(row)]));

test('each column of Every result reads its own field', () => {
  const row = { date: 'iso', date_short: 'short', date_label: 'label', opponent: 'them', result: 'score', badge: 'word', goals_for: 'for', goals_against: 'against', points: 'pts' };
  assert.deepEqual(shownBy(everyResultColumns(cards.every_result, MATCHES), row), {
    date: 'short',
    opponent: 'them',
    result: 'score',
    outcome: 'word',
    page: 'iso',
  });
});

test('each column of Scorers reads its own field', () => {
  const row = { player: 'name', goals: 1, mom: 2, appearances: 3, goals_per_appearance: 4, goals_per_appearance_rounded: 5 };
  assert.deepEqual(shownBy(scorerColumns(cards.scorers.headings), row), {
    player: 'name',
    goals: 1,
    mom: 2,
    appearances: 3,
    goals_per_appearance: 5,
  });
});

test('each column of Head to head reads its own field', () => {
  const row = { opponent: 'them', played: 1, won: 2, drawn: 3, lost: 4, goals_for: 5, goals_against: 6, goal_difference: 7, points_per_match: 8, points_per_match_rounded: 9 };
  assert.deepEqual(shownBy(opponentColumns(cards.head_to_head.headings), row), {
    opponent: 'them',
    played: 1,
    won: 2,
    drawn: 3,
    lost: 4,
    'goals-for': 5,
    'goals-against': 6,
    'goal-difference': 7,
    'points-per-match': 9,
  });
});

test('each column of a lineup reads its own field', () => {
  const row = { player: 'name', season_appearances: 41, attendance_rate: 0.75 };
  assert.deepEqual(shownBy(lineupColumns(cards.match_detail.headings), row), {
    player: 'name',
    'season-appearances': 41,
    turnout: '75%',
  });
});
