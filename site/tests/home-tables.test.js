import assert from 'node:assert/strict';
import { test } from 'node:test';

import { leagueWindow, playerStatRows, splitEstimate } from '../src/lib/home-tables.js';

const table = (n) => Array.from({ length: n }, (_, i) => ({ position: i + 1, team: `T${i + 1}` }));
const positions = (rows) => rows.map((row) => row.position);

test('two either side in the middle', () => {
  assert.deepEqual(positions(leagueWindow(table(9), 'T4')), [2, 3, 4, 5, 6]);
});

test('the window slides at the top and the bottom', () => {
  assert.deepEqual(positions(leagueWindow(table(9), 'T1')), [1, 2, 3, 4, 5]);
  assert.deepEqual(positions(leagueWindow(table(9), 'T9')), [5, 6, 7, 8, 9]);
});

test('a short league shows all of it, and a missing team none', () => {
  assert.deepEqual(positions(leagueWindow(table(3), 'T2')), [1, 2, 3]);
  assert.deepEqual(leagueWindow(table(9), 'Nobody'), []);
});

test("R's estimate splits from its p-value", () => {
  assert.deepEqual(splitEstimate('-2.34 [0.090]'), { estimate: '-2.34', p: '0.090' });
  assert.deepEqual(splitEstimate('0.0437'), { estimate: '0.0437', p: '' });
  assert.deepEqual(splitEstimate(null), { estimate: '', p: '' });
});

const effect = (estimate, display) => ({ estimate, display });

const subset = {
  scorers: { rows: [{ player: 'B', goals: 3 }, { player: 'Guest', goals: 2 }, { player: 'A', goals: 3 }] },
  profiles: [
    { player: 'A', appearances: 5, attendance_rate: 0.5 },
    { player: 'B', appearances: 8, attendance_rate: 0.8 },
    { player: 'C', appearances: 2, attendance_rate: 0.2 },
  ],
  regression: {
    ols: {
      has_regression: true,
      players: [
        { player: 'A', outcomes: { goal_difference: effect(1.2, '1.20 [0.400]') } },
        { player: 'B', outcomes: { goal_difference: effect(-0.5, '-0.50 [0.700]') } },
      ],
    },
  },
};

test('goals first, then turnout, with guests and non-scorers in', () => {
  assert.deepEqual(
    playerStatRows(subset).map((row) => row.player),
    ['B', 'A', 'Guest', 'C'],
  );
});

test('a guest has no turnout and a player under the floor no coefficient', () => {
  const rows = new Map(playerStatRows(subset).map((row) => [row.player, row]));
  assert.equal(rows.get('Guest').attendance_rate, null);
  assert.equal(rows.get('C').goals, 0);
  assert.equal(rows.get('C').gd_display, null);
  assert.deepEqual(rows.get('A').gd_display, { estimate: '1.20', p: '0.400' });
});

test('no OLS fit leaves every coefficient empty', () => {
  const unfit = { ...subset, regression: { ols: { has_regression: false, players: [] } } };
  assert.ok(playerStatRows(unfit).every((row) => row.gd_effect === null));
});
