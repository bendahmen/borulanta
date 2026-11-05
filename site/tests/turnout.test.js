import assert from 'node:assert/strict';
import { test } from 'node:test';

import { formatTurnout } from '../src/lib/turnout.js';

test('a tie rounds up, as DT does and R does not', () => {
  assert.equal(formatTurnout(0.125), '13%');
});

test('nothing and everything', () => {
  assert.equal(formatTurnout(0), '0%');
  assert.equal(formatTurnout(1), '100%');
});

test('the rates on the live lineups', () => {
  assert.equal(formatTurnout(0.8039215686), '80%');
  assert.equal(formatTurnout(0.01960784314), '2%');
  assert.equal(formatTurnout(0.5714285714), '57%');
});

test('no rate is an empty cell, as in DT', () => {
  assert.equal(formatTurnout(null), '');
  assert.equal(formatTurnout(undefined), '');
  assert.equal(formatTurnout(Number.NaN), '');
});
