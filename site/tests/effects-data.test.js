import assert from 'node:assert/strict';
import { test } from 'node:test';

import {
  attackDefenceRows,
  axisTickFormat,
  coefficientDomain,
  coefficientHeight,
  coefficientRows,
  dotRadius,
  equalDomains,
  sizeKeyValues,
} from '../src/lib/effects-data.js';

const player = (name, scored, conceded) => ({
  player: name,
  appearances: 10,
  outcomes: {
    goals_scored: { estimate: scored, conf_low: scored - 1, conf_high: scored + 1, display: 'not read' },
    goals_conceded: { estimate: conceded, conf_low: null, conf_high: null, display: 'not read' },
  },
});

test('defence is goals conceded with the sign flipped', () => {
  const [row] = attackDefenceRows([player('Boris', 2.5, 0.75)]);
  assert.equal(row.goals_scored, 2.5);
  assert.equal(row.defensive_effect, -0.75);
});

test('both axes get one span, so a unit is the same length across and up', () => {
  const { x, y } = equalDomains([-9, 8], [-5, 2]);
  assert.ok(Math.abs(x[1] - x[0] - (y[1] - y[0])) < 1e-9);
});

test('zero and all the data are inside both domains', () => {
  const { x, y } = equalDomains([1, 2], [-5, -3]);
  for (const value of [0, 1, 2]) assert.ok(value > x[0] && value < x[1]);
  for (const value of [0, -5, -3]) assert.ok(value > y[0] && value < y[1]);
});

test('ridge values many orders of magnitude apart keep the wider axis scale', () => {
  // Season 2 under ridge: goals scored near 1e-37, goals conceded near 1.
  const { x, y } = equalDomains([-6.7e-37, 3.4e-37], [-0.6, 1.97]);
  const span = y[1] - y[0];
  assert.ok(span > 2.5 && span < 4);
  assert.ok(Math.abs(x[1] - x[0] - span) < 1e-9);
  assert.ok(x[0] < 0 && x[1] > 0);
});

test('tiny values on both axes get a tiny domain', () => {
  const { x, y } = equalDomains([-1e-40, 2e-40], [-3e-40, 1e-40]);
  assert.ok(x[1] - x[0] < 1e-39);
  assert.ok(Math.abs(x[1] - x[0] - (y[1] - y[0])) < 1e-50);
  assert.ok(x[1] > x[0] && y[1] > y[0]);
});

test('everything at zero still has a usable domain', () => {
  const { x, y } = equalDomains([0, 0], [0, 0]);
  assert.deepEqual(x, [-0.5, 0.5]);
  assert.deepEqual(y, [-0.5, 0.5]);
});

test('the coefficient domain covers the intervals, zero and a missing interval', () => {
  const [low, high] = coefficientDomain([{ estimate: 1, conf_low: 0.5, conf_high: 3 }]);
  assert.ok(low < 0 && high > 3);
  const [ridgeLow, ridgeHigh] = coefficientDomain([{ estimate: -1e-37, conf_low: null, conf_high: null }]);
  assert.ok(ridgeLow < -1e-37 && ridgeHigh > 0);
  assert.ok(ridgeHigh - ridgeLow < 1e-36);
});

test('coefficient rows run from the largest estimate down, ties by name', () => {
  const rows = coefficientRows([player('B', 1, 0), player('C', 1e-37, 0), player('A', 1, 0), player('D', -2, 0)], 'goals_scored');
  assert.deepEqual(rows.map((row) => row.player), ['A', 'B', 'C', 'D']);
});

test('the plot grows with the player count', () => {
  assert.ok(coefficientHeight(17) > coefficientHeight(10));
  assert.equal(coefficientHeight(17) - coefficientHeight(16), coefficientHeight(11) - coefficientHeight(10));
});

test('axis ticks keep Plot\'s default until they would run off the phone', () => {
  assert.equal(axisTickFormat([-3, 3]), undefined);
  assert.equal(axisTickFormat([-0.003, 0.002]), undefined);
  const format = axisTickFormat([-7e-37, 4e-37]);
  assert.equal(format(0), '0');
  assert.equal(format(5e-37), '5e-37');
  assert.equal(format(-2.5e-37), '-2.5e-37');
  assert.equal(format(3.0000000000000004e-37), '3e-37');
});

test('dot area is proportional to appearances', () => {
  assert.equal(dotRadius(40, 40, 10), 10);
  assert.equal(dotRadius(10, 40, 10), 5);
  assert.equal(dotRadius(0, 40, 10), 0);
  assert.equal(dotRadius(3, 0, 10), 0);
});

test('the size key uses round numbers and no more than four circles', () => {
  assert.deepEqual(sizeKeyValues(41), [10, 20, 30, 40]);
  assert.deepEqual(sizeKeyValues(21), [5, 10, 15, 20]);
  assert.deepEqual(sizeKeyValues(7), [2, 4, 6]);
  assert.deepEqual(sizeKeyValues(3), [1, 2, 3]);
  assert.deepEqual(sizeKeyValues(180), [50, 100, 150]);
});
