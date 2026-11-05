import assert from 'node:assert/strict';
import { readdirSync, readFileSync } from 'node:fs';
import { test } from 'node:test';

import {
  attackDefenceAxes,
  attackDefenceDomains,
  attackDefenceRows,
  axisTickFormat,
  coefficientDomain,
  coefficientHeight,
  coefficientRows,
  dotRadius,
  equalDomains,
  needsSeparateAxes,
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

test('axes stay equal up to and including a ratio of exactly 5', () => {
  assert.equal(needsSeparateAxes([0, 1], [0, 5]), false);
  assert.equal(needsSeparateAxes([0, 5], [0, 1]), false);
  assert.equal(needsSeparateAxes([-1, 1], [-3, 7]), false);
  assert.equal(needsSeparateAxes([0, 1], [0, 5.000001]), true);
  assert.equal(needsSeparateAxes([0, 5.000001], [0, 1]), true);
});

test('a span counts 0, so equal values away from 0 still have one', () => {
  assert.equal(needsSeparateAxes([2, 2, 2], [3, 3, 3]), false);
  assert.equal(needsSeparateAxes([2, 2, 2], [11, 11, 11]), true);
  assert.equal(needsSeparateAxes([1, 2], [-4, 6]), false);
  assert.equal(needsSeparateAxes([1, 2], [-4, 7]), true);
});

test('an axis that is all exactly 0 keeps the equal scale, with or without data on the other', () => {
  assert.equal(needsSeparateAxes([0, 0], [0, 0]), false);
  assert.equal(needsSeparateAxes([0, 0], [-3, 3]), false);
  assert.equal(needsSeparateAxes([-3, 3], [0, 0]), false);
  assert.equal(needsSeparateAxes([], []), false);
});

test('the season 2 ridge shape gets its own domain on each axis, and 0 stays in both', () => {
  const xs = [-4e-37, 6e-37];
  const ys = [-1, 1.5];
  const domains = attackDefenceDomains(xs, ys);
  assert.equal(domains.separate, true);
  assert.deepEqual(domains.x.map((value) => Number(value.toPrecision(6))), [-5.4e-37, 7.4e-37]);
  assert.deepEqual(domains.y.map((value) => Number(value.toPrecision(6))), [-1.35, 1.85]);

  const oneSided = attackDefenceDomains([1e-30, 2e-30], [1, 2]);
  assert.ok(oneSided.x[0] < 0 && oneSided.x[1] > 2e-30);
  assert.ok(oneSided.y[0] < 0 && oneSided.y[1] > 2);
});

test('below the ratio the domains are the equal ones', () => {
  const xs = [-9, 8];
  const ys = [-5, 2];
  assert.deepEqual(attackDefenceDomains(xs, ys), { ...equalDomains(xs, ys), separate: false });
});

test('the plot and its note get one answer: separate axes come with the wording, equal ones with none', () => {
  const row = (goals_scored, defensive_effect) => ({ goals_scored, defensive_effect });

  const separate = attackDefenceAxes([row(-4e-37, -1), row(6e-37, 1.5)]);
  assert.equal(separate.note, 'The two axes use different scales here: one set of estimates is far smaller than the other.');
  assert.ok(separate.x[1] - separate.x[0] < 1e-35);
  assert.ok(separate.y[1] - separate.y[0] > 1);

  const equal = attackDefenceAxes([row(-1, -1), row(2, 1.5)]);
  assert.equal(equal.note, null);
  assert.ok(Math.abs(equal.x[1] - equal.x[0] - (equal.y[1] - equal.y[0])) < 1e-9);

  assert.equal(attackDefenceAxes([]).note, null);
});

// Written against whatever the export holds today, so a weekly merge cannot break it.
test('every real state holds all its dots, and shows the note only when its axes are separate', () => {
  const folder = new URL('../public/data/seasons/', import.meta.url);
  for (const file of readdirSync(folder)) {
    const { regression } = JSON.parse(readFileSync(new URL(file, folder)));
    for (const [estimator, { has_regression: hasRegression, players }] of Object.entries(regression)) {
      if (!hasRegression) continue;
      const rows = attackDefenceRows(players);
      const xs = rows.map((one) => one.goals_scored);
      const ys = rows.map((one) => one.defensive_effect);
      const axes = attackDefenceAxes(rows);
      const state = `${file} ${estimator}`;
      assert.equal(axes.note !== null, needsSeparateAxes(xs, ys), state);
      for (const value of xs) assert.ok(value > axes.x[0] && value < axes.x[1], state);
      for (const value of ys) assert.ok(value > axes.y[0] && value < axes.y[1], state);
    }
  }
});
