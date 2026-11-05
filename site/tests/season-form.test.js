import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { test } from 'node:test';

import { formMetric, formPoints } from '../src/lib/season-form.js';

const wording = JSON.parse(readFileSync(new URL('../src/generated/wording.json', import.meta.url)));
const labels = wording.plot_labels.season_form;
const columns = (metric) => formMetric(metric, labels).series.map((line) => line.column);

test('each metric draws the rolling columns season_form_plot() draws', () => {
  assert.deepEqual(columns('points'), ['rolling_points']);
  assert.deepEqual(columns('goals'), ['rolling_goals_scored', 'rolling_goals_conceded']);
  assert.deepEqual(columns('goal_difference'), ['rolling_goal_difference']);
  assert.deepEqual(columns('squad_size'), ['rolling_squad_size']);
});

test('every metric R offers has its lines, and the axis label R gives it', () => {
  const offered = wording.matches_cards.season_form.metrics;
  assert.equal(offered.length, 4);
  for (const { value } of offered) {
    assert.equal(formMetric(value, labels).yLabel, labels[value].y);
    assert.ok(labels[value].y);
  }
});

test('a metric nobody knows fails loudly', () => {
  assert.throws(() => formMetric('shots', labels), /shots/);
});

test('points has the fixed axis, and only goal difference has the line at 0', () => {
  assert.deepEqual(formMetric('points', labels).domain, [0, 3]);
  for (const metric of ['goals', 'goal_difference', 'squad_size']) assert.equal(formMetric(metric, labels).domain, null);
  assert.equal(formMetric('goal_difference', labels).reference, 0);
  for (const metric of ['points', 'goals', 'squad_size']) assert.equal(formMetric(metric, labels).reference, null);
});

test('only the two lines on goals have names, for the key, and the colours are tokens', () => {
  const goals = formMetric('goals', labels).series;
  assert.deepEqual(goals.map((line) => line.label), ['Goals scored', 'Goals conceded']);
  assert.deepEqual(goals.map((line) => line.colour), ['var(--blue)', 'var(--amber)']);
  for (const metric of ['points', 'goal_difference', 'squad_size']) {
    assert.deepEqual(formMetric(metric, labels).series.map((line) => line.label), [null]);
  }
});

test('the points are the exported numbers, on each match day in UTC', () => {
  const form = [
    { date: '2026-10-21', rolling_goals_scored: 3, rolling_goals_conceded: 1.6 },
    { date: '2026-10-28', rolling_goals_scored: 2.8, rolling_goals_conceded: 2 },
  ];
  const [scored, conceded] = formPoints(form, formMetric('goals', labels));
  assert.deepEqual(scored.points.map((point) => point.value), [3, 2.8]);
  assert.deepEqual(conceded.points.map((point) => point.value), [1.6, 2]);
  assert.equal(scored.points[0].date.toISOString(), '2026-10-21T00:00:00.000Z');
  assert.equal(scored.colour, 'var(--blue)');
});
