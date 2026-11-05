import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { test } from 'node:test';
import vm from 'node:vm';

import { onReturn, upcomingFixtures } from '../src/lib/fixture-dates.js';
import { moduleToInline } from '../src/lib/inline.js';
import { mountRunIn, paintRunIn } from '../src/lib/run-in.js';
import { fakeWindow } from './helpers/fake-dom.js';

const nextDay = (iso, days = 1) => {
  const date = new Date(`${iso}T12:00:00Z`);
  date.setUTCDate(date.getUTCDate() + days);
  return date.toISOString().slice(0, 10);
};

// Only what paintRunIn() touches.
function fakeRoot(dates) {
  const rows = dates.map((date) => ({ dataset: { date }, hidden: true }));
  const table = { hidden: false };
  const empty = { hidden: true };
  return {
    rows,
    table,
    empty,
    querySelectorAll: () => rows,
    querySelector: (selector) => (selector === 'table' ? table : empty),
  };
}

const real = JSON.parse(readFileSync(new URL('../src/generated/fixtures.json', import.meta.url)));
const dates = real.fixtures.map((fixture) => fixture.date);

// Written against whatever the export holds today, so a weekly merge cannot break it.
test('over the real fixtures, a day before the first shows them all', (t) => {
  if (dates.length === 0) return t.skip('no fixtures on file');
  const root = fakeRoot(dates);
  paintRunIn(root, nextDay(dates[0], -1));
  assert.ok(root.rows.every((row) => !row.hidden));
  assert.equal(root.table.hidden, false);
  assert.equal(root.empty.hidden, true);
});

test('over the real fixtures, one dated today still counts and yesterday\'s has gone', (t) => {
  if (dates.length < 2) return t.skip('too few fixtures on file');
  const root = fakeRoot(dates);
  paintRunIn(root, dates[1]);
  assert.equal(root.rows[0].hidden, true);
  assert.equal(root.rows[1].hidden, false);
  assert.equal(root.rows.filter((row) => !row.hidden).length, upcomingFixtures(real.fixtures, dates[1]).length);
});

test('over the real fixtures, the day after the last leaves the empty wording and no table', (t) => {
  if (dates.length === 0) return t.skip('no fixtures on file');
  const root = fakeRoot(dates);
  paintRunIn(root, nextDay(dates.at(-1)));
  assert.ok(root.rows.every((row) => row.hidden));
  assert.equal(root.table.hidden, true);
  assert.equal(root.empty.hidden, false);
});

test('the fixture list holds the same rows as the filter would keep from the far past', () => {
  assert.deepEqual(upcomingFixtures(real.fixtures, '1900-01-01'), real.fixtures);
});

test('no fixtures on file at all means no table to touch and the empty wording', () => {
  const root = fakeRoot([]);
  root.querySelector = (selector) => (selector === 'table' ? null : root.empty);
  paintRunIn(root, '2026-11-05');
  assert.equal(root.empty.hidden, false);
});

test('onReturn repaints on a restored page and a tab shown again, and at no other time', (t) => {
  const { win, doc } = fakeWindow(t);
  let paints = 0;
  onReturn(() => {
    paints += 1;
  });
  assert.equal(paints, 0, 'painting first is the caller\'s job');

  win.fire('pageshow', { persisted: false });
  assert.equal(paints, 0);
  win.fire('pageshow', { persisted: true });
  assert.equal(paints, 1);

  doc.hidden = true;
  doc.fire('visibilitychange');
  assert.equal(paints, 1);
  doc.hidden = false;
  doc.fire('visibilitychange');
  assert.equal(paints, 2);
});

test('a run-in left open past a match day drops that row when the tab comes back', (t) => {
  const { win, doc } = fakeWindow(t);
  t.mock.timers.enable({ apis: ['Date'], now: Date.parse('2026-11-04T12:00:00Z') });
  const root = fakeRoot(['2026-11-04', '2026-11-11']);

  mountRunIn(root);
  assert.deepEqual(root.rows.map((row) => row.hidden), [false, false]);

  t.mock.timers.setTime(Date.parse('2026-11-05T12:00:00Z'));
  doc.hidden = true;
  doc.fire('visibilitychange');
  assert.deepEqual(root.rows.map((row) => row.hidden), [false, false]);
  doc.hidden = false;
  doc.fire('visibilitychange');
  assert.deepEqual(root.rows.map((row) => row.hidden), [true, false]);

  t.mock.timers.setTime(Date.parse('2026-11-12T12:00:00Z'));
  win.fire('pageshow', { persisted: true });
  assert.deepEqual(root.rows.map((row) => row.hidden), [true, true]);
  assert.equal(root.empty.hidden, false);
});

test('the sources run together as a classic script', () => {
  const source = (name) => moduleToInline(readFileSync(new URL(`../src/lib/${name}.js`, import.meta.url), 'utf8'));
  const inline = `${source('fixture-dates')}\n${source('run-in')}`;
  assert.doesNotMatch(inline, /^(export|import) /m);
  const context = { rows: [{ dataset: { date: '2026-11-04' } }, { dataset: { date: '2026-11-05' } }], Set };
  vm.runInNewContext(`${inline}
    var root = { querySelectorAll: function () { return rows; }, querySelector: function () { return {}; } };
    paintRunIn(root, londonDate(new Date('2026-11-05T12:00:00Z')));`, context);
  assert.deepEqual(context.rows.map((row) => row.hidden), [true, false]);
});
