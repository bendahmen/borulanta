import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { test } from 'node:test';

import { DATE_FORMATS, formatDate } from '../src/lib/dates.js';

// R's own output for every date in data/, from fixtures/write-r-dates.R.
const r = JSON.parse(readFileSync(new URL('./fixtures/r-dates.json', import.meta.url), 'utf8'));

test('the six formats are the ones R writes', () => {
  assert.deepEqual(DATE_FORMATS, r.formats);
});

test('every date on file comes out as R writes it, in all six formats', () => {
  assert.ok(r.dates.length > 50);
  for (const format of r.formats) {
    assert.deepEqual(r.dates.map((date) => formatDate(date, format)), r.written[format], format);
  }
});

// The weekday is worked out by hand, so check it against Date for every day of a few years,
// leap days and the turn of a century included.
test('the weekday is right on every day, leap years included', () => {
  const names = ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'];
  for (let day = Date.UTC(1999, 11, 1); day <= Date.UTC(2032, 2, 1); day += 86400000) {
    const iso = new Date(day).toISOString().slice(0, 10);
    assert.equal(formatDate(iso, '%a %d %b %Y').slice(0, 3), names[new Date(day).getUTCDay()], iso);
  }
  assert.equal(formatDate('2028-02-29', '%a %d %b %Y'), 'Tue 29 Feb 2028');
});

test('a day and a month keep their leading zero, and a month keeps its English name', () => {
  assert.equal(formatDate('2026-09-09', '%a %d %b %Y'), 'Wed 09 Sep 2026');
  assert.equal(formatDate('2026-09-09', '%d %B %Y'), '09 September 2026');
  assert.equal(formatDate('2026-06-03', '%b'), 'Jun');
});

test('a format R does not use is a mistake', () => {
  assert.throws(() => formatDate('2026-09-09', '%Y-%m-%d'), /No date format/);
});
