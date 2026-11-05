import assert from 'node:assert/strict';
import { execFileSync } from 'node:child_process';
import { readFileSync } from 'node:fs';
import { test } from 'node:test';
import vm from 'node:vm';

import { daysBetween, londonDate, upcomingFixtures } from '../src/lib/fixture-dates.js';
import { moduleToInline, scriptSafeJson } from '../src/lib/inline.js';
import { isStale, nextMatchView, whenWord } from '../src/lib/next-match.js';

const wording = {
  tonight: 'Tonight',
  tomorrow: 'Tomorrow',
  in_days: 'In {days} days',
  opponent_unknown: 'Opponent to be confirmed',
  no_fixtures_left: 'None left',
  no_fixture_list: 'No list',
};

const fixture = (date, extra = {}) => ({
  date,
  date_label: `label ${date}`,
  opponent: `v ${date}`,
  kickoff_pitch: '20:00 · Pitch 1',
  ...extra,
});

const data = (fixtures, extra = {}) => ({
  fixtures,
  have_fixtures: true,
  scraped_on: null,
  stale_note: 'Stale',
  stale_days: 8,
  wording,
  ...extra,
});

test('londonDate moves to the next day an hour early in summer', () => {
  assert.equal(londonDate(new Date('2026-06-30T22:59:00Z')), '2026-06-30');
  assert.equal(londonDate(new Date('2026-06-30T23:30:00Z')), '2026-07-01');
});

test('londonDate holds across the October clock change', () => {
  // Clocks go back at 01:00 UTC on 25 October 2026.
  assert.equal(londonDate(new Date('2026-10-24T22:59:00Z')), '2026-10-24');
  assert.equal(londonDate(new Date('2026-10-24T23:30:00Z')), '2026-10-25');
  assert.equal(londonDate(new Date('2026-10-25T00:30:00Z')), '2026-10-25');
  assert.equal(londonDate(new Date('2026-10-25T01:00:00Z')), '2026-10-25');
  // Now on GMT, so 23:30 UTC is still the same day.
  assert.equal(londonDate(new Date('2026-10-25T23:30:00Z')), '2026-10-25');
  assert.equal(londonDate(new Date('2026-10-26T00:00:00Z')), '2026-10-26');
});

test('londonDate holds across the March clock change', () => {
  // Clocks go forward at 01:00 UTC on 28 March 2027.
  assert.equal(londonDate(new Date('2027-03-27T23:30:00Z')), '2027-03-27');
  assert.equal(londonDate(new Date('2027-03-28T22:59:00Z')), '2027-03-28');
  assert.equal(londonDate(new Date('2027-03-28T23:30:00Z')), '2027-03-29');
});

// A child process per zone, because the formatter is built when the module loads and
// setting TZ here would come too late. London's date differs from every zone below at
// one of these instants, so a formatter that fell back to local time gives a wrong date.
test('londonDate ignores the time zone of the phone', () => {
  const moduleUrl = new URL('../src/lib/fixture-dates.js', import.meta.url).href;
  const script = `import { londonDate } from ${JSON.stringify(moduleUrl)};
    console.log(process.argv.slice(1).map((iso) => londonDate(new Date(iso))).join(' '));`;
  const instants = ['2026-06-30T23:30:00Z', '2026-06-30T12:30:00Z'];

  for (const zone of ['Pacific/Auckland', 'America/Los_Angeles', 'UTC']) {
    const output = execFileSync(process.execPath, ['--input-type=module', '-e', script, ...instants], {
      env: { ...process.env, TZ: zone },
      encoding: 'utf8',
    });
    assert.equal(output.trim(), '2026-07-01 2026-06-30', zone);
  }
});

test('daysBetween counts calendar days through month ends and clock changes', () => {
  assert.equal(daysBetween('2026-11-04', '2026-11-04'), 0);
  assert.equal(daysBetween('2026-10-31', '2026-11-01'), 1);
  assert.equal(daysBetween('2026-10-24', '2026-10-26'), 2);
  assert.equal(daysBetween('2026-12-31', '2027-01-06'), 6);
  assert.equal(daysBetween('2026-11-05', '2026-11-04'), -1);
});

test('a fixture dated today counts and past ones drop', () => {
  const fixtures = [fixture('2026-11-03'), fixture('2026-11-04'), fixture('2026-11-11')];
  assert.deepEqual(
    upcomingFixtures(fixtures, '2026-11-04').map((row) => row.date),
    ['2026-11-04', '2026-11-11'],
  );
  assert.equal(nextMatchView(data(fixtures), '2026-11-04').opponent, 'v 2026-11-04');
  assert.equal(nextMatchView(data(fixtures), '2026-11-05').opponent, 'v 2026-11-11');
});

test('the countdown word for 0, 1 and N days', () => {
  assert.equal(whenWord(0, wording), 'Tonight');
  assert.equal(whenWord(1, wording), 'Tomorrow');
  assert.equal(whenWord(2, wording), 'In 2 days');
  assert.equal(whenWord(13, wording), 'In 13 days');

  const fixtures = [fixture('2026-11-11')];
  const today = nextMatchView(data(fixtures), '2026-11-11');
  assert.equal(today.when, 'Tonight');
  assert.equal(today.isToday, true);
  assert.equal(nextMatchView(data(fixtures), '2026-11-10').when, 'Tomorrow');
  const later = nextMatchView(data(fixtures), '2026-11-04');
  assert.equal(later.when, 'In 7 days');
  assert.equal(later.isToday, false);
});

test('stale exactly past the threshold, as today - last_read > 8 in R', () => {
  assert.equal(isStale('2026-10-29', '2026-11-05', 8), false); // 7 days
  assert.equal(isStale('2026-10-29', '2026-11-06', 8), false); // exactly 8
  assert.equal(isStale('2026-10-29', '2026-11-07', 8), true); // 9
  assert.equal(isStale(null, '2026-11-07', 8), false);

  const fixtures = [fixture('2026-12-02')];
  const read = { scraped_on: '2026-10-29' };
  assert.equal(nextMatchView(data(fixtures, read), '2026-11-06').staleNote, null);
  assert.equal(nextMatchView(data(fixtures, read), '2026-11-07').staleNote, 'Stale');
});

test('an empty list takes the right one of the two wordings, with no stale note', () => {
  assert.deepEqual(nextMatchView(data([]), '2026-11-04'), { empty: 'None left' });
  assert.deepEqual(nextMatchView(data([], { have_fixtures: false }), '2026-11-04'), {
    empty: 'No list',
  });
  // A list that has run out is still "none left" rather than "no list".
  assert.deepEqual(nextMatchView(data([fixture('2026-11-03')]), '2026-11-04'), {
    empty: 'None left',
  });
  assert.deepEqual(
    nextMatchView(data([], { scraped_on: '2020-01-01' }), '2026-11-04'),
    { empty: 'None left' },
  );
});

test('a missing opponent or kickoff degrades as the R card does', () => {
  const view = nextMatchView(
    data([fixture('2026-11-11', { opponent: null, kickoff_pitch: null })]),
    '2026-11-04',
  );
  assert.equal(view.opponent, 'Opponent to be confirmed');
  assert.equal(view.kickoffPitch, null);
});

// Written against whatever the export holds today, so a weekly merge cannot break it.
test('a run over the real fixtures.json with a pinned today', (t) => {
  const real = JSON.parse(readFileSync(new URL('../src/generated/fixtures.json', import.meta.url)));
  if (real.fixtures.length === 0) return t.skip('no fixtures on file');
  const realData = { ...real, stale_days: 8 };
  const [first] = real.fixtures;
  const last = real.fixtures.at(-1);

  const onTheDay = nextMatchView(realData, first.date);
  assert.equal(onTheDay.when, real.wording.tonight);
  assert.equal(onTheDay.opponent, first.opponent);
  assert.equal(onTheDay.dateLabel, first.date_label);
  assert.equal(onTheDay.kickoffPitch, first.kickoff_pitch);

  const dayBefore = new Date(`${first.date}T12:00:00Z`);
  dayBefore.setUTCDate(dayBefore.getUTCDate() - 1);
  assert.equal(nextMatchView(realData, dayBefore.toISOString().slice(0, 10)).when, real.wording.tomorrow);

  const dayAfterLast = new Date(`${last.date}T12:00:00Z`);
  dayAfterLast.setUTCDate(dayAfterLast.getUTCDate() + 1);
  assert.deepEqual(nextMatchView(realData, dayAfterLast.toISOString().slice(0, 10)), {
    empty: real.wording.no_fixtures_left,
  });
});

test('the inlined source runs as a classic script and agrees with the module', () => {
  const read = (name) => readFileSync(new URL(`../src/lib/${name}.js`, import.meta.url), 'utf8');
  const inline = `${moduleToInline(read('fixture-dates'))}\n${moduleToInline(read('next-match'))}`;
  assert.doesNotMatch(inline, /^(export|import) /m);

  const context = {};
  vm.runInNewContext(`${inline}\nthis.result = [
    londonDate(new Date('2026-06-30T23:30:00Z')),
    nextMatchView(${scriptSafeJson(data([fixture('2026-11-11')]))}, '2026-11-10').when,
  ];`, context);
  assert.equal(context.result[0], '2026-07-01');
  assert.equal(context.result[1], 'Tomorrow');
});

test('scriptSafeJson cannot close a script tag', () => {
  const json = scriptSafeJson({ opponent: '</script><script>alert(1)</script>' });
  assert.doesNotMatch(json, /</);
  assert.equal(JSON.parse(json).opponent, '</script><script>alert(1)</script>');
});
