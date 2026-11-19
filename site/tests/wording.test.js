// The words that depend on the data. Each sentence is R's, word for word, so the expected
// strings here are written out in full rather than built the way the code builds them.
import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { test } from 'node:test';

import { feeRows } from '../src/lib/fee-rows.js';
import { FEE_STATUS } from '../src/lib/labels.js';
import {
  differentialNote,
  estimatorNote,
  fixtureWords,
  freshnessNote,
  headlineTiles,
  kickoffPitch,
  leaguePosition,
  leagueSubtitle,
  listedScorers,
  matchDetails,
  matchMetaLine,
  matchSummaryNote,
  ordinal,
  pickerSeasons,
  regressionExplanation,
  resultBadge,
  scorerCoverage,
  seasonSpan,
  staleFixtureNote,
  tableDate,
} from '../src/lib/wording.js';

const read = (path) => JSON.parse(readFileSync(new URL(path, import.meta.url), 'utf8'));

test('ordinals, the teens included', () => {
  const shown = [1, 2, 3, 4, 11, 12, 13, 21, 22, 23, 101, 111, 112].map(ordinal);
  assert.deepEqual(shown, ['1st', '2nd', '3rd', '4th', '11th', '12th', '13th', '21st', '22nd', '23rd', '101st', '111th', '112th']);
});

test('the league position line, or none when the table does not list us', () => {
  assert.equal(leaguePosition({ position: 2, teams: 9, points: 11, played: 9 }), '2nd of 9 · 11 points from 9');
  assert.equal(leaguePosition(null), null);
});

test('scorers are listed with a tally only where there is one', () => {
  assert.equal(listedScorers([{ player: 'Daniel', goals: 2 }, { player: 'Elia', goals: 1 }]), 'Daniel (2), Elia');
  assert.equal(listedScorers([{ player: 'Elia', goals: 1 }]), 'Elia');
});

test('the badge is on the points', () => {
  assert.deepEqual([3, 1, 0].map(resultBadge), ['WIN', 'DRAW', 'LOSS']);
});

test('the kickoff line drops a part the league page left out, and is null with neither', () => {
  assert.equal(kickoffPitch({ kickoff: '20:00', pitch: 'Front Pitch' }), '20:00 · Front Pitch');
  assert.equal(kickoffPitch({ kickoff: '19:40', pitch: null }), '19:40');
  assert.equal(kickoffPitch({ kickoff: '', pitch: 'Pitch 1' }), 'Pitch 1');
  assert.equal(kickoffPitch({ kickoff: null, pitch: null }), null);
});

test('each fixture gets its date in words and its kickoff line, and keeps a missing opponent', () => {
  const fixtures = [{ date: '2026-12-02', opponent: null, kickoff: null, pitch: null, position: 3 }];
  assert.deepEqual(fixtureWords(fixtures), [{ date: '2026-12-02', date_label: 'Wed 02 Dec 2026', opponent: null, kickoff_pitch: null }]);
});

test('a season span in each of its cases', () => {
  const season = (first_match, last_match) => ({ start_date: '2026-09-09', first_match, last_match });
  assert.equal(seasonSpan(season('2025-09-10', '2026-01-28')), 'Sep 2025 – Jan 2026');
  assert.equal(seasonSpan(season('2026-02-04', '2026-08-19')), 'Feb–Aug 2026');
  assert.equal(seasonSpan(season('2026-09-09', '2026-09-30')), 'Sep 2026');
  // A season with no matches yet is described by its start.
  assert.equal(seasonSpan(season(null, null)), 'from Sep 2026');
});

test('the picker keeps each season as it was, with its caption in place of the dates', () => {
  const [season] = pickerSeasons([{ season_id: 's3', label: 'Season 3', start_date: '2026-09-09', first_match: '2026-09-09', last_match: '2026-11-11' }]);
  assert.deepEqual(season, { season_id: 's3', label: 'Season 3', start_date: '2026-09-09', span: 'Sep–Nov 2026' });
  assert.deepEqual(Object.keys(season), ['season_id', 'label', 'start_date', 'span']);
});

test('the freshness note says what it knows and drops the rest', () => {
  assert.deepEqual(freshnessNote({ last_result: '2026-11-11', scraped_on: '2026-11-17' }), {
    full: 'Results to 11 Nov 2026 · league page read 17 Nov 2026',
    short: 'Page read 17 Nov 2026',
  });
  assert.deepEqual(freshnessNote({ last_result: '2026-11-11', scraped_on: null }), { full: 'Results to 11 Nov 2026', short: 'Results to 11 Nov 2026' });
  assert.deepEqual(freshnessNote({ last_result: null, scraped_on: '2026-11-17' }), { full: 'league page read 17 Nov 2026', short: 'Page read 17 Nov 2026' });
  assert.equal(freshnessNote({ last_result: null, scraped_on: null }), null);
});

test('the stale note and the standings subtitle carry the scrape date, and need one', () => {
  assert.equal(staleFixtureNote('2026-11-03'), 'The fixture list was last read on 03 Nov, so this may be out of date.');
  assert.equal(staleFixtureNote(null), null);
  assert.equal(leagueSubtitle('2026-11-17'), "From the league's own page, as it stood on 17 November 2026.");
  assert.equal(leagueSubtitle(null), null);
});

const played = { date: '2026-11-11', result: '3-4', points: 0, opponent: 'Shamrock Posers', season_label: 'Season 3', squad_size: 7, scorers: [{ player: 'Daniel', goals: 2 }, { player: 'Elia', goals: 1 }], man_of_the_match: 'Daniel' };

test('a match line and details have what we hold, and drop the rest', () => {
  assert.equal(matchMetaLine(played), 'Wed 11 Nov 2026 · v Shamrock Posers · Season 3');
  assert.deepEqual(matchDetails(played), [
    { label: 'Squad', value: '7 players' },
    { label: 'Scorers', value: 'Daniel (2), Elia' },
    { label: 'Man of the match', value: 'Daniel' },
  ]);

  // Before the sync: no opponent and nothing from the event log. A match dated before the first
  // season has no season either. A squad of one is still "players", as R wrote it.
  const old = { ...played, date: '2025-09-10', opponent: null, season_label: null, squad_size: 1, scorers: [], man_of_the_match: null };
  assert.equal(matchMetaLine(old), 'Wed 10 Sep 2025');
  assert.deepEqual(matchDetails(old), [{ label: 'Squad', value: '1 players' }]);
});

test('the summary note under a picked result', () => {
  assert.equal(matchSummaryNote(played), '11 November 2026 — Shamrock Posers — Loss');
  assert.equal(matchSummaryNote({ date: '2025-09-10', opponent: null, points: 3 }), '10 September 2025 — Win');
  assert.equal(matchSummaryNote({ date: '2025-09-17', opponent: null, points: 1 }), '17 September 2025 — Draw');
});

test('which coverage line the scorers table gets', () => {
  assert.equal(scorerCoverage({ observed: 0, total: 20 }), 'No match in the selected seasons has its goals on file. They start with the sync.');
  assert.equal(scorerCoverage({ observed: 9, total: 9 }), 'All 9 matches in the selected seasons have their goals on file.');
  assert.equal(
    scorerCoverage({ observed: 9, total: 53 }),
    '9 of the 53 matches in the selected seasons have their goals on file. The rest were played before the sync existed and are left out, appearances included.',
  );
});

const profile = { player: 'Oleg', appearances: 3, matches: 24, turnout_display: '12%', covered: 9, goals: 1, mom: 0, differential: 0.4, differential_display: '0.40' };

test('the headline tiles: "3 of 24", R\'s turnout, and goals only once there are some on file', () => {
  assert.deepEqual(headlineTiles(profile), [
    { label: 'Appearances', value: '3 of 24', note: '12% turnout' },
    { label: 'Goals', value: '1', note: '0 man of the match, over 9 recorded' },
  ]);
  assert.deepEqual(headlineTiles({ ...profile, covered: 0, turnout_display: null }), [{ label: 'Appearances', value: '3 of 24', note: null }]);
});

test('the with-and-without note, either way, and with nothing to compare', () => {
  const tail = 'That is a description of how those nights went, not of what they did: it holds nothing else about the match fixed. The Effects view does hold the rest of the lineup fixed.';
  assert.equal(differentialNote(profile), `The side takes 0.40 points a match better with Oleg than without. ${tail}`);
  assert.equal(differentialNote({ ...profile, differential: -0.4 }), `The side takes 0.40 points a match worse with Oleg than without. ${tail}`);
  // No gap is not a worse one.
  assert.equal(differentialNote({ ...profile, differential: 0, differential_display: '0.00' }), `The side takes 0.00 points a match better with Oleg than without. ${tail}`);
  assert.equal(
    differentialNote({ ...profile, differential: null, differential_display: null }),
    'No comparison to draw: there is no match in the selected seasons on the other side of it.',
  );
});

test('the estimator note says what a coefficient is under each', () => {
  assert.equal(estimatorNote('ols'), 'Coefficient on each outcome, with its p-value in brackets.');
  assert.equal(estimatorNote('ridge'), 'Deviation from the average player, shrunk by how little we know.');
});

const regression = (observed, count = 2) => ({
  has_regression: true,
  min_appearances: 3,
  opponent_coverage: { observed, total: 12 },
  players: Array.from({ length: count }, (_, index) => ({ player: `P${index}` })),
});

test('the explanation counts the players and says whether the opponent is adjusted for', () => {
  const [fit, threshold, opponents] = regressionExplanation(regression(9, 24), 'ols');
  assert.match(fit, /^Each column is a separate match-level regression of that outcome on indicators for the 24 players with at least 3 appearances in the selected seasons\. /);
  assert.match(threshold, /^Anyone below the appearance threshold has no indicator of their own/);
  assert.match(opponents, /^The opponent is on file for 9 of 12 matches here\. Those matches also control for opponent strength: /);
  assert.equal(regressionExplanation(regression(0), 'ols').at(-1), 'No match in this window has its opponent on file, so nothing adjusts for who we played.');
});

test('the ridge explanation has its own two paragraphs, then the same two', () => {
  const paragraphs = regressionExplanation(regression(0, 17), 'ridge');
  assert.equal(paragraphs.length, 4);
  assert.match(paragraphs[0], /^These are ridge estimates\. The plain fit spends a column on each of the 17 players over a few dozen matches, /);
  assert.match(paragraphs[1], /^The estimates come out very close to zero, and that is the result rather than a display problem\. /);
  assert.deepEqual(paragraphs.slice(2), regressionExplanation(regression(0), 'ols').slice(1));
});

test('a table date, or a dash for none', () => {
  assert.equal(tableDate('2026-11-04'), '04 Nov 2026');
  assert.equal(tableDate(null), '—');
});

test('"To pay" for a balance owed and "In credit" for one in hand, from R\'s balance', () => {
  const fees = {
    fee_players: ['Ann', 'Bob', 'Cy'],
    balances: [
      { player: 'Ann', balance: 9.5, balance_display: '£9.50' },
      { player: 'Bob', balance: -2, balance_display: '-£2.00' },
      { player: 'Cy', balance: 0, balance_display: '£0.00' },
    ],
    overviews: [],
  };
  assert.deepEqual(feeRows(fees, FEE_STATUS).map((row) => row.status), ['To pay', 'In credit', 'Nothing to pay']);
});

// On the real files, so that a field the wording reads but the export dropped fails here.
test('every sentence on the real files has its values in place', () => {
  const meta = read('../src/generated/meta.json');
  const home = read('../src/generated/home.json');
  for (const season of pickerSeasons(meta.seasons)) assert.match(season.span, /^(from )?[A-Z][a-z]{2}/, season.season_id);
  for (const file of meta.subset_files) {
    const subset = read(`../public/data/seasons/${file}`);
    assert.doesNotMatch(scorerCoverage(subset.scorers.coverage), /undefined|null|NaN/, file);
    for (const profile of subset.profiles) {
      const words = [differentialNote(profile), ...headlineTiles(profile).flatMap((tile) => [tile.value, tile.note])];
      assert.doesNotMatch(words.join(' '), /undefined|NaN/, `${file} ${profile.player}`);
    }
    for (const estimator of ['ols', 'ridge']) {
      const block = subset.regression[estimator];
      if (block.has_regression) assert.doesNotMatch(regressionExplanation(block, estimator).join(' '), /undefined|null|NaN/, file);
    }
    for (const match of subset.matches) assert.doesNotMatch(matchSummaryNote(match), /undefined|null/, match.date);
  }
  if (home.last_match) assert.doesNotMatch([matchMetaLine(home.last_match), ...matchDetails(home.last_match).map((d) => d.value)].join(' '), /undefined|null/);
});
