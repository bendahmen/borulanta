// The words that depend on the data: the small composed strings and the sentences around R's
// values. R exports raw values, dates as ISO strings, and a display string for each figure it
// rounds or writes as money, so nothing here rounds, adds up or estimates. The careful
// statistical wording says what the numbers do not show, so it changes only with Ben's say.
// Missing values arrive as null.
import { formatDate } from './dates.js';

const MIDDLE_DOT = ' · ';

// 1st, 2nd, 3rd, 4th, ... 11th, 12th, 13th, 21st.
export function ordinal(n) {
  const tens = n % 100;
  const suffix = tens >= 11 && tens <= 13 ? 'th' : ({ 1: 'st', 2: 'nd', 3: 'rd' }[n % 10] ?? 'th');
  return `${n}${suffix}`;
}

// A comma list that reads like a sentence, with a tally where there is one: "Ada (2), Ben".
export function listedScorers(scorers) {
  return scorers.map(({ player, goals }) => (goals > 1 ? `${player} (${goals})` : player)).join(', ');
}

export function resultBadge(points) {
  if (points === 3) return 'WIN';
  if (points === 1) return 'DRAW';
  return 'LOSS';
}

const outcomeWord = (points) => ({ 3: 'Win', 1: 'Draw' }[points] ?? 'Loss');

// "20:20 · Pitch 1", or null when the league page gave neither.
export function kickoffPitch({ kickoff, pitch }) {
  const parts = [kickoff, pitch].filter((part) => part !== null && part !== undefined && part !== '');
  return parts.length > 0 ? parts.join(MIDDLE_DOT) : null;
}

// When a season actually ran, as a caption for the picker: the span of matches played, or its
// start for a season with none yet. Within one calendar year the year only needs saying once.
export function seasonSpan({ start_date, first_match, last_match }) {
  if (!first_match) return `from ${formatDate(start_date, '%b %Y')}`;
  const first = formatDate(first_match, '%b %Y');
  const last = formatDate(last_match, '%b %Y');
  if (first === last) return first;
  if (first_match.slice(0, 4) === last_match.slice(0, 4)) return `${formatDate(first_match, '%b')}–${last}`;
  return `${first} – ${last}`;
}

// What the next-match card says about each fixture that does not depend on today, written at
// build time for the script that picks one.
export function fixtureWords(fixtures) {
  return fixtures.map((fixture) => ({
    date: fixture.date,
    date_label: formatDate(fixture.date, '%a %d %b %Y'),
    opponent: fixture.opponent,
    kickoff_pitch: kickoffPitch(fixture),
  }));
}

// meta.seasons as the picker takes them, each with its caption.
export function pickerSeasons(seasons) {
  return seasons.map((season) => ({
    season_id: season.season_id,
    label: season.label,
    start_date: season.start_date,
    span: seasonSpan(season),
  }));
}

// Where the numbers came from and when, under the strapline, or null with neither date. The
// stylesheet shows the full line or the short one. The short one keeps the scrape date,
// because the countdown beneath it depends on that.
export function freshnessNote({ last_result, scraped_on }) {
  const results = last_result ? `Results to ${formatDate(last_result, '%d %b %Y')}` : null;
  const read = scraped_on ? formatDate(scraped_on, '%d %b %Y') : null;
  if (!results && !read) return null;
  return {
    full: [results, read && `league page read ${read}`].filter(Boolean).join(MIDDLE_DOT),
    short: read ? `Page read ${read}` : results,
  };
}

// The warning on the countdown, for the browser to show once the list has gone stale. The
// league table's scrape date stands in for the fixture list's own, which it does not have.
export function staleFixtureNote(scrapedOn) {
  if (!scrapedOn) return null;
  return `The fixture list was last read on ${formatDate(scrapedOn, '%d %b')}, so this may be out of date.`;
}

// "4th of 9 · 11 points from 9", or null when the table does not list us.
export function leaguePosition(position) {
  if (!position) return null;
  return `${ordinal(position.position)} of ${position.teams}${MIDDLE_DOT}${position.points} points from ${position.played}`;
}

// The standings' subtitle, dated because the table ages between syncs. Null with no table.
export function leagueSubtitle(scrapedOn) {
  if (!scrapedOn) return null;
  return `From the league's own page, as it stood on ${formatDate(scrapedOn, '%d %B %Y')}.`;
}

// The line under a result's score, dropping what we do not hold.
export function matchMetaLine(match) {
  return [formatDate(match.date, '%a %d %b %Y'), match.opponent && `v ${match.opponent}`, match.season_label]
    .filter(Boolean)
    .join(MIDDLE_DOT);
}

// The squad, and the scorers and man of the match where the event log has them. Anything
// played before the sync has neither, and a line reading "Scorers: —" on every old match is
// worse than no line. The squad always comes first.
export function matchDetails(match) {
  return [
    { label: 'Squad', value: `${match.squad_size} players` },
    match.scorers.length > 0 && { label: 'Scorers', value: listedScorers(match.scorers) },
    match.man_of_the_match && { label: 'Man of the match', value: match.man_of_the_match },
  ].filter(Boolean);
}

// The note under the Result tile on Matches > Results, for one row of a season's matches.
export function matchSummaryNote(match) {
  return [formatDate(match.date, '%d %B %Y'), match.opponent, outcomeWord(match.points)].filter(Boolean).join(' — ');
}

// What the scorers table covers. For anything played before the sync it is not all-time,
// and a table that does not say so invites being read as if it were.
export function scorerCoverage({ observed, total }) {
  if (observed === 0) return 'No match in the selected seasons has its goals on file. They start with the sync.';
  if (observed === total) return `All ${total} matches in the selected seasons have their goals on file.`;
  return (
    `${observed} of the ${total} matches in the selected seasons have their goals on file. ` +
    'The rest were played before the sync existed and are left out, appearances included.'
  );
}

// A player's season at a glance. Goals only earn a tile once there is a match they could have
// been scored in: before that the sync has not run, and a zero would read as a barren season.
export function headlineTiles(profile) {
  return [
    {
      label: 'Appearances',
      value: `${profile.appearances} of ${profile.matches}`,
      note: profile.turnout_display ? `${profile.turnout_display} turnout` : null,
    },
    profile.covered > 0 && {
      label: 'Goals',
      value: String(profile.goals),
      note: `${profile.mom} man of the match, over ${profile.covered} recorded`,
    },
  ].filter(Boolean);
}

// The gap with its caveat. It compares the nights they turned up with the nights they did not,
// and nothing about that holds the rest of the lineup, the opponent or the squad size fixed.
export function differentialNote(profile) {
  if (profile.differential === null) {
    return 'No comparison to draw: there is no match in the selected seasons on the other side of it.';
  }
  const direction = profile.differential >= 0 ? 'better' : 'worse';
  return (
    `The side takes ${profile.differential_display} points a match ${direction} with ${profile.player} than without. ` +
    'That is a description of how those nights went, not of what they did: it holds nothing else about the match fixed. ' +
    'The Effects view does hold the rest of the lineup fixed.'
  );
}

// What a coefficient means, which is not the same under both estimators. The OLS fit has no
// intercept, so a coefficient is a player's additive share of the scoreline. The ridge fit has
// one, so a coefficient is their deviation from the average player.
export function estimatorNote(estimator) {
  return estimator === 'ridge'
    ? 'Deviation from the average player, shrunk by how little we know.'
    : 'Coefficient on each outcome, with its p-value in brackets.';
}

// How to read the player effects, as paragraphs, for one estimator's block of a season file.
// Only for a block with a fit: there is nothing to explain without one.
export function regressionExplanation(regression, estimator) {
  const count = regression.players.length;
  const { observed, total } = regression.opponent_coverage;

  const estimatorParagraphs =
    estimator === 'ridge'
      ? [
          `These are ridge estimates. The plain fit spends a column on each of the ${count} players over a few dozen matches, ` +
            'so somebody with three appearances is measured off three nights and lands at whichever extreme those nights went. ' +
            'Ridge pulls every estimate toward the average player by an amount inversely proportional to how much is known ' +
            'about them, so the thin players move a long way and the regulars barely move. A coefficient here is a deviation ' +
            'from the average player rather than a share of the scoreline, and there are no confidence intervals: ridge has ' +
            'no usable standard error.',
          'The estimates come out very close to zero, and that is the result rather than a display problem. The penalty is ' +
            'chosen by cross-validation, which is asking how much of a match the lineup predicts out of sample, and the ' +
            'answer here is almost none. The ordering still means something; the magnitudes are telling you not to lean on it.',
        ]
      : [
          `Each column is a separate match-level regression of that outcome on indicators for the ${count} players with at ` +
            `least ${regression.min_appearances} appearances in the selected seasons. The coefficient says how the outcome ` +
            'moves when that player is on the pitch, holding the rest of the lineup fixed. With this many players, a couple ' +
            'of p-values below 0.05 is what chance alone produces.',
        ];

  const opponentParagraph =
    observed === 0
      ? 'No match in this window has its opponent on file, so nothing adjusts for who we played.'
      : `The opponent is on file for ${observed} of ${total} matches here. Those matches also control for opponent ` +
        "strength: the other side's goal difference per game against everyone except us that season, centred so that 0 " +
        'is an average opponent. Matches with no opponent recorded take the average and an indicator, which keeps their ' +
        'own level out of the player coefficients.';

  return [
    ...estimatorParagraphs,
    'Anyone below the appearance threshold has no indicator of their own and sits in the residual. There is no time ' +
      'trend. These are descriptive: nothing here separates a good player from one who happens to play in good teams.',
    opponentParagraph,
  ];
}

// A date in a table, or "—" for none, as for a player who has never paid.
export function tableDate(iso) {
  return iso ? formatDate(iso, '%d %b %Y') : '—';
}
