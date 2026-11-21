// The site's fixed words: all of them used in two or more places, and others that belong with
// a set here, such as the navigation. A word used in one place can sit in the component that
// shows it instead. The only words in src/generated/ are ones that depend on the data.

export const SITE = {
  name: 'Borulanta',
  kicker: 'Wednesday Football',
  strapline: 'Who is next, where we sit, and who is scoring.',
};

// What a season-scoped page says instead of zeros when the ticked seasons hold nothing to show.
export const EMPTY_STATES = {
  no_matches: 'No matches in the selected seasons yet.',
  no_regression: 'Not enough matches in the selected seasons to estimate player effects. Tick more seasons.',
  no_opponent: 'No match in the selected seasons has its opponent on file. They start with the sync.',
  no_scorers: 'No goals on file yet — they arrive with the sync.',
};

// The words around the next fixture. The script below the card picks which to show, because
// which one applies depends on today. `{days}` stands for the count.
export const NEXT_MATCH = {
  tag: 'Next match',
  no_fixtures_left: 'No fixtures left on the league page. Run the sync once the new season is up.',
  no_fixture_list: 'No fixture list yet — run the sync to pull one in.',
  opponent_unknown: 'Opponent to be confirmed',
  tonight: 'Tonight',
  tomorrow: 'Tomorrow',
  in_days: 'In {days} days',
};

// The League page's two cards. The standings' subtitle depends on the scrape date, so it is
// leagueSubtitle() in wording.js. Home's snippet uses the same headings.
export const LEAGUE_CARDS = {
  standings: {
    tag: 'League table',
    title: 'Where we sit',
    empty: 'No league table yet — run the sync to pull one in.',
  },
  headings: {
    position: 'Pos',
    team: 'Team',
    played: 'P',
    won: 'W',
    drawn: 'D',
    lost: 'L',
    goal_difference: 'GD',
    points: 'Pts',
  },
  run_in: {
    tag: 'Run-in',
    title: 'Who is left',
    subtitle: 'Every fixture still to come, with where that side sits in the league.',
    headings: {
      date: 'Date',
      opponent: 'Opponent',
      position: 'Pos',
    },
    empty: 'No fixtures left on the league page.',
  },
};

// The two tables on Fees > You.
export const FEE_HISTORY_CARDS = {
  payments: {
    tag: 'Payments',
    title: 'Payments made',
    subtitle: null,
    headings: {
      date: 'Date',
      payment: 'Payment',
    },
  },
  charges: {
    tag: 'Charges',
    title: 'Match-by-match charges',
    subtitle: 'Shares are rounded to the penny here. The totals above are not.',
    headings: {
      date: 'Date',
      season: 'Season',
      result: 'Result',
      played: 'Played?',
      squad_size: 'Squad size',
      charge: 'Charge',
      explanation: 'Explanation',
    },
    played: {
      yes: 'Yes',
      no: 'No',
    },
  },
};

// Fees > Everyone.
export const EVERYONE_CARD = {
  tag: 'Everyone',
  title: 'Who is square and who is not',
  subtitle: 'Rows in amber owe something. Rows in green are owed.',
  tiles: {
    outstanding: 'Still owed',
    credit: 'In credit',
    settled: 'Settled up',
  },
  headings: {
    player: 'Player',
    charges: 'Charges',
    payments: 'Payments',
    last_payment: 'Last payment',
    balance: 'Balance',
  },
  empty: 'Nobody has been charged anything yet.',
};

// The cards on Matches > Trends and Results, and on the match pages.
export const MATCHES_CARDS = {
  season_form: {
    tag: 'Season form',
    title: 'How the season is trending',
    subtitle: 'Each point averages the last five matches. The first four average what there is.',
    metric_label: 'Show',
    metrics: [
      { value: 'points', label: 'Points per match' },
      { value: 'goals', label: 'Goals scored and conceded' },
      { value: 'goal_difference', label: 'Goal difference' },
      { value: 'squad_size', label: 'Squad size' },
    ],
  },
  // The subtitle is the coverage line, scorerCoverage() in wording.js.
  scorers: {
    tag: 'Scorers',
    title: 'Goals and man of the match',
    subtitle: null,
    headings: {
      player: 'Player',
      goals: 'Goals',
      mom: 'MOM',
      appearances: 'Appearances',
      goals_per_game: 'Goals per game',
    },
  },
  head_to_head: {
    tag: 'Head to head',
    title: 'How we do against each side',
    subtitle: 'Points per match rather than total points: we have met some sides twice as often as others.',
    headings: {
      opponent: 'Opponent',
      played: 'P',
      won: 'W',
      drawn: 'D',
      lost: 'L',
      goals_for: 'GF',
      goals_against: 'GA',
      goal_difference: 'GD',
      points_per_match: 'Pts/match',
    },
  },
  every_result: {
    tag: 'Matches',
    title: 'Every result',
    subtitle: "Newest first, for the seasons you have ticked. Pick a row to see that night's lineup above.",
    headings: {
      date: 'Date',
      opponent: 'Opponent',
      result: 'Result',
      outcome: 'Outcome',
    },
    no_opponent: '—',
  },
  match_detail: {
    tag: 'Match detail',
    title: 'Lineup and season context',
    subtitle: 'Who played, and how much of that season they turned up for.',
    result_label: 'Result',
    lineup_title: 'Lineup',
    headings: {
      player: 'Player',
      season_appearances: 'Season appearances',
      turnout: 'Turnout',
    },
  },
};

// What the Season form plot calls its axis for each metric, and the key on the one metric
// with two lines, named by the column each line draws.
export const SEASON_FORM_LABELS = {
  points: { x: null, y: 'Points per match' },
  goals: { x: null, y: 'Goals per match' },
  goal_difference: { x: null, y: 'Goal difference per match' },
  squad_size: { x: null, y: 'Players per match' },
  series: {
    rolling_goals_scored: 'Goals scored',
    rolling_goals_conceded: 'Goals conceded',
  },
};

// The cards on Players > Roster and the player pages.
export const PLAYERS_CARDS = {
  who_turns_up: {
    tag: 'Attendance',
    title: 'Who turns up',
    subtitle: "Share of matches played, and how those matches went. Pick a row for that player's page.",
    headings: {
      player: 'Player',
      turnout: 'Turnout',
      avg_points: 'Avg points',
      avg_goals_scored: 'Avg goals scored',
      avg_goals_conceded: 'Avg goals conceded',
    },
  },
  // The subtitle is the player's name and then note_after_name.
  player: {
    tag: 'Player',
    title: 'One player at a time',
    subtitle: null,
    note_after_name: '. Pick another row above to change who this is about.',
    appearances_title: 'Appearances',
    strip_key: {
      played: 'Played',
      missed: 'Missed',
    },
  },
  // The subtitle is differentialNote() in wording.js, for each player's profile.
  with_without: {
    tag: 'With and without',
    title: 'How the side does either way',
    subtitle: null,
    headings: {
      side: ' ',
      played: 'P',
      won: 'W',
      drawn: 'D',
      lost: 'L',
      goals_for: 'GF',
      goals_against: 'GA',
      points_per_match: 'Pts/match',
    },
    sides: {
      played: 'Played',
      missed: 'Missed',
    },
  },
};

// New copy for the home page's two tables. The league headings are LEAGUE_CARDS.headings.
export const HOME = {
  league: { tag: 'League', title: 'Where we sit', link: 'Full table and run-in' },
  stats: {
    tag: 'Players',
    title: 'Who is scoring',
    caption: 'Player stats',
    headings: { player: 'Player', goals: 'Goals', turnout: 'Turnout', gd_effect: 'GD effect' },
    subtitle: ({ observed, total, minAppearances }) =>
      `Goals from the ${observed} of ${total} matches with scorers on file. Turnout and GD effect over all seasons: ` +
      `the OLS coefficient on goal difference, p-value underneath, for anyone with ${minAppearances} or more appearances.`,
    effectsLink: 'All player effects',
    pValue: (p) => `p ${p}`,
  },
};

export const NAV = [
  { id: 'home', label: 'Home', href: '/' },
  { id: 'fees', label: 'Fees', href: '/fees/' },
  { id: 'league', label: 'League', href: '/league/' },
  // Opens on Trends, where it always has.
  { id: 'matches', label: 'Matches', href: '/matches/trends/' },
  // Opens on Effects, where it always has.
  { id: 'players', label: 'Players', href: '/players/effects/' },
];

export const NAV_LABEL = 'Main';

export const PLAYERS_NAV = [
  { id: 'roster', label: 'Roster', href: '/players/roster/' },
  { id: 'effects', label: 'Effects', href: '/players/effects/' },
];

export const PLAYERS_NAV_LABEL = 'Players';

export const MATCHES_NAV = [
  { id: 'results', label: 'Results', href: '/matches/results/' },
  { id: 'trends', label: 'Trends', href: '/matches/trends/' },
];

export const MATCHES_NAV_LABEL = 'Matches';

export const FEES_NAV = [
  { id: 'you', label: 'You', href: '/fees/' },
  { id: 'everyone', label: 'Everyone', href: '/fees/everyone/' },
];

export const FEES_NAV_LABEL = 'Fees';

export const SKIP_LINK = 'Skip to content';

// Beside the balance, so owing is not carried by colour alone.
export const FEE_STATUS = {
  owed: 'To pay',
  square: 'Nothing to pay',
  credit: 'In credit',
};

export const NO_SCRIPT = {
  fixtures: 'Your browser is not running scripts, so here is every fixture on the list.',
  balance: 'Your browser is not running scripts, so your balance cannot show here.',
  effects: 'Your browser is not running scripts, so the player effects cannot show here.',
  feeHistory: 'Your browser is not running scripts, so your charges and payments cannot show here. Everyone has every balance.',
  results: 'Your browser is not running scripts, so the results cannot show here.',
  trends: 'Your browser is not running scripts, so the trends cannot show here.',
  roster: 'Your browser is not running scripts, so the roster cannot show here.',
  player: 'Your browser is not running scripts, so this player cannot show here.',
};

// Any page with the season picker: its legend, and the words for a failed fetch.
export const SEASON_SCOPE = {
  legend: 'Seasons',
  loadFailed: 'These seasons did not load.',
  retry: 'Try again',
};

// The words in player_regression_table() in R/analysis.R, which writes them inline and does
// not export them.
export const EFFECTS = {
  player: 'Player',
  appearances: 'Appearances',
  playerNames: 'player names',
  moreFor: (player) => `Other outcomes for ${player}`,
  separateAxes: 'The two axes use different scales here: one set of estimates is far smaller than the other.',
};

// Fees > You, around its two tables. noPayments and noCharges are their empty states.
export const FEES = {
  loading: 'Loading',
  loadFailed: 'Your charges and payments did not load.',
  retry: 'Try again',
  noCharges: 'No match charges yet.',
  showAll: 'Show all {count}',
  showFewer: 'Show the latest {count}',
  noPayments: 'No payments recorded yet.',
};

// Words on Matches and the match pages beyond the cards' own. A synced match has no lineup
// until the attendance is typed in.
export const MATCHES = {
  showAll: 'Show all {count}',
  showFewer: 'Show the first {count}',
  matchPage: 'Match page',
  matchPageFor: (date) => `Match page for ${date}`,
  openMatchPage: 'Open this match on its own page',
  noLineup: 'No lineup on file for this match yet.',
  backToResults: 'All results',
  otherMatches: 'Other matches',
  previous: 'Previous match',
  next: 'Next match',
  lineupSeason: (season) => `Season appearances and Turnout are counted over ${season} alone.`,
};

// Words on Roster and the player pages beyond the cards' own, which are PLAYERS_CARDS.
export const PLAYERS = {
  // Beside the name, so the top three are not carried by the tint alone.
  topThree: 'Top 3',
  // R leaves this heading blank. A screen reader still needs one.
  sideHeading: 'Played or missed',
  foldFor: (player) => `Goals for ${player}`,
  playerPage: 'Player page',
  playerPageFor: (player) => `Player page for ${player}`,
  // A synced match has no attendance until it is typed in, so a window can have matches and
  // nobody in them.
  noRoster: 'Nobody has a turnout on file for these seasons yet.',
  didNotPlay: (player) => `${player} did not turn out in the selected seasons.`,
  backToRoster: 'Roster',
  stripSummary: (appearances, matches) => `Played ${appearances} of ${matches} matches.`,
  stripSpan: (first, last) => (first === last ? first : `${first} to ${last}, oldest first`),
  stripSquare: (date, side) => `${date}: ${side}`,
  stripDates: 'Match dates',
  // Link previews stay up for months and the ticks change what the page says, so no numbers.
  description: (player) => `How often ${player} turns out, which matches they played, and how the side does with and without them.`,
};

export const NOT_FOUND = {
  tag: 'Not found',
  text: 'There is nothing at this address. It may be an old or mistyped link.',
  link: 'Go to Home',
};

export const PAGES = {
  home: { title: SITE.name, description: SITE.strapline },
  you: {
    title: 'Your fees',
    description: 'Your balance, your match charges and payments, and how to settle up.',
  },
  everyone: {
    title: "Everyone's fees",
    description: 'Who is square and who is not, with every balance and the totals.',
  },
  league: {
    title: 'League',
    description: 'Where we sit in the league table, and who is left to play.',
  },
  results: {
    title: 'Results',
    description: 'Every result, and who played on the night.',
  },
  trends: {
    title: 'Trends',
    description: 'How the season is trending, who is scoring and how we do against each side.',
  },
  roster: {
    title: 'Roster',
    description: 'Who turns up, and any one player: their appearances, and how the side does with and without them.',
  },
  effects: { title: 'Effects', description: 'Player effects.' },
  notFound: { title: 'Page not found', description: 'There is nothing at this address.' },
};
