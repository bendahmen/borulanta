// Every label not exported from R. Words that R owns come from src/generated/.

export const SITE = {
  name: 'Borulanta',
  // These two are the words in app.R, which writes them inline and exports neither.
  kicker: 'Wednesday Football',
  strapline: 'Who is next, where we sit, and who is scoring.',
};

// New copy for the home page's two tables. The league headings are R's, in wording.json.
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
  // Opens on Trends, as Shiny does.
  { id: 'matches', label: 'Matches', href: '/matches/trends/' },
  // Opens on Effects, as Shiny does.
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

// Any page with the season picker. The legend is the word in season_picker() in R/cards.R,
// which writes it inline and does not export it. The rest is new copy for a failed fetch.
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

// New copy for Fees > You. match_charge_table() sets no empty wording of its own, so
// Shiny falls back to DataTables' default there.
export const FEES = {
  loading: 'Loading',
  loadFailed: 'Your charges and payments did not load.',
  retry: 'Try again',
  noCharges: 'No match charges yet.',
  showAll: 'Show all {count}',
  showFewer: 'Show the latest {count}',
};

// New copy for Matches and the match pages. match_lineup_table() sets no empty wording of
// its own, and a synced match has no lineup until the attendance is typed in.
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

// New copy for Roster and the player pages. The cards' own wording is R's, in wording.json.
export const PLAYERS = {
  // Beside the name, so the top three are not carried by the tint alone.
  topThree: 'Top 3',
  // R leaves this heading blank. A screen reader still needs one.
  sideHeading: 'Played or missed',
  foldFor: (player) => `Goals for ${player}`,
  playerPage: 'Player page',
  playerPageFor: (player) => `Player page for ${player}`,
  // A synced match has no attendance until it is typed in, so a window can have matches and
  // nobody in them. attendance_table() sets no empty wording of its own.
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
