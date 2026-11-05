// Every label not exported from R. Words that R owns come from src/generated/.

export const SITE = {
  name: 'Borulanta',
  // These two are the words in app.R, which writes them inline and exports neither.
  kicker: 'Wednesday Football',
  strapline: 'Who is next, where we left off, and what you owe.',
  liveUrl: 'https://bendahmen-borulanta.share.connect.posit.cloud/',
};

export const NAV = [
  { id: 'home', label: 'Home', href: '/' },
  { id: 'fees', label: 'Fees', href: '/fees/' },
  { id: 'league', label: 'League', href: '/league/' },
  { id: 'matches', label: 'Matches', href: '/matches/' },
  // Opens on Effects, as Shiny does.
  { id: 'players', label: 'Players', href: '/players/effects/' },
];

export const NAV_LABEL = 'Main';

export const PLAYERS_NAV = [
  { id: 'roster', label: 'Roster', href: '/players/roster/' },
  { id: 'effects', label: 'Effects', href: '/players/effects/' },
];

export const PLAYERS_NAV_LABEL = 'Players';

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
};

export const STUB = {
  tag: 'Prototype',
  text: 'This page is not in the prototype yet. It is all still on the live app.',
  link: 'Open the live app',
};

// The words in season_picker() in R/cards.R and player_regression_table() in R/analysis.R,
// which write them inline and export neither, plus the new copy for a failed fetch.
export const EFFECTS = {
  seasons: 'Seasons',
  player: 'Player',
  appearances: 'Appearances',
  playerNames: 'player names',
  loadFailed: 'These seasons did not load.',
  retry: 'Try again',
  moreFor: (player) => `Other outcomes for ${player}`,
};

export const PAGES = {
  home: { title: SITE.name, description: SITE.strapline },
  fees: { title: 'Fees', description: 'Fees for the Wednesday football game.' },
  league: { title: 'League', description: 'The league table and the run-in.' },
  matches: { title: 'Matches', description: 'Results and trends.' },
  roster: { title: 'Roster', description: 'Who turns up.' },
  effects: { title: 'Effects', description: 'Player effects.' },
};
