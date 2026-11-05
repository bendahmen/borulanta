// The small decisions behind Results, Trends and the match pages. Nothing here computes a
// statistic: every figure is R's, and these only say which row is on show and where it goes.

// The parts of a season's file these pages use. The rest is for Effects and the roster.
export const pickMatches = ({ has_matches, matches, season_form, match_details, scorers, opponent_record }) => ({
  has_matches,
  matches,
  season_form,
  match_details,
  scorers,
  opponent_record,
});

// The page lengths match_table() and scorer_display_table() give their pagers.
export const FIRST_RESULTS = 10;
export const FIRST_SCORERS = 15;

export function firstRows(rows, first, showingAll) {
  return showingAll ? rows : rows.slice(0, first);
}

export const matchHref = (date) => `/matches/${date}/`;

// `matches` come newest first. Falling back to the first row, rather than to nothing, keeps
// the card filled on arrival and when the ticks move on, as selected_match_date() in app.R:
// an empty card under a full table reads as a fault.
export function selectedMatchDate(matches, picked) {
  if (matches.some((match) => match.date === picked)) return picked;
  return matches[0]?.date ?? null;
}

// The row and the lineup for one date of a season's file, or null with nothing to show.
export function matchDetail(subset, date) {
  const match = subset?.matches.find((row) => row.date === date);
  const detail = subset?.match_details.find((row) => row.date === date);
  return match && detail ? { match, summaryNote: detail.summary_note, lineup: detail.lineup } : null;
}

// The detail sits above the table. A tap well down the table would otherwise change a card
// nobody can see, while a card already on screen stays where it is.
export function needsReveal(top, viewportHeight) {
  return top < 0 || top >= viewportHeight;
}

export function revealElement(element, win, needs = needsReveal) {
  if (!needs(element.getBoundingClientRect().top, win.innerHeight)) return;
  const still = win.matchMedia('(prefers-reduced-motion: reduce)').matches;
  element.scrollIntoView({ behavior: still ? 'auto' : 'smooth', block: 'start' });
}
