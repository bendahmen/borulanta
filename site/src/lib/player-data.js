// The small decisions behind Players > Roster and the player pages. Nothing here computes a
// statistic: every figure is R's, and these only say who is on show and what goes where.
import { PLAYERS } from './labels.js';
import { firstRows } from './match-data.js';
import { readPlayer } from './player-picker.js';
import { scopeView } from './season-scope.js';
import { sortOnField } from './table-sort.js';

// The parts of a season's file these pages use. The matches are there for R's date strings.
export const pickPlayers = ({ has_matches, matches, attendance_list, profiles }) => ({
  has_matches,
  matches,
  attendance_list,
  profiles,
});

// The page length attendance_table() gives its pager, and the order it opens in.
export const FIRST_ROSTER = 15;
export const ROSTER_SORT = { key: 'attendance_rate', direction: 'descending' };

// Whoever was picked while they are in the window, then the fee picker's person, then
// whoever turns up most, which is R's first row whatever the table is sorted on. Worked out
// from the ticked window each time, so a tick change that drops someone falls back the same
// way as arriving does, and the row and the profile cannot disagree. The stored player is
// only ever read: "The two do not track each other."
export function selectedPlayer(roster, picked, stored) {
  const inWindow = (name) => roster.some((row) => row.player === name);
  if (inWindow(picked)) return picked;
  if (inWindow(stored)) return stored;
  return roster[0]?.player ?? null;
}

// What Roster remembers between ticks. The stored player is read once and never written here:
// an opening position rather than a link. `onchange` gets the whole state after every change.
export function createRosterSelection({ getStorage, onchange = () => {} }) {
  let state = { stored: null, picked: null };
  const update = (changes) => {
    state = { ...state, ...changes };
    onchange(state);
  };
  return {
    get state() {
      return state;
    },
    open() {
      update({ stored: readPlayer(getStorage) });
    },
    pick(player) {
      update({ picked: player });
    },
  };
}

// From R's order rather than from the screen, so the marks stay on the same people through
// a sort, as rank_class in attendance_table() does.
export const topThree = (roster) => new Set(roster.slice(0, 3).map((row) => row.player));

// The first rows of the table as sorted, and the selected player's row after them if it would
// otherwise be hidden: the profile below names them, so their row has to be there to say so.
export function shownRoster(sorted, selected, showingAll) {
  const first = firstRows(sorted, FIRST_ROSTER, showingAll);
  if (first.some((row) => row.player === selected)) return first;
  const row = sorted.find((candidate) => candidate.player === selected);
  return row ? [...first, row] : first;
}

export const findProfile = (subset, player) => subset?.profiles.find((profile) => profile.player === player) ?? null;

// Everything Roster shows for the file on the page, the selection and the table's own controls.
export function rosterView(subset, { picked, stored }, sort, showingAll) {
  const roster = subset?.attendance_list ?? [];
  const selected = selectedPlayer(roster, picked, stored);
  return {
    roster,
    selected,
    profile: findProfile(subset, selected),
    top: topThree(roster),
    rows: shownRoster(sortOnField(roster, sort), selected, showingAll),
  };
}

// What a player's page should be showing. A window can have matches and not this player, and
// then it says so rather than showing anybody else.
export function playerPageView(state, player) {
  const profile = findProfile(state.subset, player);
  const view = scopeView(state);
  return { view: view === 'ready' && !profile ? 'did-not-play' : view, profile };
}

// With and without, as with_without_table() lays it out. An empty side has no rate, which
// reaches the table as null and shows as a blank cell.
export function withWithoutRows(profile, sides) {
  return [
    { side: sides.played, ...profile.present },
    { side: sides.missed, ...profile.absent },
  ];
}

// The appearance strip, oldest first as R sorts it, with R's date strings: for each square, and
// as the two lists that say the same thing in words. `keyWords` are the strip's Played and Missed.
export function stripModel(timeline, matches, keyWords) {
  const dateStrings = new Map(matches.map((match) => [match.date, match.date_short]));
  const squares = timeline.map(({ date, played }) => {
    const dateShort = dateStrings.get(date) ?? date;
    return { date, played, dateShort, label: PLAYERS.stripSquare(dateShort, played ? keyWords.played : keyWords.missed) };
  });
  const datesWhere = (played) => squares.filter((square) => square.played === played).map((square) => square.dateShort);
  return {
    squares,
    first: squares[0]?.dateShort ?? null,
    last: squares.at(-1)?.dateShort ?? null,
    playedDates: datesWhere(true),
    missedDates: datesWhere(false),
  };
}

// The profile sits under the table, so a tap can change a card that is out of sight, or that
// shows only its heading above the tab bar. Where most of the screen's height is left for it,
// it is in view already, and the row just tapped stays where it is.
const REVEAL_BELOW = 0.6;

export function profileNeedsReveal(top, viewportHeight) {
  return top < 0 || top > viewportHeight * REVEAL_BELOW;
}
