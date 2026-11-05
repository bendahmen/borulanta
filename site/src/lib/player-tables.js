// The columns of the Players tables, each with R's headings. Each key is the exported field
// the column sorts on, and each value is R's: Turnout goes through formatTurnout(), and the
// averages are already rounded in the export. Nothing here rounds them again.
import { escapeHtml } from './data-table.js';
import { GO } from './match-tables.js';
import { playerHref } from './slug.js';
import { formatTurnout } from './turnout.js';

// All five of attendance_table(), then the way to the player's page. A phone keeps the name,
// Turnout and Avg points, and the goals open under the row. `top` is who gets the top three's
// mark, from topThree().
export function rosterColumns(headings, words, top) {
  return [
    { key: 'player', label: headings.player, value: (row) => row.player, lead: true, cue: (row) => (top.has(row.player) ? words.topThree : '') },
    { key: 'attendance_rate', label: headings.turnout, value: (row) => formatTurnout(row.attendance_rate), number: true },
    { key: 'avg_points', label: headings.avg_points, value: (row) => row.avg_points, number: true },
    { key: 'avg_goals_scored', label: headings.avg_goals_scored, value: (row) => row.avg_goals_scored, number: true, folds: true },
    { key: 'avg_goals_conceded', label: headings.avg_goals_conceded, value: (row) => row.avg_goals_conceded, number: true, folds: true },
    {
      key: 'page',
      label: words.playerPage,
      hideLabel: true,
      unsorted: true,
      value: (row) => row.player,
      html: (row) => `<a class="dt-go" href="${escapeHtml(playerHref(row.player))}" aria-label="${escapeHtml(words.playerPageFor(row.player))}">${GO}</a>`,
    },
  ];
}

// All eight of with_without_table(), whose first heading is blank. The rate is R's rounded
// one, or nothing on an empty side.
export function withWithoutColumns(headings, words) {
  return [
    { key: 'side', label: words.sideHeading, hideLabel: true, value: (row) => row.side, lead: true },
    { key: 'played', label: headings.played, value: (row) => row.played, number: true },
    { key: 'won', label: headings.won, value: (row) => row.won, number: true },
    { key: 'drawn', label: headings.drawn, value: (row) => row.drawn, number: true },
    { key: 'lost', label: headings.lost, value: (row) => row.lost, number: true },
    { key: 'goals_for', label: headings.goals_for, value: (row) => row.goals_for, number: true },
    { key: 'goals_against', label: headings.goals_against, value: (row) => row.goals_against, number: true },
    { key: 'points_per_match', label: headings.points_per_match, value: (row) => row.points_per_match_rounded, number: true },
  ];
}
