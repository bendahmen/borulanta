// The columns of the Matches tables, each with R's headings. Lineups are on Results and on
// the match pages, so this is the one copy.
import { escapeHtml } from './data-table.js';
import { matchHref } from './match-data.js';
import { playerHref } from './slug.js';
import { formatTurnout } from './turnout.js';

// The live site's win, draw and loss colours, keyed on R's own badge word. See global.css.
export const badgeClass = (badge) => `badge badge-${String(badge).toLowerCase()}`;

export const GO = '<svg viewBox="0 0 24 24" width="18" height="18" aria-hidden="true" focusable="false"><path d="m9 6 6 6-6 6"/></svg>';

// A chevron rather than words, because a phone has no room for them. The whole row picks the
// match, and this is the way to its page.
export function everyResultColumns(card, words) {
  const { headings } = card;
  return [
    { key: 'date', label: headings.date, value: (row) => row.date_short, lead: true },
    { key: 'opponent', label: headings.opponent, value: (row) => row.opponent ?? card.no_opponent },
    { key: 'result', label: headings.result, value: (row) => row.result, number: true },
    {
      key: 'outcome',
      label: headings.outcome,
      value: (row) => row.badge,
      html: (row) => `<span class="${escapeHtml(badgeClass(row.badge))}">${escapeHtml(row.badge)}</span>`,
    },
    {
      key: 'page',
      label: words.matchPage,
      hideLabel: true,
      value: (row) => row.date,
      html: (row) => `<a class="dt-go" href="${escapeHtml(matchHref(row.date))}" aria-label="${escapeHtml(words.matchPageFor(row.date_short))}">${GO}</a>`,
    },
  ];
}

// Each key is the exported field the column sorts on. The Goals per game value shown is R's
// rounded one and the sort uses the full one, so ties fall as the rate says.
export function scorerColumns(headings) {
  return [
    { key: 'player', label: headings.player, value: (row) => row.player, lead: true },
    { key: 'goals', label: headings.goals, value: (row) => row.goals, number: true },
    { key: 'mom', label: headings.mom, value: (row) => row.mom, number: true },
    { key: 'appearances', label: headings.appearances, value: (row) => row.appearances, number: true },
    { key: 'goals_per_appearance', label: headings.goals_per_game, value: (row) => row.goals_per_appearance_rounded, number: true },
  ];
}

// All nine of opponent_record_table(). A phone keeps the record and the column the table is
// ranked on, and the goals open under the row.
export function opponentColumns(headings) {
  return [
    { key: 'opponent', label: headings.opponent, value: (row) => row.opponent, lead: true },
    { key: 'played', label: headings.played, value: (row) => row.played, number: true },
    { key: 'won', label: headings.won, value: (row) => row.won, number: true },
    { key: 'drawn', label: headings.drawn, value: (row) => row.drawn, number: true },
    { key: 'lost', label: headings.lost, value: (row) => row.lost, number: true },
    { key: 'goals-for', label: headings.goals_for, value: (row) => row.goals_for, number: true, folds: true },
    { key: 'goals-against', label: headings.goals_against, value: (row) => row.goals_against, number: true, folds: true },
    { key: 'goal-difference', label: headings.goal_difference, value: (row) => row.goal_difference, number: true, folds: true },
    { key: 'points-per-match', label: headings.points_per_match, value: (row) => row.points_per_match_rounded, number: true },
  ];
}

// As match_lineup_table(). Enlarged text folds the appearances. A name is its player's page.
export function lineupColumns(headings) {
  return [
    { key: 'player', label: headings.player, value: (row) => row.player, lead: true, href: (row) => playerHref(row.player) },
    { key: 'season-appearances', label: headings.season_appearances, value: (row) => row.season_appearances, number: true, folds: true },
    { key: 'turnout', label: headings.turnout, value: (row) => formatTurnout(row.attendance_rate), number: true },
  ];
}
