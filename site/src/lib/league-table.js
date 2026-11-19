// The two tables on the League page, drawn at build time.
import { formatDate } from './dates.js';

// All eight columns of the standings. Enlarged text folds what the five middle ones say.
export function leagueColumns(headings) {
  return [
    { key: 'position', label: headings.position, value: (row) => row.position, number: true },
    { key: 'team', label: headings.team, value: (row) => row.team, lead: true },
    { key: 'played', label: headings.played, value: (row) => row.played, number: true, folds: true },
    { key: 'won', label: headings.won, value: (row) => row.won, number: true, folds: true },
    { key: 'drawn', label: headings.drawn, value: (row) => row.drawn, number: true, folds: true },
    { key: 'lost', label: headings.lost, value: (row) => row.lost, number: true, folds: true },
    { key: 'goal-difference', label: headings.goal_difference, value: (row) => row.goal_difference, number: true, folds: true },
    { key: 'points', label: headings.points, value: (row) => row.points, number: true },
  ];
}

// An opponent the standings do not carry has no position, and keeps its row with that cell
// empty.
export function runInColumns(headings) {
  return [
    { key: 'date', label: headings.date, value: (row) => formatDate(row.date, '%d %b %Y'), lead: true },
    { key: 'opponent', label: headings.opponent, value: (row) => row.opponent },
    { key: 'position', label: headings.position, value: (row) => row.position, number: true },
  ];
}
