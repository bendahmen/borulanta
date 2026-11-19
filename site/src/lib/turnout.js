// A rate as Turnout, to the nearest whole percent through toFixed(0), so 12.5% shows 13%, as
// it always has on the lineup and roster tables.
// The one display rounding in JS. Everything else shows R's own strings.
//
// R's round() is not the same on a tie: player_headline_ui() in R/cards.R gives 12% for a
// rate of 12.5%. No rate means an empty cell.
export function formatTurnout(rate) {
  if (rate === null || rate === undefined || Number.isNaN(rate)) return '';
  return `${(rate * 100).toFixed(0)}%`;
}
