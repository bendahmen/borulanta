// A rate as Turnout. Shiny's lineup and roster tables show it through DT's
// formatPercentage(digits = 0), which rounds in JS (DTWidget.formatPercentage in DT's
// htmlwidgets/datatables.js), so this does the same, for the same answer: 12.5% shows 13%.
// The one display rounding in JS. Everything else shows R's own strings.
//
// R's round() is not the same on a tie: player_headline_ui() in R/cards.R gives 12% for a
// rate of 12.5%. No rate means an empty cell, as in DT.
export function formatTurnout(rate) {
  if (rate === null || rate === undefined || Number.isNaN(rate)) return '';
  return `${(rate * 100).toFixed(0)}%`;
}
