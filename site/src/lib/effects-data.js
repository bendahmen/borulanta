// The pure sums behind the Effects plots. Nothing here estimates anything or formats a
// statistic: the numbers are R's, and these only decide where they go on the page.

// Today's count across all seasons. Reserves height before the data arrives, so getting
// it wrong in a later season only means a small shift.
export const TYPICAL_PLAYER_COUNT = 17;

// Square, so one data unit is one length on both axes. The margins add up the same way
// across and down, so the whole plot is square too and the page can reserve its height.
export const ATTACK_DEFENCE_MARGIN = { top: 12, right: 6, bottom: 52, left: 58 };

export const COEFFICIENT_ROW = 26;
export const COEFFICIENT_MARGIN = { top: 6, right: 14, bottom: 26 };

export function coefficientHeight(playerCount) {
  return playerCount * COEFFICIENT_ROW + COEFFICIENT_MARGIN.top + COEFFICIENT_MARGIN.bottom;
}

// The one mapping allowed in JS: the sign flip for the y axis, as attack_defence_plot().
export function attackDefenceRows(players) {
  return players.map((player) => ({
    player: player.player,
    appearances: player.appearances,
    goals_scored: player.outcomes.goals_scored.estimate,
    defensive_effect: -player.outcomes.goals_conceded.estimate,
  }));
}

// Largest estimate first, which is top of the plot, as arrange() and coord_flip() do.
export function coefficientRows(players, outcome) {
  return players
    .map((player) => ({ player: player.player, ...player.outcomes[outcome] }))
    .sort((a, b) => b.estimate - a.estimate || (a.player < b.player ? -1 : 1));
}

function extentWithZero(values) {
  // The reference lines at 0 are part of the picture, so 0 is always in range.
  return values.reduce(([low, high], value) => [Math.min(low, value), Math.max(high, value)], [0, 0]);
}

// One data unit is the same length on both axes, as coord_equal() does. On a square plot
// that means one span for both, taken from the wider axis. The narrower axis stays
// centred on its own data. Ridge can leave goals scored around 1e-37 and goals conceded
// around 1: the dots then stand in a line, which is the true picture at equal scale.
export function equalDomains(xs, ys, padding = 0.14) {
  const [xLow, xHigh] = extentWithZero(xs);
  const [yLow, yHigh] = extentWithZero(ys);
  const widest = Math.max(xHigh - xLow, yHigh - yLow);
  // Everything exactly zero has no scale of its own, so draw a unit square around it.
  const half = (widest > 0 ? widest * (1 + 2 * padding) : 1) / 2;
  const xMiddle = (xLow + xHigh) / 2;
  const yMiddle = (yLow + yHigh) / 2;
  return { x: [xMiddle - half, xMiddle + half], y: [yMiddle - half, yMiddle + half] };
}

export function coefficientDomain(rows, padding = 0.06) {
  const values = rows.flatMap((row) => [row.estimate, row.conf_low, row.conf_high]).filter((value) => value !== null && value !== undefined);
  const [low, high] = extentWithZero(values);
  const pad = high > low ? (high - low) * padding : 0.5;
  return [low - pad, high + pad];
}

// Axis ticks only. Plot's default writes a tick at 1e-37 as a 39-digit decimal, which
// runs off the phone. This never touches an estimate: those show R's string.
export function axisTickFormat(domain) {
  const largest = Math.max(Math.abs(domain[0]), Math.abs(domain[1]));
  if (largest >= 1e-3) return undefined;
  return (tick) => (tick === 0 ? '0' : Number(tick.toPrecision(3)).toExponential());
}

// Area is proportional to appearances and no appearances is no area, as scale_size_area().
export function dotRadius(appearances, maxAppearances, maxRadius) {
  return maxAppearances > 0 ? Math.sqrt(appearances / maxAppearances) * maxRadius : 0;
}

export function maxDotRadius(width) {
  return Math.min(Math.max(width * 0.033, 8.5), 15);
}

// Round numbers for the size key: the finest step that needs no more than four circles.
export function sizeKeyValues(maxAppearances) {
  const step = [1, 2, 5, 10, 20, 50, 100, 200, 500].find((candidate) => Math.floor(maxAppearances / candidate) <= 4) ?? 1000;
  const values = [];
  for (let value = step; value <= maxAppearances; value += step) values.push(value);
  return values;
}
