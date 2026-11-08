// The two tables on the home page, worked out at build time from R's exports.

// Our row and `reach` sides either way. Near the top or the bottom the window slides
// rather than shrinks, so the snippet is always the same height.
export function leagueWindow(rows, team, reach = 2) {
  const at = rows.findIndex((row) => row.team === team);
  if (at < 0) return [];
  const size = Math.min(rows.length, 2 * reach + 1);
  const start = Math.min(Math.max(at - reach, 0), rows.length - size);
  return rows.slice(start, start + size);
}

// R writes an OLS estimate as "2.14 [0.138]", the p-value in brackets. The home table puts
// the two on separate lines, so this splits R's string rather than formatting the numbers.
export function splitEstimate(display) {
  const match = /^(\S+) \[(.+)\]$/.exec(display ?? '');
  return match ? { estimate: match[1], p: match[2] } : { estimate: display ?? '', p: '' };
}

// One row per player from one season file: goals from the scorers table, turnout from the
// profiles, and the OLS coefficient on goal difference. A guest who only scored has no
// profile and so no turnout. A player under the regression's appearance floor has no
// coefficient. Goals first, then turnout, then the name, A to Z.
export function playerStatRows(subset) {
  const goals = new Map(subset.scorers.rows.map((row) => [row.player, row.goals]));
  const turnout = new Map(subset.profiles.map((row) => [row.player, row]));
  const ols = subset.regression.ols;
  const effects = new Map(ols.has_regression ? ols.players.map((row) => [row.player, row.outcomes.goal_difference]) : []);
  const names = new Set([...goals.keys(), ...turnout.keys()]);
  return [...names]
    .map((player) => {
      const effect = effects.get(player);
      return {
        player,
        goals: goals.get(player) ?? 0,
        appearances: turnout.get(player)?.appearances ?? null,
        attendance_rate: turnout.get(player)?.attendance_rate ?? null,
        gd_effect: effect?.estimate ?? null,
        gd_display: effect ? splitEstimate(effect.display) : null,
      };
    })
    .sort(
      (a, b) =>
        b.goals - a.goals ||
        (b.attendance_rate ?? -1) - (a.attendance_rate ?? -1) ||
        (a.player < b.player ? -1 : a.player > b.player ? 1 : 0),
    );
}
