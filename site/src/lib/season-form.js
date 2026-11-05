// Which columns each Season form metric draws, as season_form_plot() in R/plots.R. Pure, so
// that it can be tested, and away from Plot, so that it can be read before Plot arrives.

const METRICS = {
  points: { domain: [0, 3], series: [{ column: 'rolling_points', colour: 'var(--blue)' }] },
  goals: {
    series: [
      { column: 'rolling_goals_scored', colour: 'var(--blue)' },
      { column: 'rolling_goals_conceded', colour: 'var(--amber)' },
    ],
  },
  goal_difference: { reference: 0, series: [{ column: 'rolling_goal_difference', colour: 'var(--green)' }] },
  squad_size: { series: [{ column: 'rolling_squad_size', colour: 'var(--green)' }] },
};

// `labels` is wording.plot_labels.season_form. The key is only for more than one line.
export function formMetric(metric, labels) {
  const known = METRICS[metric];
  if (!known) throw new Error(`Season form has no metric called ${metric}.`);
  return {
    yLabel: labels[metric].y,
    domain: known.domain ?? null,
    reference: known.reference ?? null,
    series: known.series.map((line) => ({ ...line, label: known.series.length > 1 ? labels.series[line.column] : null })),
  };
}

// One point per line per match, as the numbers came. Dates are the match's own day, in UTC
// so that no time zone can move a dot across midnight.
export function formPoints(seasonForm, spec) {
  return spec.series.map((line) => ({
    ...line,
    points: seasonForm.map((row) => ({ date: new Date(`${row.date}T00:00:00Z`), value: row[line.column] })),
  }));
}
