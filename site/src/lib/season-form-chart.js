// The Season form plot, per season_form_plot() in R/plots.R. A separate chunk, imported only
// by Trends, with only the Plot parts named here. Colours are CSS tokens, so a change of light
// or dark needs no redraw.
import { axisX, axisY, dot, gridY, line, plot, ruleY } from '@observablehq/plot';

const CHART_STYLE = {
  background: 'transparent',
  color: 'var(--ink-muted)',
  fontFamily: 'var(--font-body)',
  fontSize: '12px',
  overflow: 'visible',
};

export function seasonFormChart({ spec, lines, width, height, title }) {
  // Fewer labels and smaller dots where 51 matches share a phone's width.
  const narrow = width < 480;
  const yTicks = spec.domain ? 3 : 5;
  const radius = narrow ? 2.5 : 3.5;

  return plot({
    width,
    height,
    marginTop: 8,
    marginRight: 10,
    marginBottom: 38,
    marginLeft: 30,
    ariaLabel: title,
    ariaDescription: spec.yLabel,
    style: CHART_STYLE,
    x: { type: 'utc' },
    y: spec.domain ? { domain: spec.domain } : { nice: true },
    marks: [
      gridY({ ticks: yTicks, stroke: 'var(--border)', strokeOpacity: 1 }),
      spec.reference === null ? null : ruleY([spec.reference], { stroke: 'var(--ink-muted)', strokeWidth: 1 }),
      ...lines.flatMap(({ points, colour }) => [
        line(points, { x: 'date', y: 'value', stroke: colour, strokeWidth: 2 }),
        dot(points, { x: 'date', y: 'value', r: radius, fill: colour, stroke: 'var(--surface)', strokeWidth: 0.75 }),
      ]),
      axisX({ ticks: narrow ? 4 : 8, tickSize: 0, label: null }),
      axisY({ ticks: yTicks, tickSize: 0, label: null }),
    ],
  });
}
