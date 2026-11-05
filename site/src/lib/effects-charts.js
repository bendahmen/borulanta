// The two Effects plots. A separate chunk, imported only by the Effects page, and only
// the Plot parts named here reach the bundle. Colours are CSS tokens, so a change of
// light or dark needs no redraw.
import { axisX, axisY, dot, gridX, gridY, plot, ruleX, ruleY } from '@observablehq/plot';

import {
  ATTACK_DEFENCE_MARGIN,
  attackDefenceRows,
  axisTickFormat,
  COEFFICIENT_MARGIN,
  coefficientDomain,
  coefficientHeight,
  dotRadius,
  equalDomains,
  maxDotRadius,
} from './effects-data.js';
import { dodgeLabels } from './label-dodge.js';
import { EFFECTS } from './labels.js';

const SVG_NS = 'http://www.w3.org/2000/svg';
const FONT_FAMILY = '"Barlow", "Barlow Fallback", "Helvetica Neue", Arial, sans-serif';
const LABEL_FONT_SIZE = 12;
const LABEL_HEIGHT = 14;
const LABEL_FONT = `600 ${LABEL_FONT_SIZE}px ${FONT_FAMILY}`;
const NAME_FONT = `400 13px ${FONT_FAMILY}`;
const CHART_STYLE = {
  background: 'transparent',
  color: 'var(--ink-muted)',
  fontFamily: 'var(--font-body)',
  fontSize: '12px',
  overflow: 'visible',
};

let measuringContext = null;

function textWidth(text, font) {
  measuringContext ??= document.createElement('canvas').getContext('2d');
  measuringContext.font = font;
  return measuringContext.measureText(text).width;
}

function svgElement(name, attributes) {
  const element = document.createElementNS(SVG_NS, name);
  for (const [key, value] of Object.entries(attributes)) element.setAttribute(key, value);
  return element;
}

function labelsMark(rows, radius) {
  return (index, scales, values, dimensions) => {
    const points = rows.map((row) => ({
      id: row.player,
      x: scales.x(row.goals_scored),
      y: scales.y(row.defensive_effect),
      r: radius(row),
      // Room for the halo on both sides.
      width: textWidth(row.player, LABEL_FONT) + 4,
      height: LABEL_HEIGHT,
    }));
    const placed = dodgeLabels(
      points,
      {
        left: dimensions.marginLeft + 1,
        right: dimensions.width - dimensions.marginRight - 1,
        top: dimensions.marginTop + 1,
        bottom: dimensions.height - dimensions.marginBottom - 1,
      },
      { avoidX: [scales.x(0)], avoidY: [scales.y(0)] },
    );

    const group = svgElement('g', { 'aria-label': EFFECTS.playerNames });
    for (const label of placed) {
      if (!label.leader) continue;
      group.append(
        svgElement('line', {
          x1: label.leader.x1,
          y1: label.leader.y1,
          x2: label.leader.x2,
          y2: label.leader.y2,
          stroke: 'var(--ink-muted)',
          'stroke-width': 0.75,
        }),
      );
    }
    for (const label of placed) {
      const text = svgElement('text', {
        x: label.cx,
        y: label.cy,
        'text-anchor': 'middle',
        'dominant-baseline': 'central',
        fill: 'var(--ink)',
        stroke: 'var(--surface)',
        'stroke-width': 3,
        'stroke-linejoin': 'round',
        'paint-order': 'stroke',
        style: `font: ${LABEL_FONT}`,
      });
      text.textContent = label.id;
      group.append(text);
    }
    return group;
  };
}

export function attackDefenceChart({ players, width, labels, title, description }) {
  // Four ticks on a phone, or a run of six like -0.003 leaves them touching.
  const ticks = width < 400 ? 4 : 5;
  const rows = attackDefenceRows(players);
  const maxAppearances = Math.max(...rows.map((row) => row.appearances));
  const maxRadius = maxDotRadius(width);
  const radius = (row) => dotRadius(row.appearances, maxAppearances, maxRadius);
  const domains = equalDomains(
    rows.map((row) => row.goals_scored),
    rows.map((row) => row.defensive_effect),
  );
  // Biggest first, so a small dot is never hidden behind a large one.
  const drawOrder = [...rows].sort((a, b) => b.appearances - a.appearances || (a.player < b.player ? -1 : 1));

  return plot({
    width,
    height: width,
    marginTop: ATTACK_DEFENCE_MARGIN.top,
    marginRight: ATTACK_DEFENCE_MARGIN.right,
    marginBottom: ATTACK_DEFENCE_MARGIN.bottom,
    marginLeft: ATTACK_DEFENCE_MARGIN.left,
    ariaLabel: title,
    ariaDescription: description,
    style: CHART_STYLE,
    x: { domain: domains.x },
    y: { domain: domains.y },
    r: { type: 'identity' },
    marks: [
      gridX({ ticks, stroke: 'var(--border)', strokeOpacity: 1 }),
      gridY({ ticks, stroke: 'var(--border)', strokeOpacity: 1 }),
      ruleX([0], { stroke: 'var(--ink-muted)', strokeWidth: 1 }),
      ruleY([0], { stroke: 'var(--ink-muted)', strokeWidth: 1 }),
      dot(drawOrder, {
        x: 'goals_scored',
        y: 'defensive_effect',
        r: radius,
        fill: 'var(--blue)',
        fillOpacity: 0.8,
        stroke: 'var(--surface)',
        strokeWidth: 0.75,
      }),
      labelsMark(rows, radius),
      axisX({
        ticks,
        tickSize: 0,
        tickFormat: axisTickFormat(domains.x),
        label: labels.x,
        labelAnchor: 'center',
        labelArrow: 'none',
        labelOffset: 40,
      }),
      axisY({
        ticks,
        tickSize: 0,
        tickFormat: axisTickFormat(domains.y),
        label: labels.y,
        labelAnchor: 'center',
        labelArrow: 'none',
        labelOffset: 55,
      }),
    ],
  });
}

export function coefficientChart({ rows, width, title, description }) {
  const domain = coefficientDomain(rows);
  const longestName = Math.max(...rows.map((row) => textWidth(row.player, NAME_FONT)));
  const hasInterval = rows.some((row) => row.conf_low !== null && row.conf_low !== undefined);

  return plot({
    width,
    height: coefficientHeight(rows.length),
    marginTop: COEFFICIENT_MARGIN.top,
    marginRight: COEFFICIENT_MARGIN.right,
    marginBottom: COEFFICIENT_MARGIN.bottom,
    marginLeft: Math.ceil(longestName) + 10,
    ariaLabel: title,
    ariaDescription: description,
    style: CHART_STYLE,
    x: { domain },
    y: { type: 'point', domain: rows.map((row) => row.player), padding: 0.5 },
    marks: [
      gridX({ ticks: 5, stroke: 'var(--border)', strokeOpacity: 1 }),
      ruleX([0], { stroke: 'var(--ink-muted)', strokeWidth: 1 }),
      // Ridge has no standard error, so it has no bars, as coefficient_plot().
      hasInterval ? ruleY(rows, { y: 'player', x1: 'conf_low', x2: 'conf_high', stroke: 'var(--ink-muted)', strokeWidth: 1.25 }) : null,
      dot(rows, { x: 'estimate', y: 'player', r: 4.5, fill: 'var(--blue)', stroke: 'var(--surface)', strokeWidth: 0.75 }),
      axisX({ ticks: 5, tickSize: 0, tickFormat: axisTickFormat(domain), label: null }),
      axisY({ tickSize: 0, tickPadding: 8, label: null, fontSize: 13, color: 'var(--ink)' }),
    ],
  });
}
