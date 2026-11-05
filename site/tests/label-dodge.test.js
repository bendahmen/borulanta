import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { test } from 'node:test';

import { ATTACK_DEFENCE_MARGIN, attackDefenceRows, dotRadius, equalDomains, maxDotRadius } from '../src/lib/effects-data.js';
import { dodgeLabels } from '../src/lib/label-dodge.js';

const bounds = { left: 58, top: 12, right: 303, bottom: 257 };

const point = (id, x, y, r = 6, width = 46, height = 14) => ({ id, x, y, r, width, height });

function overlappingPairs(labels) {
  const pairs = [];
  labels.forEach((a, i) => {
    labels.slice(i + 1).forEach((b) => {
      if (a.left < b.right && b.left < a.right && a.top < b.bottom && b.top < a.bottom) pairs.push(`${a.id}/${b.id}`);
    });
  });
  return pairs;
}

// Each label against every dot, its own included, with how far it reaches into the dot.
function coveredDots(labels, points) {
  const covered = [];
  for (const label of labels) {
    for (const dot of points) {
      const dx = dot.x - Math.min(Math.max(dot.x, label.left), label.right);
      const dy = dot.y - Math.min(Math.max(dot.y, label.top), label.bottom);
      const depth = dot.r - Math.hypot(dx, dy);
      if (depth > 0) covered.push(`${label.id} on ${dot.id} by ${depth.toFixed(1)} px`);
    }
  }
  return covered;
}

function crossingLeaders(labels) {
  const side = (line, x, y) => (line.x2 - line.x1) * (y - line.y1) - (line.y2 - line.y1) * (x - line.x1);
  const crosses = (a, b) =>
    side(a, b.x1, b.y1) * side(a, b.x2, b.y2) < 0 && side(b, a.x1, a.y1) * side(b, a.x2, a.y2) < 0;
  const pairs = [];
  labels.forEach((a, i) => {
    labels.slice(i + 1).forEach((b) => {
      if (a.leader && b.leader && crosses(a.leader, b.leader)) pairs.push(`${a.id}/${b.id}`);
    });
  });
  return pairs;
}

function outsideBounds(labels, box) {
  return labels.filter((l) => l.left < box.left || l.right > box.right || l.top < box.top || l.bottom > box.bottom).map((l) => l.id);
}

// Seventeen players packed into the middle of a phone-sized plot.
const crowd = Array.from({ length: 17 }, (_, i) =>
  point(`Player${String(i).padStart(2, '0')}`, 170 + ((i * 37) % 60) - 30, 130 + ((i * 53) % 50) - 25, 3 + (i % 8)),
);

test('no two labels overlap on a crowded fixture', () => {
  const labels = dodgeLabels(crowd, bounds);
  assert.equal(labels.length, crowd.length);
  assert.ok(labels.every((label) => label.placed));
  assert.deepEqual(overlappingPairs(labels), []);
  assert.deepEqual(outsideBounds(labels, bounds), []);
});

test('the same input gives the same answer', () => {
  assert.deepEqual(dodgeLabels(crowd, bounds), dodgeLabels(crowd, bounds));
});

test('the answer does not depend on the order of the input', () => {
  const shuffled = [...crowd].sort((a, b) => (a.y * 7 + a.x) % 13 - ((b.y * 7 + b.x) % 13));
  assert.notDeepEqual(shuffled.map((p) => p.id), crowd.map((p) => p.id));
  assert.deepEqual(dodgeLabels(shuffled, bounds), dodgeLabels(crowd, bounds));
});

test('dots in one spot still get separate labels', () => {
  // What ridge could do: every estimate indistinguishable at this scale.
  const stacked = Array.from({ length: 17 }, (_, i) => point(`P${i}`, 180, 130));
  const labels = dodgeLabels(stacked, bounds);
  assert.deepEqual(overlappingPairs(labels), []);
  assert.deepEqual(outsideBounds(labels, bounds), []);
  assert.equal(new Set(labels.map((label) => `${label.left},${label.top}`)).size, 17);
});

test('dots in a vertical line spread their labels out', () => {
  // Season 2 under ridge: goals scored around 1e-37, so every dot shares an x.
  const line = Array.from({ length: 13 }, (_, i) => point(`P${i}`, 180, 60 + i * 12, 8));
  const labels = dodgeLabels(line, bounds);
  assert.deepEqual(overlappingPairs(labels), []);
});

test('dots at the corners keep their labels inside the bounds', () => {
  const corners = [
    point('TopLeft', bounds.left, bounds.top),
    point('TopRight', bounds.right, bounds.top),
    point('BottomLeft', bounds.left, bounds.bottom),
    point('BottomRight', bounds.right, bounds.bottom),
  ];
  const labels = dodgeLabels(corners, bounds);
  assert.deepEqual(outsideBounds(labels, bounds), []);
  assert.deepEqual(overlappingPairs(labels), []);
});

test('a label does not sit on its own dot at the edge of the plot', () => {
  // Pushed back inside the bounds, the usual spot to the right would land on the dot.
  const atEdges = [
    point('Right', bounds.right - 2, 120, 8),
    point('Left', bounds.left + 2, 60, 8),
    point('Top', 180, bounds.top + 2, 8),
    point('Bottom', 180, bounds.bottom - 2, 8),
  ];
  const labels = dodgeLabels(atEdges, bounds);
  assert.deepEqual(coveredDots(labels, atEdges), []);
  assert.deepEqual(outsideBounds(labels, bounds), []);
});

test('a label goes round a dot in its way', () => {
  // A ring of six dots leaves the middle one's nearby spots mostly covered.
  const ring = [
    point('Middle', 180, 130, 6, 60),
    ...[0, 60, 120, 180, 240, 300].map((degrees) =>
      point(`D${degrees}`, 180 + Math.cos((degrees * Math.PI) / 180) * 30, 130 + Math.sin((degrees * Math.PI) / 180) * 30, 10, 30),
    ),
  ];
  const labels = dodgeLabels(ring, bounds);
  assert.deepEqual(coveredDots(labels, ring), []);
  assert.deepEqual(overlappingPairs(labels), []);
});

test('a lone dot gets its label beside it, with no leader', () => {
  const [label] = dodgeLabels([point('Solo', 180, 130)], bounds);
  assert.equal(label.leader, null);
  assert.ok(label.left >= 180 && label.left - 180 < 12, 'to the right of the dot');
  assert.equal(label.cy, 130);
});

test('a label stays off a line it was asked to avoid, if that is cheap', () => {
  const free = dodgeLabels([point('Solo', 180, 130)], bounds);
  assert.ok(free[0].left < 200 && free[0].right > 200, 'the line runs through the usual spot');
  const [label] = dodgeLabels([point('Solo', 180, 130)], bounds, { avoidX: [200] });
  assert.ok(!(label.left < 200 && label.right > 200));
});

test('a label pushed away gets a leader that starts at the edge of its dot', () => {
  const labels = dodgeLabels(crowd, bounds);
  const withLeader = labels.filter((label) => label.leader);
  assert.ok(withLeader.length > 0);
  for (const label of withLeader) {
    const dot = crowd.find((p) => p.id === label.id);
    const fromCentre = Math.hypot(label.leader.x1 - dot.x, label.leader.y1 - dot.y);
    assert.ok(Math.abs(fromCentre - dot.r) < 1e-6, `${label.id} leader starts at the dot's edge`);
  }
});

test('reports a label it could not seat rather than losing it', () => {
  const tiny = { left: 0, top: 0, right: 60, bottom: 20 };
  const labels = dodgeLabels(
    [point('A', 20, 10), point('B', 30, 10), point('C', 40, 10), point('D', 50, 10)],
    tiny,
  );
  assert.equal(labels.length, 4);
  assert.ok(labels.some((label) => !label.placed));
  assert.ok(labels.every((label) => Number.isFinite(label.cx) && Number.isFinite(label.cy)));
});

// As the page measures them: canvas, 600 12px Barlow.
const NAME_WIDTHS = {
  Anton: 31.87,
  Ben: 20.48,
  Berghi: 34.42,
  Boris: 27.62,
  Dadde: 33.54,
  Daniel: 32.58,
  Elia: 19.3,
  Emiliano: 45.46,
  Jasper: 36.86,
  Luca: 25.98,
  Mateus: 38.02,
  Max: 21.04,
  Oleg: 23.4,
  Pietro: 32.12,
  Seb: 20.29,
  Torge: 30.04,
  Vitto: 25.91,
};

test('holds on the real data, for every subset and estimator from small phones to desktop', () => {
  const subsets = ['s1', 's2', 's3', 's1-s2', 's1-s3', 's2-s3', 's1-s2-s3'];
  const margin = ATTACK_DEFENCE_MARGIN;
  let checked = 0;
  for (const name of subsets) {
    const file = new URL(`../public/data/seasons/${name}.json`, import.meta.url);
    const data = JSON.parse(readFileSync(file, 'utf8'));
    for (const estimator of ['ols', 'ridge']) {
      const players = data.regression[estimator].players;
      if (players.length === 0) continue;
      // Plot widths for screens of 320, 360, 375, 390, 412 and 430 px, and two columns on desktop.
      for (const width of [254, 294, 309, 324, 346, 364, 462]) {
        const rows = attackDefenceRows(players);
        const most = Math.max(...rows.map((row) => row.appearances));
        const domains = equalDomains(
          rows.map((row) => row.goals_scored),
          rows.map((row) => row.defensive_effect),
        );
        const inner = width - margin.left - margin.right;
        const x = (value) => margin.left + ((value - domains.x[0]) / (domains.x[1] - domains.x[0])) * inner;
        const y = (value) => margin.top + inner - ((value - domains.y[0]) / (domains.y[1] - domains.y[0])) * inner;
        const points = rows.map((row) => ({
          id: row.player,
          x: x(row.goals_scored),
          y: y(row.defensive_effect),
          r: dotRadius(row.appearances, most, maxDotRadius(width)),
          // Room for the halo on both sides, as the plot leaves.
          width: NAME_WIDTHS[row.player] + 4,
          height: 14,
        }));
        const box = { left: margin.left + 1, right: width - margin.right - 1, top: margin.top + 1, bottom: width - margin.bottom - 1 };
        const labels = dodgeLabels(points, box, { avoidX: [x(0)], avoidY: [y(0)] });
        const where = `${name} ${estimator} at ${width}`;
        assert.deepEqual(overlappingPairs(labels), [], where);
        assert.deepEqual(outsideBounds(labels, box), [], where);
        assert.deepEqual(coveredDots(labels, points), [], where);
        assert.deepEqual(crossingLeaders(labels), [], where);
        checked += 1;
      }
    }
  }
  assert.equal(checked, 13 * 7);
});
