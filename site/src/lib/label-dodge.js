// Places a name beside each dot without any two names touching. Works in pixels and
// has no randomness and no dependence on input order, so one input gives one answer.
// Far-flung labels get a leader line back to their dot.

// Preference order, worth a little more than the sub-pixel differences in reach between
// nearby angles. Right, then the diagonals, then the rest.
const ANGLES = [0, -45, 45, 180, -135, 135, -90, 90, -22.5, 22.5, -157.5, 157.5, -67.5, 67.5, -112.5, 112.5];
// How far past the dot's edge a label may sit, tried in this order.
const REACHES = [0, 3, 6, 10, 15, 21, 28, 36, 45, 56, 68, 82, 98, 116, 136, 160];
// More than the longest reach costs, so a label takes any free spot before it covers a dot.
const DOT_PENALTY = 400;
const OWN_DOT_PENALTY = 400;
const LEADER_CROSSING_PENALTY = 130;
const LEADER_OVER_LABEL_PENALTY = 160;
const LEADER_OVER_DOT_PENALTY = 30;
const LINE_PENALTY = 9;
const ANGLE_PREFERENCE = 0.35;
const OUTWARD_WEIGHT = 16;
const LONG_REACH = 80;
const NEIGHBOUR_DISTANCE = 75;
const MAX_SWEEPS = 8;
const GRID_STEP = 6;
const UNPLACED_COST = 1e6;

const clamp = (value, low, high) => Math.min(Math.max(value, low), high);

function boxAt(cx, cy, width, height, bounds) {
  const left = clamp(cx - width / 2, bounds.left, Math.max(bounds.left, bounds.right - width));
  const top = clamp(cy - height / 2, bounds.top, Math.max(bounds.top, bounds.bottom - height));
  return { left, top, right: left + width, bottom: top + height };
}

function boxesOverlap(a, b, pad) {
  return a.left < b.right + pad && b.left < a.right + pad && a.top < b.bottom + pad && b.top < a.bottom + pad;
}

function circleOverlapsBox(point, radius, box) {
  const dx = point.x - clamp(point.x, box.left, box.right);
  const dy = point.y - clamp(point.y, box.top, box.bottom);
  return dx * dx + dy * dy < radius * radius;
}

// How far a label sits from the edge of its dot, the same measure as a reach.
function boxReach(point, box) {
  const dx = point.x - clamp(point.x, box.left, box.right);
  const dy = point.y - clamp(point.y, box.top, box.bottom);
  return Math.max(0, Math.hypot(dx, dy) - point.r);
}

// From the edge of the dot to the nearest point of the label. Null when they touch.
function leaderFor(point, box, minLength) {
  const toX = clamp(point.x, box.left, box.right);
  const toY = clamp(point.y, box.top, box.bottom);
  const dx = toX - point.x;
  const dy = toY - point.y;
  const distance = Math.hypot(dx, dy);
  if (distance - point.r < minLength) return null;
  return {
    x1: point.x + (dx / distance) * point.r,
    y1: point.y + (dy / distance) * point.r,
    x2: toX,
    y2: toY,
  };
}

function segmentsCross(a, b) {
  const side = (ax, ay, bx, by, cx, cy) => (bx - ax) * (cy - ay) - (by - ay) * (cx - ax);
  const d1 = side(a.x1, a.y1, a.x2, a.y2, b.x1, b.y1);
  const d2 = side(a.x1, a.y1, a.x2, a.y2, b.x2, b.y2);
  const d3 = side(b.x1, b.y1, b.x2, b.y2, a.x1, a.y1);
  const d4 = side(b.x1, b.y1, b.x2, b.y2, a.x2, a.y2);
  return d1 * d2 < 0 && d3 * d4 < 0;
}

// Liang-Barsky clipping: does any part of the segment fall inside the box?
function segmentHitsBox(segment, box) {
  let low = 0;
  let high = 1;
  const dx = segment.x2 - segment.x1;
  const dy = segment.y2 - segment.y1;
  const edges = [
    [-dx, segment.x1 - box.left],
    [dx, box.right - segment.x1],
    [-dy, segment.y1 - box.top],
    [dy, box.bottom - segment.y1],
  ];
  for (const [p, q] of edges) {
    if (p === 0) {
      if (q < 0) return false;
    } else {
      const t = q / p;
      if (p < 0) low = Math.max(low, t);
      else high = Math.min(high, t);
      if (low > high) return false;
    }
  }
  return true;
}

function segmentHitsCircle(segment, point, radius) {
  const dx = segment.x2 - segment.x1;
  const dy = segment.y2 - segment.y1;
  const lengthSquared = dx * dx + dy * dy;
  const t = lengthSquared === 0 ? 0 : clamp(((point.x - segment.x1) * dx + (point.y - segment.y1) * dy) / lengthSquared, 0, 1);
  const nearX = segment.x1 + t * dx;
  const nearY = segment.y1 + t * dy;
  return Math.hypot(point.x - nearX, point.y - nearY) < radius;
}

// Reaching twice as far costs more than twice as much, so one label does not take a very
// long leader to spare its neighbours a slightly longer one. In a crowd, pointing away
// from the crowd also costs less, because leaders that fan out do not cross.
function baseCost(point, box, push) {
  const reach = boxReach(point, box);
  let cost = reach + (reach * reach) / LONG_REACH;
  if (push) {
    const dx = (box.left + box.right) / 2 - point.x;
    const dy = (box.top + box.bottom) / 2 - point.y;
    const length = Math.hypot(dx, dy) || 1;
    cost += ((1 - (dx * push.x + dy * push.y) / length) / 2) * OUTWARD_WEIGHT * push.strength;
  }
  return cost;
}

function evaluate(box, tieBreak, index, points, placed, options) {
  const point = points[index];
  for (let other = 0; other < placed.length; other += 1) {
    if (other !== index && placed[other] && boxesOverlap(box, placed[other].box, options.labelGap)) return Infinity;
  }
  const leader = leaderFor(point, box, options.minLeader);
  let cost = baseCost(point, box, options.pushes[index]) + tieBreak;
  for (const x of options.avoidX) if (box.left < x && x < box.right) cost += LINE_PENALTY;
  for (const y of options.avoidY) if (box.top < y && y < box.bottom) cost += LINE_PENALTY;
  points.forEach((other, otherIndex) => {
    const padded = other.r + options.dotGap;
    if (otherIndex === index) {
      if (circleOverlapsBox(other, other.r, box)) cost += OWN_DOT_PENALTY;
      return;
    }
    if (circleOverlapsBox(other, padded, box)) cost += DOT_PENALTY;
    if (leader && segmentHitsCircle(leader, other, other.r)) cost += LEADER_OVER_DOT_PENALTY;
    const neighbour = placed[otherIndex];
    if (!neighbour) return;
    if (leader && segmentHitsBox(leader, neighbour.box)) cost += LEADER_OVER_LABEL_PENALTY;
    if (neighbour.leader) {
      if (segmentHitsBox(neighbour.leader, box)) cost += LEADER_OVER_LABEL_PENALTY;
      if (leader && segmentsCross(leader, neighbour.leader)) cost += LEADER_CROSSING_PENALTY;
    }
  });
  return cost;
}

// The cheapest spot for one label, given where the others are. Reaches ascend and a
// reach is part of the cost, so the search can stop once the reach alone costs more.
// A box pushed back inside the bounds can sit nearer than its reach says, but then a
// shorter reach has already tried that spot.
function bestSpot(index, points, placed, bounds, options) {
  const point = points[index];
  let best = null;
  for (const reach of REACHES) {
    if (best && reach > best.cost) break;
    ANGLES.forEach((angle, order) => {
      const radians = (angle * Math.PI) / 180;
      const ux = Math.cos(radians);
      const uy = Math.sin(radians);
      const ring = point.r + options.dotGap + reach;
      // The box touches the ring: its near edge or corner sits on it, not its centre.
      const cx = point.x + ux * ring + (clamp(ux * 1.5, -1, 1) * point.width) / 2;
      const cy = point.y + uy * ring + (clamp(uy * 1.5, -1, 1) * point.height) / 2;
      const box = boxAt(cx, cy, point.width, point.height, bounds);
      const cost = evaluate(box, order * ANGLE_PREFERENCE, index, points, placed, options);
      if (cost < Infinity && (!best || cost < best.cost)) best = { box, cost };
    });
  }
  return best;
}

// Only for a crowd the rings could not seat: look everywhere inside the bounds.
function anywhereInBounds(index, points, placed, bounds, options) {
  const point = points[index];
  let best = null;
  for (let top = bounds.top; top + point.height <= bounds.bottom; top += GRID_STEP) {
    for (let left = bounds.left; left + point.width <= bounds.right; left += GRID_STEP) {
      const box = { left, top, right: left + point.width, bottom: top + point.height };
      const cost = evaluate(box, 0, index, points, placed, options);
      if (cost < Infinity && (!best || cost < best.cost)) best = { box, cost };
    }
  }
  return best;
}

// Which way is out of the crowd around each dot, and how crowded it is. Null if alone.
function pushesFor(points) {
  return points.map((point) => {
    let x = 0;
    let y = 0;
    let count = 0;
    for (const other of points) {
      const distance = Math.hypot(other.x - point.x, other.y - point.y);
      if (other === point || distance >= NEIGHBOUR_DISTANCE) continue;
      // Two dots in one spot give no direction, so they push apart along the id order.
      const weight = 1 - distance / NEIGHBOUR_DISTANCE;
      x += distance > 0 ? ((point.x - other.x) / distance) * weight : point.id < other.id ? -weight : weight;
      y += distance > 0 ? ((point.y - other.y) / distance) * weight : 0;
      count += 1;
    }
    const length = Math.hypot(x, y);
    return count === 0 || length === 0 ? null : { x: x / length, y: y / length, strength: Math.min(1, count / 3) };
  });
}

function solve(order, points, bounds, options) {
  const placed = new Array(points.length).fill(null);
  const seat = (index, spot) => {
    placed[index] = { box: spot.box, leader: leaderFor(points[index], spot.box, options.minLeader) };
  };

  for (const index of order) {
    const spot = bestSpot(index, points, placed, bounds, options) ?? anywhereInBounds(index, points, placed, bounds, options);
    if (spot) seat(index, spot);
  }

  // Earlier choices were made without knowing the later ones, so revisit each in turn.
  for (let sweep = 0; sweep < MAX_SWEEPS; sweep += 1) {
    let moved = false;
    for (const index of order) {
      const current = placed[index];
      if (!current) continue;
      placed[index] = null;
      const stay = evaluate(current.box, 0, index, points, placed, options);
      const spot = bestSpot(index, points, placed, bounds, options);
      if (spot && spot.cost < stay - 0.5) {
        seat(index, spot);
        moved = true;
      } else {
        placed[index] = current;
      }
    }
    if (!moved) break;
  }

  // Each index is scored with the rest in place, so anything shared counts for both.
  let total = 0;
  placed.forEach((current, index) => {
    total += current ? evaluate(current.box, 0, index, points, placed, options) : UNPLACED_COST;
  });
  return { placed, total };
}

/**
 * @param {{id: string, x: number, y: number, r: number, width: number, height: number}[]} input
 * @param {{left: number, top: number, right: number, bottom: number}} bounds
 * @param {{avoidX?: number[], avoidY?: number[]}} [options] lines labels should stay off if
 *   they easily can, such as the reference lines at 0
 * @returns one entry per input point, by id order: its label's box, its leader line or
 *   null, and `placed: false` only if no free space was left in the bounds at all.
 */
export function dodgeLabels(input, bounds, { labelGap = 1, dotGap = 1.5, minLeader = 3, avoidX = [], avoidY = [] } = {}) {
  const points = [...input].sort((a, b) => (a.id < b.id ? -1 : a.id > b.id ? 1 : 0));
  const options = { labelGap, dotGap, minLeader, avoidX, avoidY, pushes: pushesFor(points) };
  const indices = points.map((_, index) => index);

  // The result depends on who chooses first, and no one order wins everywhere, so try a
  // few and keep the best: most crowded first, then each side of the plot inwards.
  const crowding = points.map(
    (point) => points.filter((other) => other !== point && Math.hypot(other.x - point.x, other.y - point.y) < point.width * 1.2).length,
  );
  const orders = [
    [...indices].sort((a, b) => crowding[b] - crowding[a] || a - b),
    [...indices].sort((a, b) => points[a].x - points[b].x || a - b),
    [...indices].sort((a, b) => points[b].x - points[a].x || a - b),
    [...indices].sort((a, b) => points[a].y - points[b].y || a - b),
    [...indices].sort((a, b) => points[b].y - points[a].y || a - b),
  ];
  let best = null;
  for (const order of orders) {
    const attempt = solve(order, points, bounds, options);
    if (!best || attempt.total < best.total) best = attempt;
  }

  return points.map((point, index) => {
    const seated = best.placed[index];
    // Nowhere free. Still returned on the plot, so a name is never simply lost.
    const box = seated?.box ?? boxAt(point.x, point.y, point.width, point.height, bounds);
    return {
      id: point.id,
      ...box,
      cx: (box.left + box.right) / 2,
      cy: (box.top + box.bottom) / 2,
      leader: seated?.leader ?? null,
      placed: Boolean(seated),
    };
  });
}
