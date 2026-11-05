import assert from 'node:assert/strict';
import { test } from 'node:test';

import { DEFAULT_SORT, nextSort, sortOnField, rowComparator, sortOnShownColumn, sortRows } from '../src/lib/table-sort.js';

const row = (player, appearances, points) => ({
  player,
  appearances,
  outcomes: { points: { estimate: points, display: 'not read' } },
});

const names = (rows) => rows.map((item) => item.player).join(' ');

test('the default order is appearances descending, then player', () => {
  const rows = [row('Mateus', 35, 0), row('Boris', 41, 0), row('Ben', 35, 0), row('Oleg', 3, 0)];
  assert.equal(names(sortRows(rows, DEFAULT_SORT)), 'Boris Ben Mateus Oleg');
});

test('sorts on the number, not on how it would read as text', () => {
  const rows = [row('A', 1, 9), row('B', 1, 10), row('C', 1, -11), row('D', 1, 2)];
  assert.equal(names(sortRows(rows, { key: 'points', direction: 'ascending' })), 'C D A B');
  assert.equal(names(sortRows(rows, { key: 'points', direction: 'descending' })), 'B A D C');
});

test('tells apart values around 1e-53, and negatives from positives', () => {
  const rows = [
    row('A', 1, 5.93e-53),
    row('B', 1, -6.73e-37),
    row('C', 1, 3.37e-37),
    row('D', 1, 0),
    row('E', 1, -5.93e-53),
    row('F', 1, 1e-53),
  ];
  assert.equal(names(sortRows(rows, { key: 'points', direction: 'ascending' })), 'B E D F A C');
  assert.equal(names(sortRows(rows, { key: 'points', direction: 'descending' })), 'C A F D E B');
});

test('missing values go last whichever way the column runs', () => {
  const rows = [row('A', 1, null), row('B', 1, 2), row('C', 1, undefined), row('D', 1, -2), row('E', 1, Number.NaN)];
  assert.equal(names(sortRows(rows, { key: 'points', direction: 'ascending' })), 'D B A C E');
  assert.equal(names(sortRows(rows, { key: 'points', direction: 'descending' })), 'B D A C E');
});

test('an outcome the row lacks counts as missing', () => {
  const rows = [{ player: 'A', appearances: 1, outcomes: {} }, row('B', 1, 1)];
  assert.equal(names(sortRows(rows, { key: 'points', direction: 'descending' })), 'B A');
});

test('ties break on the player, A to Z, in both directions', () => {
  const rows = [row('Pietro', 22, 1), row('Emiliano', 22, 1), row('Torge', 22, 1)];
  assert.equal(names(sortRows(rows, { key: 'points', direction: 'ascending' })), 'Emiliano Pietro Torge');
  assert.equal(names(sortRows(rows, { key: 'points', direction: 'descending' })), 'Emiliano Pietro Torge');
  assert.equal(names(sortRows(rows, { key: 'appearances', direction: 'descending' })), 'Emiliano Pietro Torge');
});

test('sorts by name in the C locale, as arrange() does', () => {
  const rows = [row('anna', 1, 0), row('Boris', 1, 0), row('Ben', 1, 0)];
  assert.equal(names(sortRows(rows, { key: 'player', direction: 'ascending' })), 'Ben Boris anna');
  assert.equal(names(sortRows(rows, { key: 'player', direction: 'descending' })), 'anna Boris Ben');
});

test('sorting leaves the input alone', () => {
  const rows = [row('B', 1, 0), row('A', 2, 0)];
  sortRows(rows, DEFAULT_SORT);
  assert.equal(names(rows), 'B A');
});

test('the comparator never returns zero for two different players', () => {
  const compare = rowComparator({ key: 'points', direction: 'ascending' });
  assert.notEqual(compare(row('A', 1, 1), row('B', 1, 1)), 0);
});

test('a press sorts names from A and numbers from the largest, and a second reverses', () => {
  const first = nextSort(DEFAULT_SORT, 'points');
  assert.deepEqual(first, { key: 'points', direction: 'descending' });
  assert.deepEqual(nextSort(first, 'points'), { key: 'points', direction: 'ascending' });
  assert.deepEqual(nextSort(first, 'player'), { key: 'player', direction: 'ascending' });
  assert.deepEqual(nextSort(DEFAULT_SORT, 'appearances'), { key: 'appearances', direction: 'ascending' });
});

test('a sort on a column no longer showing moves to the selected outcome, keeping its direction', () => {
  const onHidden = { key: 'goals_conceded', direction: 'ascending' };
  const onlySelected = (key) => key === 'player' || key === 'appearances' || key === 'points';
  assert.deepEqual(sortOnShownColumn(onHidden, onlySelected, 'points'), { key: 'points', direction: 'ascending' });
});

test('a sort on a column still showing stays exactly as it was', () => {
  const onlySelected = (key) => key === 'player' || key === 'appearances' || key === 'points';
  assert.equal(sortOnShownColumn(DEFAULT_SORT, onlySelected, 'points'), DEFAULT_SORT);
  const onName = { key: 'player', direction: 'descending' };
  assert.equal(sortOnShownColumn(onName, onlySelected, 'points'), onName);
  const onOther = { key: 'goal_difference', direction: 'descending' };
  assert.equal(sortOnShownColumn(onOther, () => true, 'points'), onOther);
});

const scorers = [
  { player: 'Mateus', goals: 4, mom: 0, rate: 1 },
  { player: 'Emiliano', goals: 3, mom: 1, rate: 0.75 },
  { player: 'Daniel', goals: 3, mom: 1, rate: 0.6 },
  { player: 'Akash', goals: 2, mom: 0, rate: null },
  { player: 'Torge', goals: 2, mom: 0, rate: 0.4 },
];
const order = (rows) => rows.map((item) => item.player);

test('a field sort leaves ties in the order R sent, whichever way it runs', () => {
  assert.deepEqual(order(sortOnField(scorers, { key: 'goals', direction: 'descending' })), ['Mateus', 'Emiliano', 'Daniel', 'Akash', 'Torge']);
  assert.deepEqual(order(sortOnField(scorers, { key: 'goals', direction: 'ascending' })), ['Akash', 'Torge', 'Emiliano', 'Daniel', 'Mateus']);
});

test('a field sort puts a missing value last, whichever way it runs', () => {
  assert.equal(sortOnField(scorers, { key: 'rate', direction: 'descending' }).at(-1).player, 'Akash');
  assert.equal(sortOnField(scorers, { key: 'rate', direction: 'ascending' }).at(-1).player, 'Akash');
});

test('a field sort orders names as R does, and leaves the rows it was given alone', () => {
  const before = order(scorers);
  assert.deepEqual(order(sortOnField(scorers, { key: 'player', direction: 'ascending' })), ['Akash', 'Daniel', 'Emiliano', 'Mateus', 'Torge']);
  assert.deepEqual(order(scorers), before);
});

test('the first press on a column of figures starts with the largest', () => {
  assert.deepEqual(nextSort({ key: 'goals', direction: 'descending' }, 'mom'), { key: 'mom', direction: 'descending' });
  assert.deepEqual(nextSort({ key: 'goals', direction: 'descending' }, 'goals'), { key: 'goals', direction: 'ascending' });
});
