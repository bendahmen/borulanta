import assert from 'node:assert/strict';
import { test } from 'node:test';

import { feeRows } from '../src/lib/fee-rows.js';
import { readPlayer, writePlayer } from '../src/lib/player-picker.js';

test('blocked storage reads as nothing and never throws', () => {
  const blocked = () => {
    throw new Error('SecurityError');
  };
  assert.equal(readPlayer(blocked), null);
  assert.doesNotThrow(() => writePlayer(blocked, 'Ben'));

  const throwing = () => ({
    getItem() {
      throw new Error('denied');
    },
    setItem() {
      throw new Error('quota');
    },
  });
  assert.equal(readPlayer(throwing), null);
  assert.doesNotThrow(() => writePlayer(throwing, 'Ben'));
});

test('the choice is kept under borulanta.player', () => {
  const stored = new Map();
  const storage = () => ({
    getItem: (key) => stored.get(key) ?? null,
    setItem: (key, value) => stored.set(key, value),
  });
  writePlayer(storage, 'Ben');
  assert.equal(stored.get('borulanta.player'), 'Ben');
  assert.equal(readPlayer(storage), 'Ben');
});

const words = { owed: 'owed', square: 'square', credit: 'credit' };

test('feeRows keeps the R string and picks the status from the balance', () => {
  const fees = {
    fee_players: ['A', 'B', 'C'],
    balances: [
      { player: 'A', balance: 9.5, balance_display: '£9.50' },
      { player: 'B', balance: 0, balance_display: '£0.00' },
      { player: 'C', balance: -2, balance_display: '-£2.00' },
      { player: 'Left', balance: 5, balance_display: '£5.00' },
    ],
    overviews: [],
  };
  assert.deepEqual(feeRows(fees, words), [
    { player: 'A', display: '£9.50', owed: true, status: 'owed' },
    { player: 'B', display: '£0.00', owed: false, status: 'square' },
    { player: 'C', display: '-£2.00', owed: false, status: 'credit' },
  ]);
});

test('a signing with no match yet falls back to overviews, and a gap fails the build', () => {
  const fees = {
    fee_players: ['New'],
    balances: [],
    overviews: [{ player: 'New', balance: 0, balance_display: '£0.00' }],
  };
  assert.deepEqual(feeRows(fees, words), [
    { player: 'New', display: '£0.00', owed: false, status: 'square' },
  ]);
  assert.throws(() => feeRows({ ...fees, overviews: [] }, words), /no balance for New/);
});
