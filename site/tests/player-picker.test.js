import assert from 'node:assert/strict';
import { test } from 'node:test';

import { feeRows } from '../src/lib/fee-rows.js';
import { mountPlayerPicker, readPlayer, writePlayer } from '../src/lib/player-picker.js';
import { fakeClassList, fakeElement, fakeSelect, fakeWindow } from './helpers/fake-dom.js';

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

// The card as FeeCard.astro writes it with `breakdown`: a slot per data attribute.
function mountCard(t, saved) {
  const { win } = fakeWindow(t);
  const stored = new Map(saved ? [['borulanta.player', saved]] : []);
  const storage = () => ({
    getItem: (key) => stored.get(key) ?? null,
    setItem: (key, value) => stored.set(key, value),
  });
  const slot = (fee) => fakeElement({ dataset: { fee } });
  const slots = {
    balance: slot('balance'),
    status: slot('status'),
    charges: slot('charges'),
    payments: slot('payments'),
  };
  const select = fakeSelect([
    { value: '', dataset: {} },
    { value: 'Anna', dataset: { balance: '£5.00', status: 'To pay', owed: '1', charges: '£15.00', payments: '£10.00' } },
    { value: 'Bert', dataset: { balance: '£0.00', status: 'Nothing to pay', owed: '0', charges: '£7.00', payments: '£7.00' } },
  ]);
  const root = {
    classList: fakeClassList(),
    querySelector: () => select,
    querySelectorAll: () => Object.values(slots),
  };
  mountPlayerPicker(root, storage);
  const shown = () => Object.fromEntries(Object.entries(slots).map(([name, element]) => [name, element.textContent]));
  return { root, select, shown, stored, win };
}

const ANNA_SHOWN = { balance: '£5.00', status: 'To pay', charges: '£15.00', payments: '£10.00' };
const BERT_SHOWN = { balance: '£0.00', status: 'Nothing to pay', charges: '£7.00', payments: '£7.00' };
const NOTHING_SHOWN = { balance: '', status: '', charges: '', payments: '' };

test('a saved name fills every slot from its own option, before anything else happens', (t) => {
  const card = mountCard(t, 'Anna');
  assert.equal(card.select.value, 'Anna');
  assert.deepEqual(card.shown(), ANNA_SHOWN);
  assert.ok(card.root.classList.contains('has-player'));
  assert.ok(card.root.classList.contains('is-owed'));
});

test('a new choice replaces every slot and is remembered', (t) => {
  const card = mountCard(t, 'Anna');
  card.select.choose('Bert');
  assert.deepEqual(card.shown(), BERT_SHOWN);
  assert.ok(!card.root.classList.contains('is-owed'));
  assert.equal(card.stored.get('borulanta.player'), 'Bert');
});

test('a first visit, or a name no longer on the roster, leaves the slots empty', (t) => {
  for (const saved of [null, 'Left The Club']) {
    const card = mountCard(t, saved);
    assert.equal(card.select.value, '');
    assert.deepEqual(card.shown(), NOTHING_SHOWN);
    assert.ok(!card.root.classList.contains('has-player'));
  }
});

test('back on the placeholder, nothing of the last player stays in a slot', (t) => {
  const card = mountCard(t, 'Anna');
  card.select.choose('');
  assert.deepEqual(card.shown(), NOTHING_SHOWN);
  assert.ok(!card.root.classList.contains('has-player'));
  assert.ok(!card.root.classList.contains('is-owed'));
});

test('a page restored from the back-forward cache picks up a name chosen elsewhere', (t) => {
  const card = mountCard(t, 'Anna');
  card.stored.set('borulanta.player', 'Bert');
  card.win.fire('pageshow', { persisted: false });
  assert.deepEqual(card.shown(), ANNA_SHOWN);
  card.win.fire('pageshow', { persisted: true });
  assert.equal(card.select.value, 'Bert');
  assert.deepEqual(card.shown(), BERT_SHOWN);
});
