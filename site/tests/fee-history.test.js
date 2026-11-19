import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { test } from 'node:test';
import vm from 'node:vm';

import { balanceColumns, balanceRowClass, balanceState, feeBreakdown, feeFileOverview } from '../src/lib/fee-rows.js';
import { chargeColumns, chargesHtml, feeFile, FIRST_CHARGES, moreChargesLabel, paymentColumns, paymentsHtml } from '../src/lib/fee-history.js';
import { moduleToInline, scriptSafeJson } from '../src/lib/inline.js';
import { EVERYONE_CARD, FEE_HISTORY_CARDS as cards, FEE_STATUS, FEES } from '../src/lib/labels.js';

const read = (path) => JSON.parse(readFileSync(new URL(path, import.meta.url)));
const fees = read('../src/generated/fees.json');
// What Fees > You fetches: each player's overview as src/pages/data/fees/[slug].json.js writes it.
const feeFiles = fees.overviews.map(feeFileOverview);
const count = (html, pattern) => (html.match(pattern) ?? []).length;

const words = FEES;

test('the balance picks the state, and only the sign of it is read', () => {
  assert.equal(balanceState(0.01), 'owed');
  assert.equal(balanceState(0), 'square');
  assert.equal(balanceState(-0.01), 'credit');
  assert.equal(balanceRowClass({ balance: 5 }), 'is-owed');
  assert.equal(balanceRowClass({ balance: 0 }), '');
  assert.equal(balanceRowClass({ balance: -5 }), 'is-credit');
});

test('Everyone says in words what the row says in colour, and only where it says something', () => {
  const [player] = balanceColumns(EVERYONE_CARD.headings, FEE_STATUS);
  assert.equal(player.cue({ balance: 9.5 }), FEE_STATUS.owed);
  assert.equal(player.cue({ balance: -2 }), FEE_STATUS.credit);
  assert.equal(player.cue({ balance: 0 }), '');
});

test('Everyone keeps all five columns, and a phone keeps Player and Balance', () => {
  const columns = balanceColumns(EVERYONE_CARD.headings, FEE_STATUS);
  assert.deepEqual(columns.map((column) => column.label), ['Player', 'Charges', 'Payments', 'Last payment', 'Balance']);
  assert.deepEqual(columns.filter((column) => !column.folds).map((column) => column.label), ['Player', 'Balance']);
  const [row] = fees.balances;
  const values = columns.map((column) => column.value(row));
  assert.deepEqual(values.toSpliced(3, 1), [row.player, row.charges_display, row.payments_display, row.balance_display]);
  assert.equal(columns[3].value({ last_payment: '2026-09-02' }), '02 Sep 2026');
  // Somebody who has never paid.
  assert.equal(columns[3].value({ last_payment: null }), '—');
});

test('a player\'s file writes out each date beside it and changes nothing else', () => {
  const overview = { player: 'A', match_charges: [{ date: '2026-09-09', charge: 1 }], payment_history: [{ date: '2026-10-01', amount: 2 }] };
  assert.deepEqual(feeFileOverview(overview), {
    player: 'A',
    match_charges: [{ date: '2026-09-09', date_short: '09 Sep 2026', charge: 1 }],
    payment_history: [{ date: '2026-10-01', date_short: '01 Oct 2026', amount: 2 }],
  });
  assert.deepEqual(overview.match_charges[0], { date: '2026-09-09', charge: 1 });
});

test('the charges keep all seven columns in their order, and a phone keeps three', () => {
  const columns = chargeColumns(cards.charges.headings, cards.charges.played);
  assert.deepEqual(columns.map((column) => column.label), [
    'Date', 'Season', 'Result', 'Played?', 'Squad size', 'Charge', 'Explanation',
  ]);
  assert.deepEqual(columns.filter((column) => !column.folds).map((column) => column.label), ['Date', 'Result', 'Charge']);
  const row = feeFiles[0].match_charges[0];
  const values = columns.map((column) => column.value(row));
  assert.deepEqual(values, [
    row.date_short, row.season, row.result, row.played ? 'Yes' : 'No', row.squad_size, row.charge_display, row.explanation,
  ]);
  assert.equal(columns[3].value({ played: true }), 'Yes');
  assert.equal(columns[3].value({ played: false }), 'No');
});

test('payments show Date and Payment, as R writes them', () => {
  const columns = paymentColumns(cards.payments.headings);
  assert.deepEqual(columns.map((column) => column.label), ['Date', 'Payment']);
  const row = { date_short: '02 Sep 2026', amount_display: '£23.19' };
  assert.deepEqual(columns.map((column) => column.value(row)), ['02 Sep 2026', '£23.19']);
});

test('the export keeps both tables newest first, and the page does not reorder them', () => {
  for (const overview of feeFiles) {
    const dates = overview.match_charges.map((row) => row.date);
    assert.deepEqual(dates, [...dates].sort().reverse(), overview.player);
    const paid = overview.payment_history.map((row) => row.date);
    assert.deepEqual(paid, [...paid].sort().reverse(), overview.player);
    const html = chargesHtml(overview, cards.charges, words);
    const shown = [...html.matchAll(/<span class="dt-whole"><span class="dt-name">([^<]*)</g)].map((match) => match[1]);
    assert.deepEqual(shown, overview.match_charges.map((row) => row.date_short), overview.player);
  }
});

test('the first twelve charges show and the rest wait for the button, as the pager had it', () => {
  const overview = feeFiles[0];
  const html = chargesHtml(overview, cards.charges, words);
  assert.equal(FIRST_CHARGES, 12);
  assert.equal(count(html, /data-later hidden/g), Math.max(overview.match_charges.length - 12, 0));
  assert.equal(moreChargesLabel(12, false, FEES), null);
  assert.equal(moreChargesLabel(13, false, FEES), 'Show all 13');
  assert.equal(moreChargesLabel(13, true, FEES), 'Show the latest 12');
});

test('nothing to show says so, in the empty-state words for payments', () => {
  const empty = { match_charges: [], payment_history: [] };
  assert.match(paymentsHtml(empty, cards.payments, words), new RegExp(FEES.noPayments));
  assert.match(chargesHtml(empty, cards.charges, words), new RegExp(FEES.noCharges));
  assert.doesNotMatch(paymentsHtml(empty, cards.payments, words), /<table/);
});

test('a real payments table holds every payment, to the string', () => {
  for (const overview of feeFiles) {
    const html = paymentsHtml(overview, cards.payments, words);
    if (overview.payment_history.length === 0) continue;
    assert.equal(count(html, /class="dt-row"/g), overview.payment_history.length);
    for (const row of overview.payment_history) assert.ok(html.includes(row.amount_display));
  }
});

test('the tiles are each overview\'s totals, each in its own place, and a gap fails the build', () => {
  for (const overview of fees.overviews) {
    assert.deepEqual(feeBreakdown(fees, overview.player), {
      charges: overview.total_charges_display,
      payments: overview.total_payments_display,
    }, overview.player);
  }
  // Somebody who is square has one string on both sides, so this one cannot be.
  const lopsided = { overviews: [{ player: 'A', total_charges_display: '£1.00', total_payments_display: '£2.00' }] };
  assert.deepEqual(feeBreakdown(lopsided, 'A'), { charges: '£1.00', payments: '£2.00' });
  assert.throws(() => feeBreakdown(fees, 'Nobody'), /no overview for Nobody/);
});

test('a player\'s file has an address made from the slug', () => {
  assert.equal(feeFile('ben'), '/data/fees/ben.json');
});

test('the three sources run together as a classic script', () => {
  const source = (name) => moduleToInline(readFileSync(new URL(`../src/lib/${name}.js`, import.meta.url), 'utf8'));
  const inline = `${source('data-table')}\n${source('subset-loader')}\n${source('fee-history')}`;
  assert.doesNotMatch(inline, /^(export|import) /m);
  assert.doesNotMatch(inline, /<\/script/i);
  const context = {};
  vm.runInNewContext(`${inline}\nthis.result = paymentsHtml(${scriptSafeJson({ payment_history: [] })}, {}, ${scriptSafeJson(words)});`, context);
  assert.match(context.result, new RegExp(FEES.noPayments));
});
