import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { test } from 'node:test';

import { FIRST_CHARGES, mountFeeHistory } from '../src/lib/fee-history.js';
import { FEES } from '../src/lib/labels.js';
import { fakeElement, fakeSelect, fakeWindow, settle } from './helpers/fake-dom.js';

const { fee_cards: cards } = JSON.parse(readFileSync(new URL('../src/generated/wording.json', import.meta.url)));
const data = {
  payments: { title: cards.payments.title, headings: cards.payments.headings },
  charges: { title: cards.charges.title, headings: cards.charges.headings, played: cards.charges.played },
  words: { ...FEES, noPayments: 'No payments recorded yet.' },
};

// `paid` shows up in the payments table and `each` in the charges table, so a test can say
// whose tables are on the page by looking for them.
function overview(paid, each, charges = 3) {
  return {
    payment_history: [{ date_short: '01 Jan 2026', amount_display: paid }],
    match_charges: Array.from({ length: charges }, (_, index) => ({
      date_short: `${index + 1} Oct 2026`,
      season: 'Season 3',
      result: '2-3',
      played: true,
      squad_size: 9,
      charge_display: each,
      explanation: 'Played.',
    })),
  };
}

const anna = overview('£1.11', '£1.00');
const bert = overview('£2.22', '£2.00');

// A fetch that answers only when told to, so a test can choose the order of replies.
function deferredFetch() {
  const calls = [];
  const fetchFile = (file) =>
    new Promise((resolve, reject) => {
      calls.push({ file, resolve, reject });
    });
  const pending = (file) => calls.findLast((call) => call.file === file);
  return {
    calls,
    fetchFile,
    reply: async (file, body) => {
      pending(file).resolve({ ok: true, status: 200, json: async () => body });
      await settle();
    },
    fail: async (file) => {
      pending(file).reject(new Error('offline'));
      await settle();
    },
  };
}

const ANNA = '/data/fees/anna.json';
const BERT = '/data/fees/bert.json';

// The page, with Anna and Bert on the picker and nobody chosen unless `chosen` says so.
function mount(t, chosen = null) {
  const { win, doc, focusable } = fakeWindow(t);
  const later = { html: null, rows: [] };
  const parts = {
    payments: fakeElement(),
    charges: fakeElement({
      // A row for each one the markup holds back, and the same ones until the markup changes.
      querySelectorAll() {
        if (this.innerHTML !== later.html) {
          later.html = this.innerHTML;
          later.rows = (this.innerHTML.match(/data-later/g) ?? []).map(() => ({ hidden: true }));
        }
        return later.rows;
      },
    }),
    more: fakeElement({ hidden: true }),
    failed: fakeElement({ hidden: true }),
    retry: focusable(),
  };
  const shownCards = [focusable({ hidden: true }), focusable({ hidden: true })];
  const root = {
    querySelector: (selector) => parts[selector.match(/data-history="(\w+)"/)[1]],
    querySelectorAll: () => shownCards,
  };
  const select = fakeSelect([
    { value: '', dataset: {} },
    { value: 'Anna', dataset: { slug: 'anna' } },
    { value: 'Bert', dataset: { slug: 'bert' } },
  ]);
  if (chosen) select.value = chosen;
  const network = deferredFetch();
  mountFeeHistory(root, select, data, network.fetchFile);
  return { ...parts, ...network, cards: shownCards, select, win, doc, laterRows: () => later.rows };
}

const showsPlayer = (page, paid, each) => {
  assert.ok(page.payments.innerHTML.includes(paid), `payments should show ${paid}`);
  assert.ok(page.charges.innerHTML.includes(each), `charges should show ${each}`);
};

const isBlank = (page) => {
  assert.equal(page.payments.innerHTML, '');
  assert.equal(page.charges.innerHTML, '');
};

test('with nobody chosen it fetches nothing and shows no card', async (t) => {
  const page = mount(t);
  await settle();
  assert.equal(page.calls.length, 0);
  assert.ok(page.cards.every((card) => card.hidden));
  assert.equal(page.failed.hidden, true);
});

test('a returning player gets their own file and both tables', async (t) => {
  const page = mount(t, 'Anna');
  assert.deepEqual(page.calls.map((call) => call.file), [ANNA]);
  assert.ok(page.cards.every((card) => !card.hidden));
  await page.reply(ANNA, anna);
  showsPlayer(page, '£1.11', '£1.00');
  assert.equal(page.failed.hidden, true);
});

test('a new choice takes the last player off the page at once, before any reply', async (t) => {
  const page = mount(t, 'Anna');
  await page.reply(ANNA, overview('£1.11', '£1.00', FIRST_CHARGES + 1));
  assert.equal(page.more.hidden, false);

  page.select.choose('Bert');
  isBlank(page);
  assert.equal(page.more.hidden, true);
  assert.ok(page.cards.every((card) => !card.hidden));
});

test('a late reply for an earlier player is dropped, after the next one has landed', async (t) => {
  const page = mount(t, 'Anna');
  page.select.choose('Bert');
  await page.reply(BERT, bert);
  showsPlayer(page, '£2.22', '£2.00');

  await page.reply(ANNA, anna);
  showsPlayer(page, '£2.22', '£2.00');
  assert.ok(!page.payments.innerHTML.includes('£1.11'));
});

test('a late reply for an earlier player is dropped, before the next one has landed', async (t) => {
  const page = mount(t, 'Anna');
  page.select.choose('Bert');
  await page.reply(ANNA, anna);
  isBlank(page);

  await page.reply(BERT, bert);
  showsPlayer(page, '£2.22', '£2.00');
});

test('a late failure for an earlier player raises no alert, whichever reply comes first', async (t) => {
  const landed = mount(t, 'Anna');
  landed.select.choose('Bert');
  await landed.reply(BERT, bert);
  await landed.fail(ANNA);
  assert.equal(landed.failed.hidden, true);
  assert.ok(landed.cards.every((card) => !card.hidden));
  showsPlayer(landed, '£2.22', '£2.00');

  const waiting = mount(t, 'Anna');
  waiting.select.choose('Bert');
  await waiting.fail(ANNA);
  assert.equal(waiting.failed.hidden, true);
  assert.ok(waiting.cards.every((card) => !card.hidden));
  await waiting.reply(BERT, bert);
  showsPlayer(waiting, '£2.22', '£2.00');
});

test('a failure shows the message and hides the cards, and Try again brings them back', async (t) => {
  const page = mount(t, 'Anna');
  await page.fail(ANNA);
  assert.equal(page.failed.hidden, false);
  assert.ok(page.cards.every((card) => card.hidden));
  isBlank(page);

  page.retry.fire('click');
  assert.equal(page.calls.length, 2);
  assert.equal(page.failed.hidden, true);
  assert.ok(page.cards.every((card) => !card.hidden));

  await page.reply(ANNA, anna);
  showsPlayer(page, '£1.11', '£1.00');
  assert.equal(page.failed.hidden, true);
});

test('a reply that is not ok counts as a failure', async (t) => {
  const page = mount(t, 'Anna');
  page.calls[0].resolve({ ok: false, status: 404, json: async () => anna });
  await settle();
  assert.equal(page.failed.hidden, false);
  isBlank(page);
});

test('Try again moves focus to the first card, and back to the button if it fails again', async (t) => {
  const page = mount(t, 'Anna');
  await page.fail(ANNA);
  assert.equal(page.doc.activeElement, null, 'a failure nobody asked about does not take focus');

  page.doc.activeElement = page.retry;
  page.retry.fire('click');
  assert.equal(page.doc.activeElement, page.cards[0]);

  await page.fail(ANNA);
  assert.equal(page.doc.activeElement, page.retry);

  page.retry.fire('click');
  assert.equal(page.doc.activeElement, page.cards[0]);
  await page.reply(ANNA, anna);
  assert.equal(page.doc.activeElement, page.cards[0]);
});

test('a second failure leaves focus alone if you have moved on, and a choice never takes it', async (t) => {
  const page = mount(t, 'Anna');
  await page.fail(ANNA);
  page.retry.fire('click');
  const elsewhere = {};
  page.doc.activeElement = elsewhere;
  await page.fail(ANNA);
  assert.equal(page.doc.activeElement, elsewhere);

  page.select.choose('Bert');
  assert.equal(page.doc.activeElement, elsewhere);
});

test('coming back to the same player leaves the tables alone, and to another reloads them', async (t) => {
  const page = mount(t, 'Anna');
  await page.reply(ANNA, overview('£1.11', '£1.00', FIRST_CHARGES + 1));
  page.more.fire('click');
  const tables = page.charges.innerHTML;

  page.win.fire('pageshow', { persisted: true });
  await settle();
  assert.equal(page.charges.innerHTML, tables);
  assert.equal(page.calls.length, 1);
  assert.ok(page.laterRows().every((row) => !row.hidden), 'show all survives');

  page.select.value = 'Bert';
  page.win.fire('pageshow', { persisted: false });
  assert.equal(page.charges.innerHTML, tables, 'an ordinary load is not a return');

  page.win.fire('pageshow', { persisted: true });
  isBlank(page);
  await page.reply(BERT, bert);
  showsPlayer(page, '£2.22', '£2.00');
});

test('the button shows the later charges and hides them again, and a new player starts over', async (t) => {
  const page = mount(t, 'Anna');
  await page.reply(ANNA, overview('£1.11', '£1.00', FIRST_CHARGES + 2));
  assert.equal(page.laterRows().length, 2);
  assert.ok(page.laterRows().every((row) => row.hidden));
  assert.equal(page.more.hidden, false);
  assert.equal(page.more.textContent, `Show all ${FIRST_CHARGES + 2}`);

  page.more.fire('click');
  assert.ok(page.laterRows().every((row) => !row.hidden));
  assert.equal(page.more.textContent, `Show the latest ${FIRST_CHARGES}`);

  page.more.fire('click');
  assert.ok(page.laterRows().every((row) => row.hidden));
  assert.equal(page.more.textContent, `Show all ${FIRST_CHARGES + 2}`);

  page.more.fire('click');
  page.select.choose('Bert');
  await page.reply(BERT, overview('£2.22', '£2.00', FIRST_CHARGES + 1));
  assert.ok(page.laterRows().every((row) => row.hidden));
  assert.equal(page.more.textContent, `Show all ${FIRST_CHARGES + 1}`);
});

test('twelve charges or fewer need no button', async (t) => {
  const page = mount(t, 'Anna');
  await page.reply(ANNA, overview('£1.11', '£1.00', FIRST_CHARGES));
  assert.equal(page.more.hidden, true);
});

test('the loading line waits 200 ms, goes with the reply, and never follows one', async (t) => {
  t.mock.timers.enable({ apis: ['setTimeout'] });
  const page = mount(t, 'Anna');

  t.mock.timers.tick(199);
  isBlank(page);
  t.mock.timers.tick(1);
  assert.ok(page.payments.innerHTML.includes(FEES.loading));
  assert.ok(page.charges.innerHTML.includes(FEES.loading));

  await page.reply(ANNA, anna);
  showsPlayer(page, '£1.11', '£1.00');
  assert.ok(!page.payments.innerHTML.includes(FEES.loading));

  page.select.choose('Bert');
  t.mock.timers.tick(100);
  await page.reply(BERT, bert);
  t.mock.timers.tick(300);
  showsPlayer(page, '£2.22', '£2.00');
  assert.ok(!page.charges.innerHTML.includes(FEES.loading));
});

test('a failure inside the 200 ms leaves no loading line behind', async (t) => {
  t.mock.timers.enable({ apis: ['setTimeout'] });
  const page = mount(t, 'Anna');
  await page.fail(ANNA);
  t.mock.timers.tick(300);
  isBlank(page);
});
