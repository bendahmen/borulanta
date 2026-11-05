// Your payments and charges on Fees > You. They are in one small file per player, because
// the whole of fees.json would be 385 KB on every visit. Inlined by moduleToInline(), after
// data-table.js and subset-loader.js, so keep it plain. Every figure is R's own string.

import { escapeHtml, moreRowsLabel, tableHtml } from './data-table.js';
import { createSubsetLoader } from './subset-loader.js';

// The page length match_charge_table() gives its pager.
export const FIRST_CHARGES = 12;

export function feeFile(slug) {
  return `/data/fees/${slug}.json`;
}

export function paymentColumns(headings) {
  return [
    { key: 'date', label: headings.date, value: (row) => row.date_short, lead: true },
    { key: 'payment', label: headings.payment, value: (row) => row.amount_display, number: true },
  ];
}

// All seven of match_charge_table(), in its order. A phone keeps three and the rest open
// under the row.
export function chargeColumns(headings, played) {
  return [
    { key: 'date', label: headings.date, value: (row) => row.date_short, lead: true },
    { key: 'season', label: headings.season, value: (row) => row.season, folds: true },
    { key: 'result', label: headings.result, value: (row) => row.result, number: true },
    { key: 'played', label: headings.played, value: (row) => (row.played ? played.yes : played.no), folds: true },
    { key: 'squad', label: headings.squad_size, value: (row) => row.squad_size, folds: true, number: true },
    { key: 'charge', label: headings.charge, value: (row) => row.charge_display, number: true },
    { key: 'explanation', label: headings.explanation, value: (row) => row.explanation, folds: true, long: true },
  ];
}

// The words on the button under the charges, or null when every row already shows.
export function moreChargesLabel(count, showingAll, words) {
  return moreRowsLabel(count, FIRST_CHARGES, showingAll, words);
}

export function paymentsHtml(overview, card, words) {
  if (overview.payment_history.length === 0) return `<p class="dt-empty">${escapeHtml(words.noPayments)}</p>`;
  return tableHtml({ className: 'dt-payments', caption: card.title, columns: paymentColumns(card.headings), rows: overview.payment_history });
}

export function chargesHtml(overview, card, words) {
  if (overview.match_charges.length === 0) return `<p class="dt-empty">${escapeHtml(words.noCharges)}</p>`;
  return tableHtml({
    className: 'dt-charges',
    caption: card.title,
    columns: chargeColumns(card.headings, card.played),
    rows: overview.match_charges,
    rowAttributes: (row, index) => (index < FIRST_CHARGES ? {} : { 'data-later': true, hidden: true }),
  });
}

export function mountFeeHistory(root, select, data, fetchFile) {
  const part = (name) => root.querySelector(`[data-history="${name}"]`);
  const cards = root.querySelectorAll('[data-history-card]');
  const loader = createSubsetLoader(fetchFile, (overview) => overview);
  let shown = null;
  let showingAll = false;
  let chargeCount = 0;
  let busyTimer = 0;

  const paintMore = () => {
    const label = moreChargesLabel(chargeCount, showingAll, data.words);
    part('more').hidden = !label;
    part('more').textContent = label || '';
    part('charges').querySelectorAll('tr[data-later]').forEach((row) => {
      row.hidden = !showingAll;
    });
  };

  const clear = () => {
    clearTimeout(busyTimer);
    showingAll = false;
    chargeCount = 0;
    part('payments').replaceChildren();
    part('charges').replaceChildren();
    part('more').hidden = true;
    part('failed').hidden = true;
  };

  const showCards = (visible) => {
    cards.forEach((card) => {
      card.hidden = !visible;
    });
  };

  // Nothing of the last player stays up while the next one loads.
  const load = async (fromRetry = false) => {
    const chosen = select.options[select.selectedIndex];
    const slug = chosen ? chosen.dataset.slug : '';
    if (slug === shown) return;
    shown = null;
    clear();
    if (!slug) {
      loader.cancel();
      showCards(false);
      return;
    }
    showCards(true);
    // "Try again" has just been hidden, which would drop focus to the page itself. The
    // first card is where the answer lands, and it is named, so a screen reader says so.
    if (fromRetry) cards[0].focus();
    // A short wait, so a file already in memory or a quick reply never flickers.
    busyTimer = setTimeout(() => {
      const line = `<p class="dt-empty">${escapeHtml(data.words.loading)}</p>`;
      part('payments').innerHTML = line;
      part('charges').innerHTML = line;
    }, 200);
    try {
      const { stale, data: overview } = await loader.load(feeFile(slug));
      if (stale) return;
      clearTimeout(busyTimer);
      part('payments').innerHTML = paymentsHtml(overview, data.payments, data.words);
      part('charges').innerHTML = chargesHtml(overview, data.charges, data.words);
      chargeCount = overview.match_charges.length;
      paintMore();
      shown = slug;
    } catch {
      // Back to the button if the card still has focus, and not if you have moved on.
      const holdsFocus = fromRetry && document.activeElement === cards[0];
      clear();
      showCards(false);
      part('failed').hidden = false;
      if (holdsFocus) part('retry').focus();
    }
  };

  select.addEventListener('change', () => load());
  part('more').addEventListener('click', () => {
    showingAll = !showingAll;
    paintMore();
  });
  part('retry').addEventListener('click', () => load(true));
  addEventListener('pageshow', (event) => {
    if (event.persisted) load();
  });
  load();
}
