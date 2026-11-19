// Only these strings reach the browser. fees.json is 385 KB, mostly overviews.
//
// A signing with no match yet is on the picker and not in balances, so overviews
// is the fallback. Both come from R: this reads the balance and never computes it.
import { formatDate } from './dates.js';
import { tableDate } from './wording.js';

export function balanceState(balance) {
  if (balance > 0) return 'owed';
  if (balance < 0) return 'credit';
  return 'square';
}

export function feeRows(fees, statusWords) {
  return fees.fee_players.map((player) => {
    const row =
      fees.balances.find((balance) => balance.player === player) ??
      fees.overviews.find((overview) => overview.player === player);
    if (!row) throw new Error(`fees.json has no balance for ${player}`);

    const state = balanceState(row.balance);
    return { player, display: row.balance_display, owed: state === 'owed', status: statusWords[state] };
  });
}

// What the balance on the Fee check card is the difference of.
export function feeBreakdown(fees, player) {
  const overview = fees.overviews.find((row) => row.player === player);
  if (!overview) throw new Error(`fees.json has no overview for ${player}`);
  return { charges: overview.total_charges_display, payments: overview.total_payments_display };
}

// The Everyone table, in the order all_player_balances() returns. Picked out by row: it is
// the person that is owed or owing, not the number. The word
// says the same thing as the row's colour, for anyone who cannot see it.
export function balanceColumns(headings, statusWords) {
  const cue = (row) => (balanceState(row.balance) === 'square' ? '' : statusWords[balanceState(row.balance)]);
  return [
    { key: 'player', label: headings.player, value: (row) => row.player, lead: true, cue },
    { key: 'charges', label: headings.charges, value: (row) => row.charges_display, folds: true, number: true },
    { key: 'payments', label: headings.payments, value: (row) => row.payments_display, folds: true, number: true },
    { key: 'last-payment', label: headings.last_payment, value: (row) => tableDate(row.last_payment), folds: true, number: true },
    { key: 'balance', label: headings.balance, value: (row) => row.balance_display, number: true },
  ];
}

export function balanceRowClass(row) {
  const state = balanceState(row.balance);
  return state === 'square' ? '' : `is-${state}`;
}

// One player's overview as their file for Fees > You has it: each date with the string its
// table shows beside it, so the script that draws the tables writes no dates.
const withDateShort = ({ date, ...rest }) => ({ date, date_short: formatDate(date, '%d %b %Y'), ...rest });

export function feeFileOverview(overview) {
  return {
    ...overview,
    match_charges: overview.match_charges.map(withDateShort),
    payment_history: overview.payment_history.map(withDateShort),
  };
}
