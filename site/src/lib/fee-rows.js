// Only these strings reach the browser. fees.json is 385 KB, mostly overviews.
//
// A signing with no match yet is on the picker and not in balances, so overviews
// is the fallback. Both come from R: this reads the balance and never computes it.

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

// What the balance on the Fee check card is the difference of, as fee_overview_summary().
export function feeBreakdown(fees, player) {
  const overview = fees.overviews.find((row) => row.player === player);
  if (!overview) throw new Error(`fees.json has no overview for ${player}`);
  return { charges: overview.total_charges_display, payments: overview.total_payments_display };
}

// The Everyone table, in the order all_player_balances() returns. Picked out by row, as
// balance_table() does: it is the person that is owed or owing, not the number. The word
// says the same thing as the row's colour, for anyone who cannot see it.
export function balanceColumns(headings, statusWords) {
  const cue = (row) => (balanceState(row.balance) === 'square' ? '' : statusWords[balanceState(row.balance)]);
  return [
    { key: 'player', label: headings.player, value: (row) => row.player, lead: true, cue },
    { key: 'charges', label: headings.charges, value: (row) => row.charges_display, folds: true, number: true },
    { key: 'payments', label: headings.payments, value: (row) => row.payments_display, folds: true, number: true },
    { key: 'last-payment', label: headings.last_payment, value: (row) => row.last_payment_display, folds: true, number: true },
    { key: 'balance', label: headings.balance, value: (row) => row.balance_display, number: true },
  ];
}

export function balanceRowClass(row) {
  const state = balanceState(row.balance);
  return state === 'square' ? '' : `is-${state}`;
}
