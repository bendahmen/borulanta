// Only these strings reach the browser. fees.json is 385 KB, mostly overviews.
//
// A signing with no match yet is on the picker and not in balances, so overviews
// is the fallback. Both come from R: this reads the balance and never computes it.
export function feeRows(fees, statusWords) {
  return fees.fee_players.map((player) => {
    const row =
      fees.balances.find((balance) => balance.player === player) ??
      fees.overviews.find((overview) => overview.player === player);
    if (!row) throw new Error(`fees.json has no balance for ${player}`);

    let status = statusWords.square;
    if (row.balance > 0) status = statusWords.owed;
    if (row.balance < 0) status = statusWords.credit;

    return { player, display: row.balance_display, owed: row.balance > 0, status };
  });
}
