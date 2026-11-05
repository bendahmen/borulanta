// Sorting for the player table. It sorts on the exported value and never parses the
// string R formatted for display.

export const DEFAULT_SORT = { key: 'appearances', direction: 'descending' };

function sortValue(row, key) {
  if (key === 'player') return row.player;
  if (key === 'appearances') return row.appearances;
  return row.outcomes?.[key]?.estimate;
}

const isMissing = (value) => value === null || value === undefined || Number.isNaN(value);

// Uses < rather than subtraction, and rather than localeCompare for names: the default
// order then matches arrange() in R, which sorts in the C locale.
function compareAscending(a, b) {
  if (a < b) return -1;
  if (a > b) return 1;
  return 0;
}

// Missing values go last whichever way the column runs. Ties break on the player, A to
// Z, whichever way the column runs, as player_regression_table() does for appearances.
export function rowComparator({ key, direction }) {
  const sign = direction === 'descending' ? -1 : 1;
  return (rowA, rowB) => {
    const a = sortValue(rowA, key);
    const b = sortValue(rowB, key);
    const missingA = isMissing(a);
    const missingB = isMissing(b);
    if (missingA !== missingB) return missingA ? 1 : -1;
    const order = missingA ? 0 : sign * compareAscending(a, b);
    return order === 0 ? compareAscending(rowA.player, rowB.player) : order;
  };
}

export function sortRows(rows, sort) {
  return [...rows].sort(rowComparator(sort));
}

// Names start A to Z and numbers start with the largest. A second press reverses.
export function nextSort(current, key) {
  if (current.key === key) {
    return { key, direction: current.direction === 'ascending' ? 'descending' : 'ascending' };
  }
  return { key, direction: key === 'player' ? 'ascending' : 'descending' };
}

// On a phone or with large text only the selected outcome has a column. A sort on a column
// that is not showing moves to that outcome, so the rows are never in an order the reader
// cannot see, whether the outcome changed or the screen did. Any other sort stays put.
export function sortOnShownColumn(sort, isShown, outcome) {
  return isShown(sort.key) ? sort : { key: outcome, direction: sort.direction };
}
