// Which seasons are ticked, where that is remembered and which precomputed file holds
// the answer. Inlined into the page by moduleToInline(), so that the ticks are right
// before first paint and the data starts loading early. Keep it plain.

const SEASONS_KEY = 'borulanta.seasons';
const SEASONS_PARAM = 'seasons';

// Known ids, once each, in default sort order. On these ids that matches R's C-locale
// sort, which is how the files are named.
export function cleanTicks(ids, known) {
  return known.filter((id) => ids.includes(id)).sort();
}

export function parseTicks(text, known) {
  return cleanTicks(
    String(text)
      .split(',')
      .map((id) => id.trim()),
    known,
  );
}

export function formatTicks(ticks) {
  return ticks.join(',');
}

// Null when the URL says nothing. A param with no known ids means none ticked.
export function ticksFromSearch(search, known) {
  const value = new URLSearchParams(search).get(SEASONS_PARAM);
  return value === null ? null : parseTicks(value, known);
}

// Takes a function because merely touching localStorage can throw when it is blocked.
// Nothing usable reads as null, so that a visitor who left with every box empty comes
// back to the whole picture rather than to an empty page.
export function readStoredTicks(getStorage, known) {
  try {
    const stored = getStorage().getItem(SEASONS_KEY);
    if (stored === null) return null;
    const ticks = parseTicks(stored, known);
    return ticks.length > 0 ? ticks : null;
  } catch {
    return null;
  }
}

export function writeStoredTicks(getStorage, ticks) {
  try {
    getStorage().setItem(SEASONS_KEY, formatTicks(ticks));
  } catch {
    // Blocked storage only means the ticks are not remembered next time.
  }
}

export function resolveTicks(search, getStorage, known) {
  return ticksFromSearch(search, known) ?? readStoredTicks(getStorage, known) ?? cleanTicks(known, known);
}

// Written by hand because URLSearchParams would turn the commas into %2C.
export function searchWithTicks(search, ticks) {
  const params = new URLSearchParams(search);
  params.delete(SEASONS_PARAM);
  const rest = params.toString();
  return `?${rest ? `${rest}&` : ''}${SEASONS_PARAM}=${formatTicks(ticks)}`;
}

// No ticks has no file.
export function subsetFile(ticks) {
  return ticks.length > 0 ? `/data/seasons/${[...ticks].sort().join('-')}.json` : null;
}
