// Which rows of the run-in still count, worked out in the browser on each visit, with the
// same tested logic as Home. Every row is in the HTML, hidden until this has run, so a
// stale one never shows. With scripts off the page shows them all. Inlined by
// moduleToInline(), after fixture-dates.js, so keep it plain.

import { londonDate, onReturn, upcomingFixtures } from './fixture-dates.js';

export function paintRunIn(root, today) {
  const rows = [...root.querySelectorAll('tr[data-date]')].map((row) => ({ date: row.dataset.date, row }));
  const upcoming = upcomingFixtures(rows, today);
  for (const { row } of rows) row.hidden = !upcoming.some((fixture) => fixture.row === row);
  const table = root.querySelector('table');
  if (table) table.hidden = upcoming.length === 0;
  root.querySelector('[data-run-in="empty"]').hidden = upcoming.length > 0;
}

export function mountRunIn(root) {
  const paint = () => paintRunIn(root, londonDate(new Date()));
  paint();
  onReturn(paint);
}
