// The date logic Home and the run-in share. It is the one logic allowed to duplicate R:
// upcoming_fixtures() in R/analysis.R. A static build would otherwise freeze the answer on
// the day it ran. Inlined by moduleToInline(), so keep it plain.

const LONDON = new Intl.DateTimeFormat('en-GB', {
  timeZone: 'Europe/London',
  year: 'numeric',
  month: '2-digit',
  day: '2-digit',
});

// The calendar date in London as yyyy-mm-dd, wherever the phone is.
export function londonDate(now) {
  const part = {};
  for (const { type, value } of LONDON.formatToParts(now)) part[type] = value;
  return `${part.year}-${part.month}-${part.day}`;
}

const utcDay = (iso) => {
  const [year, month, day] = iso.split('-').map(Number);
  return Date.UTC(year, month - 1, day);
};

export function daysBetween(from, to) {
  return (utcDay(to) - utcDay(from)) / 86400000;
}

// A fixture dated today counts as still to come: the game is in the evening.
export function upcomingFixtures(fixtures, today) {
  return fixtures.filter((fixture) => fixture.date >= today);
}

// A phone keeps a tab for days and a restored page runs no scripts, so anything about
// today has to be worked out again whenever the page comes back.
export function onReturn(paint) {
  addEventListener('pageshow', (event) => {
    if (event.persisted) paint();
  });
  document.addEventListener('visibilitychange', () => {
    if (!document.hidden) paint();
  });
}
