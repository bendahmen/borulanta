// Which fixture is next, worked out in the browser on each visit. This is the one
// date logic allowed to duplicate R: upcoming_fixtures(), next_match_card_ui() and
// stale_fixture_note() in R/. It is imported by the unit tests and inlined into
// the page by moduleToInline(), so keep it plain: no imports, no multi-line strings.

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

export function whenWord(daysAway, wording) {
  if (daysAway === 0) return wording.tonight;
  if (daysAway === 1) return wording.tomorrow;
  return wording.in_days.replace('{days}', String(daysAway));
}

export function isStale(scrapedOn, today, staleDays) {
  if (!scrapedOn) return false;
  return daysBetween(scrapedOn, today) > staleDays;
}

export function nextMatchView(data, today) {
  const next = upcomingFixtures(data.fixtures, today)[0];
  if (!next) {
    return { empty: data.have_fixtures ? data.wording.no_fixtures_left : data.wording.no_fixture_list };
  }
  const daysAway = daysBetween(today, next.date);
  return {
    when: whenWord(daysAway, data.wording),
    isToday: daysAway === 0,
    opponent: next.opponent || data.wording.opponent_unknown,
    dateLabel: next.date_label,
    kickoffPitch: next.kickoff_pitch || null,
    staleNote: isStale(data.scraped_on, today, data.stale_days) ? data.stale_note : null,
  };
}

export function paintNextMatch(root, view) {
  const fill = (slot, text) => {
    const element = root.querySelector(`[data-next="${slot}"]`);
    element.textContent = text || '';
    element.hidden = !text;
  };
  fill('empty', view.empty);
  fill('when', view.when);
  fill('opponent', view.opponent);
  fill('date', view.dateLabel);
  fill('kickoff', view.kickoffPitch);
  fill('stale', view.staleNote);
  root.classList.toggle('is-today', Boolean(view.isToday));
}

// A phone keeps a tab for days and a restored page runs no scripts, so "Tonight"
// has to be worked out again whenever the page comes back.
export function mountNextMatch(root, data) {
  const paint = () => paintNextMatch(root, nextMatchView(data, londonDate(new Date())));
  paint();
  addEventListener('pageshow', (event) => {
    if (event.persisted) paint();
  });
  document.addEventListener('visibilitychange', () => {
    if (!document.hidden) paint();
  });
}
