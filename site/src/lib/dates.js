// Every date on the site, written from R's ISO string in one of the six ways R wrote them,
// with R's own pattern as the name. The string is split by hand and the names are written
// out, with no Intl and no Date: Node's en-GB gives "Wed, 09 Sept 2026" where R gives
// "Wed 09 Sep 2026", and a Date would bring the time zone into a calendar date. Every date is
// written at build time or by an island, so no inline script carries this.

const WEEKDAYS = ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'];
const MONTHS = ['January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'];

// Sakamoto's method, for the weekday of a date in the Gregorian calendar.
const MONTH_OFFSETS = [0, 3, 2, 5, 0, 3, 5, 1, 4, 6, 2, 4];

function weekday(year, month, day) {
  const y = month < 3 ? year - 1 : year;
  return (y + Math.floor(y / 4) - Math.floor(y / 100) + Math.floor(y / 400) + MONTH_OFFSETS[month - 1] + day) % 7;
}

function parts(iso) {
  const [year, month, day] = iso.split('-').map(Number);
  return { year, month, day };
}

const twoDigits = (n) => String(n).padStart(2, '0');

const FIELDS = {
  '%a': ({ year, month, day }) => WEEKDAYS[weekday(year, month, day)],
  '%d': ({ day }) => twoDigits(day),
  '%b': ({ month }) => MONTHS[month - 1].slice(0, 3),
  '%B': ({ month }) => MONTHS[month - 1],
  '%Y': ({ year }) => String(year),
};

export const DATE_FORMATS = ['%a %d %b %Y', '%d %b %Y', '%d %B %Y', '%d %b', '%b %Y', '%b'];

// `format` is one of DATE_FORMATS. Anything else is a mistake, so it throws.
export function formatDate(iso, format) {
  if (!DATE_FORMATS.includes(format)) throw new Error(`No date format ${format}`);
  const date = parts(iso);
  return format.replace(/%[a-zA-Z]/g, (field) => FIELDS[field](date));
}
