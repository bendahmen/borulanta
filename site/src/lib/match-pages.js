// What each /matches/<date>/ page is built from, worked out once at build time. Any match
// the export wrote and these cannot place fails the build here, so a broken page never ships.
import { playerSlugs } from './slug.js';

// Newest first, as the export writes them. Each lineup is read from that match's own season,
// because a shared link cannot depend on what somebody has ticked. `readSeason` takes a
// season id and returns its file, and `otherNames` are the rest of the site's players, so
// that two people who would share an address stop the build whichever page they are on.
export function matchPages(matches, readSeason, otherNames = []) {
  const dates = new Set();
  for (const { date } of matches) {
    if (dates.has(date)) throw new Error(`Two matches share the date ${date}, so they would share an address.`);
    dates.add(date);
  }

  const newestFirst = [...matches].sort((a, b) => (a.date < b.date ? 1 : -1));
  const pages = newestFirst.map((match, index) => {
    const entry = readSeason(match.season_id).match_details.find((detail) => detail.date === match.date);
    if (!entry) throw new Error(`The match on ${match.date} has no lineup entry in ${match.season_id}.json.`);
    return {
      match,
      lineup: entry.lineup,
      next: newestFirst[index - 1] ?? null,
      previous: newestFirst[index + 1] ?? null,
    };
  });

  playerSlugs([...pages.flatMap((page) => page.lineup.map((row) => row.player)), ...otherNames]);
  return pages;
}

// "Borulanta 2-3 The Dirty Boys". A match from before the sync has no opponent on file, so its
// date is what tells one 4-5 from another.
export function matchTitle(match, ourTeam) {
  const score = `${ourTeam} ${match.result}`;
  return match.opponent ? `${score} ${match.opponent}` : `${score}, ${match.date_label}`;
}

// R's own line and squad size for a link preview. The squad is always the card's first
// detail. No names: the scorers can stay on the page.
export function matchDescription(match) {
  const [squad] = match.details;
  return squad ? `${match.meta_line} · ${squad.value}` : match.meta_line;
}
