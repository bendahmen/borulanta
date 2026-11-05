// Who gets a /players/<slug>/ page, worked out once at build time.
import { playerSlugs } from './slug.js';

// Everyone with a profile in any combination of seasons, which is everyone with attendance on
// a match. A guest who only scored has none, so no page. `readSubset` takes a file's id and
// returns it. `otherNames` are the rest of the site's players, so that two people who would
// share an address stop the build whichever pages they are on.
export function playerPages(subsetIds, readSubset, otherNames = []) {
  const players = new Set(subsetIds.flatMap((id) => readSubset(id).profiles.map((profile) => profile.player)));
  const slugs = playerSlugs([...players, ...otherNames]);
  return [...players].sort().map((player) => ({ player, slug: slugs.get(player) }));
}
