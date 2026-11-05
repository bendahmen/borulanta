// A player's name as a piece of URL. The per-player fee files and the /players/<slug>/ pages
// use it, so it depends on the name alone: nobody's address changes when somebody else joins.

// /players/<slug>/ shares a folder with these two pages.
const RESERVED = ['roster', 'effects'];

export function playerSlug(name) {
  const slug = String(name)
    .normalize('NFKD')
    .replace(/[\u0300-\u036f]/g, '')
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, '-')
    .replace(/^-+|-+$/g, '');
  if (!slug) throw new Error(`Player "${name}" has no letters or digits to make an address from.`);
  return slug;
}

export const playerHref = (name) => `/players/${playerSlug(name)}/`;

// Every name's slug, or a failed build naming the two players who would share an address.
// Silently numbering one would hand a link to somebody else's balance.
export function playerSlugs(names) {
  const owners = new Map();
  const slugs = new Map();
  for (const name of new Set(names)) {
    const slug = playerSlug(name);
    if (RESERVED.includes(slug)) throw new Error(`Player "${name}" would take the address of the ${slug} page.`);
    if (owners.has(slug)) throw new Error(`Players "${owners.get(slug)}" and "${name}" would share the address "${slug}".`);
    owners.set(slug, name);
    slugs.set(name, slug);
  }
  return slugs;
}
