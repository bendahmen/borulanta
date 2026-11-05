// One file per player for Fees > You, which fetches only the one you chose. Every figure
// is R's, passed through as exported. Two players on one address fail the build here.
import fees from '../../../generated/fees.json';
import { playerSlugs } from '../../../lib/slug.js';

export function getStaticPaths() {
  const slugs = playerSlugs(fees.overviews.map((overview) => overview.player));
  return fees.overviews.map((overview) => ({
    params: { slug: slugs.get(overview.player) },
    props: { overview },
  }));
}

export function GET({ props }) {
  return new Response(JSON.stringify(props.overview), {
    headers: { 'Content-Type': 'application/json' },
  });
}
