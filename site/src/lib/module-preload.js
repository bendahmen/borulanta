// Astro starts an island's scripts one level at a time: the page names the entry, the
// entry names its imports, those name theirs. On slow 4G each level costs a round trip
// of over half a second. Listing every chunk in the page's head fetches them together.
// Reads the built files, so a miss only costs speed and can never break a page.

const IMPORT_SPECIFIER = /(?:from|import)\s*\(?\s*["'`]\.\/([^"'`]+\.js)["'`]/g;
const ISLAND_URL = /(?:component|renderer)-url="\/_astro\/([^"]+\.js)"/g;

export function chunkImports(source) {
  return [...source.matchAll(IMPORT_SPECIFIER)].map((match) => match[1]);
}

// Dynamic imports count too: the Effects page imports Plot that way so that its controls
// work before Plot arrives, but its first view still needs Plot.
export function reachableChunks(entries, readChunk) {
  const seen = new Set();
  const queue = [...entries];
  while (queue.length > 0) {
    const name = queue.shift();
    if (seen.has(name)) continue;
    seen.add(name);
    queue.push(...chunkImports(readChunk(name)));
  }
  return [...seen];
}

// Ahead of the font preloads, because a host on HTTP/1.1 has only six connections and the
// fonts can show a fallback while they wait, but the plot cannot draw without its chunk.
// A page with no island has no chunks, and comes back unchanged.
export function addModulePreloads(html, readChunk) {
  const entries = [...html.matchAll(ISLAND_URL)].map((match) => match[1]);
  if (entries.length === 0) return html;
  const links = reachableChunks(entries, readChunk)
    .map((name) => `<link rel="modulepreload" href="/_astro/${name}">`)
    .join('');
  return html.replace('<head>', `<head>${links}`);
}
