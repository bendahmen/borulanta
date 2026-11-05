// Fetches a season combination's file. Finished results stay in memory for the visit,
// so going back to an earlier combination is instant, and a reply that arrives after
// the ticks have moved on is reported as stale rather than shown.

export function createSubsetLoader(fetchFile) {
  const cache = new Map();
  let latest = 0;

  const fetchJson = async (file) => {
    const response = await fetchFile(file);
    if (!response.ok) throw new Error(`${file}: ${response.status}`);
    // The page uses only these. The rest of the file is for the Stage 3 pages.
    const { has_matches, regression } = await response.json();
    return { has_matches, regression };
  };

  return {
    async load(file) {
      latest += 1;
      const request = latest;
      if (!cache.has(file)) {
        const pending = fetchJson(file);
        // A failure must not stay in the cache, or a retry could never succeed.
        pending.catch(() => cache.delete(file));
        cache.set(file, pending);
      }
      try {
        const data = await cache.get(file);
        return { stale: request !== latest, data };
      } catch (error) {
        if (request !== latest) return { stale: true, data: null };
        throw error;
      }
    },
    // For when the ticks move to a state with no file at all.
    cancel() {
      latest += 1;
    },
  };
}
