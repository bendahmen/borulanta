// Fetches a season combination's file. Finished results stay in memory for the visit,
// so going back to an earlier combination is instant, and a reply that arrives after
// the ticks have moved on is reported as stale rather than shown. Fees uses it too, for
// the chosen player's file, with `pick` keeping everything.

// The Effects page uses only these. The rest of a season's file is for the Stage 3 pages.
const pickRegression = ({ has_matches, regression }) => ({ has_matches, regression });

export function createSubsetLoader(fetchFile, pick = pickRegression) {
  const cache = new Map();
  let latest = 0;

  const fetchJson = async (file) => {
    const response = await fetchFile(file);
    if (!response.ok) throw new Error(`${file}: ${response.status}`);
    return pick(await response.json());
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
    // For when the choice moves to a state with no file at all.
    cancel() {
      latest += 1;
    },
  };
}
