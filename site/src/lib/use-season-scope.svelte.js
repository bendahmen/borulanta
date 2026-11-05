// A season scope as Svelte state, for a page's component to call while it is setting up.
// That timing is the point: it is what reads the boxes before hydration overwrites them.
import { createSeasonScope, readEarlyTicks } from './season-scope.js';

export function useSeasonScope(seasons, pick) {
  const knownIds = seasons.map((season) => season.season_id);
  const earlyTicks = typeof document === 'undefined' ? null : readEarlyTicks(document, knownIds);
  const scope = createSeasonScope({
    knownIds,
    pick,
    fetchFile: (file) => fetch(file),
    onchange: (next) => {
      state = next;
    },
  });
  let state = $state.raw(scope.state);

  return {
    get state() {
      return state;
    },
    start: () => scope.start(earlyTicks),
    changeTick: scope.changeTick,
    reload: scope.reload,
    fail: scope.fail,
  };
}
