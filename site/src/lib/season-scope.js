// What a season-scoped page does with its ticks: which seasons are ticked, keeping the URL
// and storage in step, and loading the file for them. The page draws whatever this reports.
// The picker is the same on Players > Effects, Matches > Results and Matches > Trends.
import {
  cleanTicks,
  formatTicks,
  resolveTicks,
  searchWithTicks,
  subsetFile,
  ticksFromSearch,
  writeStoredTicks,
} from './season-ticks.js';
import { createSubsetLoader } from './subset-loader.js';

// A short wait, so a file already in memory or a quick reply never flickers.
const BUSY_DELAY_MS = 200;

// What the boxes say at this moment, to be read before hydration overwrites them. A tap can
// land between first paint and hydration, and the native control has already shown it, so
// it counts. Null until the inline script has set the boxes from the URL or storage.
export function readEarlyTicks(doc, knownIds) {
  if (!doc.documentElement.hasAttribute('data-ticks-set')) return null;
  const boxes = [...doc.querySelectorAll('input[data-season-tick]')];
  return cleanTicks(boxes.filter((box) => box.checked).map((box) => box.value), knownIds);
}

// What such a page should be showing. Zero ticks has no file, so it needs no answer from
// one. `loading` covers the wait for the first file, when the cards are up with nothing in them.
export function scopeView({ ticks, failed, subset }) {
  if (ticks.length === 0) return 'no-matches';
  if (failed) return 'failed';
  if (!subset) return 'loading';
  return subset.has_matches ? 'ready' : 'no-matches';
}

// `onchange` gets the whole state after every change. Everything ticked is the starting
// state for the server's render and the first client render, so that they match. start()
// then applies the real ticks in the same frame, and the inline script has already set the
// boxes, so no wrong tick is ever painted.
export function createSeasonScope({ knownIds, fetchFile, pick, onchange, win = globalThis }) {
  const getStorage = () => win.localStorage;
  const loader = createSubsetLoader(fetchFile, pick);
  let busyTimer = 0;
  let state = {
    ticks: cleanTicks(knownIds, knownIds),
    // What is on the page. It stays there while the next one loads, so nothing jumps.
    subset: null,
    failed: false,
    busy: false,
  };

  const update = (changes) => {
    state = { ...state, ...changes };
    onchange(state);
  };

  const writeTicksToUrl = () => {
    win.history.replaceState(win.history.state, '', searchWithTicks(win.location.search, state.ticks) + win.location.hash);
  };

  async function reload() {
    clearTimeout(busyTimer);
    update({ failed: false });
    const file = subsetFile(state.ticks);
    if (!file) {
      loader.cancel();
      update({ subset: null, busy: false });
      return;
    }
    busyTimer = setTimeout(() => update({ busy: true }), BUSY_DELAY_MS);
    try {
      const { stale, data } = await loader.load(file);
      if (stale) return;
      clearTimeout(busyTimer);
      update({ busy: false, subset: data });
    } catch {
      clearTimeout(busyTimer);
      update({ busy: false, failed: true });
    }
  }

  return {
    get state() {
      return state;
    },
    start(earlyTicks) {
      const resolved = resolveTicks(win.location.search, getStorage, knownIds);
      const fromUrl = ticksFromSearch(win.location.search, knownIds);
      update({ ticks: earlyTicks ?? resolved });
      if (formatTicks(state.ticks) !== formatTicks(resolved)) {
        // A tap beat us here, so remember it as any tap would. The inline script has already
        // asked for the file the old ticks wanted: this takes it, for the cache, and spares
        // the browser's warning about a preload nobody used.
        const preloaded = subsetFile(resolved);
        if (preloaded) loader.load(preloaded);
        writeTicksToUrl();
        writeStoredTicks(getStorage, state.ticks);
      } else if (fromUrl !== null) {
        // A shared link's ticks follow the visitor to the next page, whose links carry no query.
        writeStoredTicks(getStorage, state.ticks);
      } else if (state.ticks.length < knownIds.length) {
        // Ticks restored from storage go in the URL too, so a copied link shows the same.
        writeTicksToUrl();
      }
      return reload();
    },
    changeTick(id, checked) {
      update({ ticks: cleanTicks(checked ? [...state.ticks, id] : state.ticks.filter((tick) => tick !== id), knownIds) });
      writeTicksToUrl();
      writeStoredTicks(getStorage, state.ticks);
      return reload();
    },
    reload,
    // For a page whose other parts can fail too, such as a chart's chunk.
    fail() {
      update({ failed: true });
    },
  };
}
