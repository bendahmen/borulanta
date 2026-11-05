<script>
  // Players > Effects. Every number comes from R's JSON: this file sorts them and hands
  // them to the plots, and shows R's own strings.
  import { onMount, untrack } from 'svelte';

  import { EFFECTS } from '../../lib/labels.js';
  import {
    cleanTicks,
    formatTicks,
    resolveTicks,
    searchWithTicks,
    subsetFile,
    ticksFromSearch,
    writeStoredTicks,
  } from '../../lib/season-ticks.js';
  import { createSubsetLoader } from '../../lib/subset-loader.js';
  import { DEFAULT_SORT } from '../../lib/table-sort.js';
  import AttackDefencePlot from './AttackDefencePlot.svelte';
  import CardHead from './CardHead.svelte';
  import CoefficientPlot from './CoefficientPlot.svelte';
  import PlayerTable from './PlayerTable.svelte';
  import SeasonPicker from './SeasonPicker.svelte';
  import Segmented from './Segmented.svelte';

  let { seasons, emptyStates, outcomeLabels, estimatorControl, cards, plotLabels } = $props();

  // The build sets these props once, so reading them once is right.
  const knownIds = untrack(() => seasons.map((season) => season.season_id));
  const outcomeOptions = untrack(() => Object.entries(outcomeLabels).map(([value, label]) => ({ value, label })));
  const getStorage = () => localStorage;
  const loader = createSubsetLoader((file) => fetch(file));

  // What the controls say at this moment, read before hydration overwrites them. A tap can
  // land between first paint and this code, and the native control has already shown it,
  // so it counts. The boxes also carry the URL or storage, once the inline script has run.
  function readControls() {
    const chosen = (name) => document.querySelector(`input[name="${name}"]:checked`)?.value;
    const boxes = [...document.querySelectorAll('input[data-season-tick]')];
    const ticked = boxes.filter((box) => box.checked).map((box) => box.value);
    return {
      ticks: document.documentElement.hasAttribute('data-ticks-set') ? cleanTicks(ticked, knownIds) : null,
      estimator: chosen('estimator'),
      outcome: chosen('outcome-plot'),
    };
  }

  const earlyControls = typeof document === 'undefined' ? null : readControls();

  // Everything ticked for the server's render and the first client render, so they
  // match. onMount then applies what was read above in the same frame, and the inline
  // script on the page has already set the boxes, so no wrong tick is ever painted.
  let ticks = $state.raw(cleanTicks(knownIds, knownIds));
  let estimator = $state('ols');
  let outcome = $state('points');
  let sort = $state.raw(DEFAULT_SORT);
  // What is on the page. It stays there while the next one loads, so nothing jumps.
  let subset = $state.raw(null);
  let failed = $state(false);
  let busy = $state(false);
  let charts = $state.raw(null);

  const regression = $derived(subset?.regression[estimator] ?? null);
  const players = $derived(regression?.players ?? []);

  const view = $derived.by(() => {
    if (ticks.length === 0) return 'no-matches';
    if (failed) return 'failed';
    if (!subset) return 'results';
    if (!subset.has_matches) return 'no-matches';
    return regression.has_regression ? 'results' : 'no-regression';
  });

  let busyTimer = 0;

  async function requestSubset() {
    clearTimeout(busyTimer);
    failed = false;
    const file = subsetFile(ticks);
    if (!file) {
      loader.cancel();
      subset = null;
      busy = false;
      return;
    }
    // A short wait, so a file already in memory or a quick reply never flickers.
    busyTimer = setTimeout(() => {
      busy = true;
    }, 200);
    try {
      const { stale, data } = await loader.load(file);
      if (stale) return;
      clearTimeout(busyTimer);
      busy = false;
      subset = data;
    } catch {
      clearTimeout(busyTimer);
      busy = false;
      failed = true;
    }
  }

  function loadCharts() {
    import('../../lib/effects-charts.js')
      .then((module) => {
        charts = module;
      })
      .catch(() => {
        failed = true;
      });
  }

  function writeTicksToUrl() {
    history.replaceState(history.state, '', searchWithTicks(location.search, ticks) + location.hash);
  }

  function changeTick(id, checked) {
    ticks = cleanTicks(checked ? [...ticks, id] : ticks.filter((tick) => tick !== id), knownIds);
    writeTicksToUrl();
    writeStoredTicks(getStorage, ticks);
    requestSubset();
  }

  function retry() {
    if (!charts) loadCharts();
    requestSubset();
  }

  onMount(() => {
    const resolved = resolveTicks(location.search, getStorage, knownIds);
    ticks = earlyControls.ticks ?? resolved;
    estimator = earlyControls.estimator ?? estimator;
    outcome = earlyControls.outcome ?? outcome;
    if (formatTicks(ticks) !== formatTicks(resolved)) {
      // A tap beat us here, so remember it as any tap would. The inline script has already
      // asked for the file the old ticks wanted: this takes it, for the cache, and spares
      // the browser's warning about a preload nobody used.
      const preloaded = subsetFile(resolved);
      if (preloaded) loader.load(preloaded);
      writeTicksToUrl();
      writeStoredTicks(getStorage, ticks);
    } else if (ticksFromSearch(location.search, knownIds) === null && ticks.length < knownIds.length) {
      // Ticks restored from storage go in the URL too, so a copied link shows the same.
      writeTicksToUrl();
    }
    requestSubset();
    loadCharts();
  });
</script>

<div class="effects">
  <section class="card controls">
    <SeasonPicker {seasons} {ticks} onchange={changeTick} />
    {#if view !== 'no-matches'}
      <!-- Stays up when this estimator has no rows: ridge can still fit, as on season 3 alone. -->
      <Segmented
        legend={estimatorControl.legend}
        name="estimator"
        options={estimatorControl.options}
        value={estimator}
        onchange={(next) => (estimator = next)}
      />
    {/if}
  </section>

  {#if view === 'no-matches'}
    <p class="card message" role="status">{emptyStates.no_matches}</p>
  {:else if view === 'failed'}
    <div class="card message" role="alert">
      <p>{EFFECTS.loadFailed}</p>
      <button type="button" class="retry" onclick={retry}>{EFFECTS.retry}</button>
    </div>
  {:else if view === 'no-regression'}
    <p class="card message" role="status">{emptyStates.no_regression}</p>
  {:else}
    <div class="results" class:is-busy={busy} aria-busy={busy || !subset}>
      <div class="plots">
        <section class="card" aria-labelledby="attack-defence-title">
          <CardHead id="attack-defence-title" card={cards.attack_defence} />
          <AttackDefencePlot
            {charts}
            {players}
            labels={plotLabels.attack_defence[estimator]}
            title={cards.attack_defence.title}
            description={cards.attack_defence.subtitle}
          />
        </section>

        <section class="card" aria-labelledby="coefficient-title">
          <CardHead id="coefficient-title" card={cards.coefficient_plot} />
          <div class="outcome">
            <Segmented
              legend={cards.coefficient_plot.picker_label}
              name="outcome-plot"
              layout="grid"
              options={outcomeOptions}
              value={outcome}
              onchange={(next) => (outcome = next)}
            />
          </div>
          <CoefficientPlot
            {charts}
            {players}
            {outcome}
            axisLabel={plotLabels.coefficient[estimator][outcome].y}
            title={cards.coefficient_plot.title}
          />
        </section>
      </div>

      <section class="card" aria-labelledby="player-effects-title">
        <CardHead id="player-effects-title" card={cards.player_effects} />
        {#if regression}
          <p class="note">{regression.note}</p>
          <PlayerTable
            {players}
            {outcome}
            {outcomeLabels}
            {sort}
            onsort={(next) => (sort = next)}
            caption={cards.player_effects.title}
          >
            <!-- For when the table shows one outcome, as this is a long way below the plot's control. -->
            {#snippet control()}
              <div class="outcome">
                <Segmented
                  legend={cards.coefficient_plot.picker_label}
                  name="outcome-table"
                  layout="grid"
                  options={outcomeOptions}
                  value={outcome}
                  onchange={(next) => (outcome = next)}
                />
              </div>
            {/snippet}
          </PlayerTable>
        {/if}
      </section>

      <section class="card" aria-labelledby="method-title">
        <CardHead id="method-title" card={cards.method} />
        {#if regression}
          <div class="method">
            {#each regression.explanation as paragraph, index (index)}
              <p>{paragraph}</p>
            {/each}
          </div>
        {/if}
      </section>
    </div>
  {/if}
</div>

<style>
  .effects,
  .results,
  .plots {
    display: grid;
    gap: 10px;
    min-width: 0;
  }

  .results > *,
  .plots > * {
    min-width: 0;
  }

  .controls {
    display: grid;
    gap: 10px;
    padding-top: 10px;
    padding-bottom: 12px;
  }

  .message {
    color: var(--ink-muted);
  }

  .retry {
    display: inline-flex;
    align-items: center;
    min-height: 44px;
    margin-top: 6px;
    padding: 0 16px;
    border: 1px solid var(--blue);
    border-radius: 12px;
    background: var(--surface);
    color: var(--blue);
    font: inherit;
    font-weight: 600;
    cursor: pointer;
  }

  .outcome {
    margin-top: 10px;
  }

  .note {
    margin-top: 4px;
    color: var(--ink-muted);
    font-size: 0.9375rem;
    line-height: 1.35;
  }

  .method {
    display: grid;
    gap: 10px;
    margin-top: 8px;
    max-width: 70ch;
    line-height: 1.45;
  }

  .is-busy {
    opacity: 0.55;
  }

  @media (min-width: 768px) {
    .effects,
    .results {
      gap: 16px;
    }

    .plots {
      grid-template-columns: minmax(0, 1fr) minmax(0, 1fr);
      gap: 16px;
    }

    .controls {
      grid-template-columns: minmax(0, 3fr) minmax(0, 2fr);
      gap: 16px;
      align-items: start;
    }
  }

  @media (prefers-reduced-motion: no-preference) {
    .results {
      transition: opacity 0.15s ease;
    }
  }
</style>
