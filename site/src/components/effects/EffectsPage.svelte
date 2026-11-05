<script>
  // Players > Effects. Every number comes from R's JSON: this file sorts them and hands
  // them to the plots, and shows R's own strings.
  import { onMount, untrack } from 'svelte';

  import { scopeView } from '../../lib/season-scope.js';
  import { DEFAULT_SORT } from '../../lib/table-sort.js';
  import { useSeasonScope } from '../../lib/use-season-scope.svelte.js';
  import AttackDefencePlot from './AttackDefencePlot.svelte';
  import CardHead from '../CardHead.svelte';
  import CoefficientPlot from './CoefficientPlot.svelte';
  import PlayerTable from './PlayerTable.svelte';
  import ScopeMessage from '../ScopeMessage.svelte';
  import SeasonPicker from '../SeasonPicker.svelte';
  import Segmented from '../Segmented.svelte';

  let { seasons, emptyStates, outcomeLabels, estimatorControl, cards, plotLabels } = $props();

  // The build sets these props once, so reading them once is right.
  const outcomeOptions = untrack(() => Object.entries(outcomeLabels).map(([value, label]) => ({ value, label })));
  const scope = useSeasonScope(untrack(() => seasons));

  // What these two controls say at this moment, read before hydration overwrites them, for
  // the same reason as the ticks. See readEarlyTicks().
  function readControls() {
    const chosen = (name) => document.querySelector(`input[name="${name}"]:checked`)?.value;
    return { estimator: chosen('estimator'), outcome: chosen('outcome-plot') };
  }

  const earlyControls = typeof document === 'undefined' ? null : readControls();

  let estimator = $state('ols');
  let outcome = $state('points');
  let sort = $state.raw(DEFAULT_SORT);
  let charts = $state.raw(null);

  const ticks = $derived(scope.state.ticks);
  const subset = $derived(scope.state.subset);
  const busy = $derived(scope.state.busy);
  const regression = $derived(subset?.regression[estimator] ?? null);
  const players = $derived(regression?.players ?? []);

  const view = $derived.by(() => {
    const state = scopeView(scope.state);
    if (state === 'loading') return 'results';
    if (state === 'ready') return regression.has_regression ? 'results' : 'no-regression';
    return state;
  });

  function loadCharts() {
    import('../../lib/effects-charts.js')
      .then((module) => {
        charts = module;
      })
      .catch(() => {
        scope.fail();
      });
  }

  function retry() {
    if (!charts) loadCharts();
    scope.reload();
  }

  onMount(() => {
    estimator = earlyControls.estimator ?? estimator;
    outcome = earlyControls.outcome ?? outcome;
    scope.start();
    loadCharts();
  });
</script>

<div class="effects">
  <section class="card controls">
    <SeasonPicker {seasons} {ticks} onchange={scope.changeTick} />
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
    <ScopeMessage message={emptyStates.no_matches} />
  {:else if view === 'failed'}
    <ScopeMessage onretry={retry} />
  {:else if view === 'no-regression'}
    <ScopeMessage message={emptyStates.no_regression} />
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
