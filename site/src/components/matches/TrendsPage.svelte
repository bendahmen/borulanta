<script>
  // Matches > Trends: how the season is going, who is scoring and how we do against each side,
  // all for the ticked seasons. Every number comes from R's JSON and shows R's own string.
  import { onMount, untrack } from 'svelte';

  import { moreRowsLabel, tableHtml } from '../../lib/data-table.js';
  import { MATCHES } from '../../lib/labels.js';
  import { FIRST_SCORERS, firstRows, pickMatches } from '../../lib/match-data.js';
  import { opponentColumns, scorerColumns } from '../../lib/match-tables.js';
  import { scopeView } from '../../lib/season-scope.js';
  import { sortOnField } from '../../lib/table-sort.js';
  import { useSeasonScope } from '../../lib/use-season-scope.svelte.js';
  import CardHead from '../CardHead.svelte';
  import DataTable from '../DataTable.svelte';
  import ScopeMessage from '../ScopeMessage.svelte';
  import SeasonPicker from '../SeasonPicker.svelte';
  import Segmented from '../Segmented.svelte';
  import FormPlot from './FormPlot.svelte';

  let { seasons, emptyStates, cards, plotLabels } = $props();

  const scope = useSeasonScope(untrack(() => seasons), pickMatches);
  // The tap on this control can beat hydration too. See readEarlyTicks().
  const earlyMetric = typeof document === 'undefined' ? undefined : document.querySelector('input[name="form-metric"]:checked')?.value;

  let metric = $state('points');
  // On Goals, as scorer_display_table() orders it.
  let scorerSort = $state.raw({ key: 'goals', direction: 'descending' });
  let showAllScorers = $state(false);
  let charts = $state.raw(null);

  const subset = $derived(scope.state.subset);
  const view = $derived(scopeView(scope.state));
  const scorers = $derived(sortOnField(subset?.scorers.rows ?? [], scorerSort));
  const shownScorers = $derived(firstRows(scorers, FIRST_SCORERS, showAllScorers));
  const moreScorers = $derived(moreRowsLabel(scorers.length, FIRST_SCORERS, showAllScorers, MATCHES));
  const scorerCard = $derived({ ...cards.scorers, subtitle: subset?.scorers.note ?? null });
  const opponents = $derived(
    tableHtml({
      className: 'dt-opponents',
      caption: cards.head_to_head.title,
      columns: opponentColumns(cards.head_to_head.headings),
      rows: subset?.opponent_record ?? [],
    }),
  );

  function changeTick(id, checked) {
    showAllScorers = false;
    scope.changeTick(id, checked);
  }

  function loadCharts() {
    import('../../lib/season-form-chart.js')
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
    metric = earlyMetric ?? metric;
    scope.start();
    loadCharts();
  });
</script>

<div class="page-grid">
  <section class="card controls">
    <SeasonPicker {seasons} ticks={scope.state.ticks} onchange={changeTick} />
  </section>

  {#if view === 'no-matches'}
    <ScopeMessage message={emptyStates.no_matches} />
  {:else if view === 'failed'}
    <ScopeMessage onretry={retry} />
  {:else}
    <div class="cards" class:is-busy={scope.state.busy} aria-busy={scope.state.busy || !subset}>
      <section class="card form" aria-labelledby="season-form-title">
        <CardHead id="season-form-title" card={cards.season_form} />
        <div class="metric">
          <Segmented
            legend={cards.season_form.metric_label}
            name="form-metric"
            layout="grid"
            options={cards.season_form.metrics}
            value={metric}
            onchange={(next) => (metric = next)}
          />
        </div>
        <FormPlot {charts} seasonForm={subset?.season_form ?? []} {metric} labels={plotLabels} title={cards.season_form.title} />
      </section>

      <section class="card" aria-labelledby="scorers-title">
        <CardHead id="scorers-title" card={scorerCard} />
        {#if subset && scorers.length === 0}
          <p class="dt-empty">{emptyStates.no_scorers}</p>
        {:else if subset}
          <DataTable
            className="dt-scorers"
            caption={cards.scorers.title}
            columns={scorerColumns(cards.scorers.headings)}
            rows={shownScorers}
            rowKey={(row) => row.player}
            sort={scorerSort}
            onsort={(next) => (scorerSort = next)}
          />
          {#if moreScorers}
            <button type="button" class="dt-button" onclick={() => (showAllScorers = !showAllScorers)}>{moreScorers}</button>
          {/if}
        {/if}
      </section>

      <section class="card" aria-labelledby="head-to-head-title">
        <CardHead id="head-to-head-title" card={cards.head_to_head} />
        {#if subset && subset.opponent_record.length === 0}
          <p class="dt-empty">{emptyStates.no_opponent}</p>
        {:else if subset}
          {@html opponents}
        {/if}
      </section>
    </div>
  {/if}
</div>

<style>
  .page-grid,
  .cards {
    display: grid;
    gap: 10px;
    min-width: 0;
  }

  .cards > * {
    min-width: 0;
  }

  .controls {
    padding-top: 10px;
    padding-bottom: 12px;
  }

  .metric {
    margin-top: 10px;
  }

  .is-busy {
    opacity: 0.55;
  }

  @media (min-width: 768px) {
    .page-grid,
    .cards {
      gap: 16px;
    }

    .cards {
      grid-template-columns: minmax(0, 2fr) minmax(0, 3fr);
      align-items: start;
    }

    .form {
      grid-column: 1 / -1;
    }
  }

  @media (prefers-reduced-motion: no-preference) {
    .cards {
      transition: opacity 0.15s ease;
    }
  }
</style>
