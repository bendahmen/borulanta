<script>
  // Matches > Results: every result, and the one you pick out of it. The detail stays above the
  // table, because R's wording says so. No Plot on this page.
  import { onMount, tick, untrack } from 'svelte';

  import { moreRowsLabel, tableHtml } from '../../lib/data-table.js';
  import { MATCHES } from '../../lib/labels.js';
  import { FIRST_RESULTS, firstRows, matchDetail, matchHref, pickMatches, revealElement, selectedMatchDate } from '../../lib/match-data.js';
  import { everyResultColumns, lineupColumns } from '../../lib/match-tables.js';
  import { scopeView } from '../../lib/season-scope.js';
  import { useSeasonScope } from '../../lib/use-season-scope.svelte.js';
  import CardHead from '../CardHead.svelte';
  import DataTable from '../DataTable.svelte';
  import ScopeMessage from '../ScopeMessage.svelte';
  import SeasonPicker from '../SeasonPicker.svelte';

  let { seasons, noMatches, cards } = $props();

  const scope = useSeasonScope(untrack(() => seasons), pickMatches);

  let picked = $state(null);
  let showingAll = $state(false);
  let detailCard = $state.raw(null);

  const subset = $derived(scope.state.subset);
  const view = $derived(scopeView(scope.state));
  const matches = $derived(subset?.matches ?? []);
  const selectedDate = $derived(selectedMatchDate(matches, picked));
  const detail = $derived(matchDetail(subset, selectedDate));
  const shown = $derived(firstRows(matches, FIRST_RESULTS, showingAll));
  const more = $derived(moreRowsLabel(matches.length, FIRST_RESULTS, showingAll, MATCHES));
  const lineup = $derived(
    tableHtml({
      className: 'dt-lineup dt-tight',
      caption: cards.match_detail.lineup_title,
      columns: lineupColumns(cards.match_detail.headings),
      rows: detail?.lineup ?? [],
    }),
  );

  // A change of ticks falls back to the newest match, as a new table does in Shiny.
  function changeTick(id, checked) {
    picked = null;
    showingAll = false;
    scope.changeTick(id, checked);
  }

  async function pick(row) {
    picked = row.date;
    await tick();
    revealElement(detailCard, window);
  }

  onMount(() => {
    scope.start();
  });
</script>

<div class="page-grid">
  <section class="card controls">
    <SeasonPicker {seasons} ticks={scope.state.ticks} onchange={changeTick} />
  </section>

  {#if view === 'no-matches'}
    <ScopeMessage message={noMatches} />
  {:else if view === 'failed'}
    <ScopeMessage onretry={scope.reload} />
  {:else}
    <div class="cards" class:is-busy={scope.state.busy} aria-busy={scope.state.busy || !subset}>
      <section class="card detail" aria-labelledby="match-detail-title" bind:this={detailCard}>
        <CardHead id="match-detail-title" card={cards.match_detail} />
        {#if detail}
          <div class="detail-body">
            <div>
              <!-- Read out when a pick changes it, without moving anyone from the row they are on. -->
              <div class="tile" aria-live="polite">
                <p class="tile-label">{cards.match_detail.result_label}</p>
                <p class="tile-value">{detail.match.result}</p>
                <p class="tile-note">{detail.summaryNote}</p>
              </div>
              <a class="open" href={matchHref(detail.match.date)}>{MATCHES.openMatchPage}</a>
            </div>
            <div class="lineup">
              <h3>{cards.match_detail.lineup_title}</h3>
              {#if detail.lineup.length > 0}
                {@html lineup}
              {:else}
                <p class="dt-empty">{MATCHES.noLineup}</p>
              {/if}
            </div>
          </div>
        {/if}
      </section>

      <section class="card" aria-labelledby="every-result-title">
        <CardHead id="every-result-title" card={cards.every_result} />
        {#if subset}
          <DataTable
            className="dt-results"
            caption={cards.every_result.title}
            columns={everyResultColumns(cards.every_result, MATCHES)}
            rows={shown}
            rowKey={(row) => row.date}
            picked={selectedDate}
            onpick={pick}
          />
          {#if more}
            <button type="button" class="dt-button" onclick={() => (showingAll = !showingAll)}>{more}</button>
          {/if}
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

  /* Clear of the top of the screen when it is brought into view. */
  .detail {
    scroll-margin-top: 8px;
  }

  .tile {
    margin-top: 10px;
    padding: 8px 10px;
    border: 1px solid var(--border);
    border-radius: 12px;
    background: var(--bg);
  }

  .tile-label {
    color: var(--ink-muted);
    font-size: 0.68rem;
    font-weight: 600;
    letter-spacing: 0.08em;
    line-height: 1.2;
    text-transform: uppercase;
  }

  .tile-value {
    margin-top: 2px;
    font-family: var(--font-display);
    font-size: 2rem;
    font-weight: 700;
    font-variant-numeric: tabular-nums;
    line-height: 1.1;
  }

  .tile-note {
    margin-top: 2px;
    color: var(--ink-muted);
    font-size: 0.9375rem;
    line-height: 1.3;
  }

  .open {
    display: inline-flex;
    align-items: center;
    min-height: 44px;
    font-weight: 600;
  }

  h3 {
    margin-top: 2px;
    font-size: 0.9375rem;
    font-weight: 600;
  }

  .is-busy {
    opacity: 0.55;
  }

  /* min-width lets the table inside measure the column rather than its own content. */
  .lineup {
    min-width: 0;
  }

  @media (min-width: 768px) {
    .page-grid,
    .cards {
      gap: 16px;
    }

    .detail-body {
      display: grid;
      grid-template-columns: minmax(0, 2fr) minmax(0, 3fr);
      align-items: start;
      gap: 16px;
    }

    h3 {
      margin-top: 10px;
    }
  }

  @media (prefers-reduced-motion: no-preference) {
    .cards {
      transition: opacity 0.15s ease;
    }
  }
</style>
