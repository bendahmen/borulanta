<script>
  // Players > Roster: the table is the picker and the profile sits under it, as the README
  // describes. No Plot on this page.
  import { onMount, tick, untrack } from 'svelte';

  import { moreRowsLabel } from '../../lib/data-table.js';
  import { MATCHES, PLAYERS } from '../../lib/labels.js';
  import { FIRST_ROSTER, ROSTER_SORT, createRosterSelection, pickPlayers, profileNeedsReveal, rosterView } from '../../lib/player-data.js';
  import { rosterColumns } from '../../lib/player-tables.js';
  import { revealElement } from '../../lib/match-data.js';
  import { scopeView } from '../../lib/season-scope.js';
  import { useSeasonScope } from '../../lib/use-season-scope.svelte.js';
  import CardHead from '../CardHead.svelte';
  import DataTable from '../DataTable.svelte';
  import ScopeMessage from '../ScopeMessage.svelte';
  import SeasonPicker from '../SeasonPicker.svelte';
  import PlayerProfile from './PlayerProfile.svelte';

  let { seasons, noMatches, cards } = $props();

  const scope = useSeasonScope(untrack(() => seasons), pickPlayers);

  const chooser = createRosterSelection({
    getStorage: () => localStorage,
    onchange: (next) => {
      selection = next;
    },
  });
  let selection = $state.raw(chooser.state);
  let sort = $state.raw(ROSTER_SORT);
  let showingAll = $state(false);
  let profileBlock = $state.raw(null);

  const subset = $derived(scope.state.subset);
  const view = $derived(scopeView(scope.state));
  const table = $derived(rosterView(subset, selection, sort, showingAll));
  const columns = $derived(rosterColumns(cards.who_turns_up.headings, PLAYERS, table.top));
  const more = $derived(moreRowsLabel(table.roster.length, FIRST_ROSTER, showingAll, MATCHES));

  function changeTick(id, checked) {
    showingAll = false;
    scope.changeTick(id, checked);
  }

  async function pick(row) {
    chooser.pick(row.player);
    await tick();
    if (profileBlock) revealElement(profileBlock, window, profileNeedsReveal);
  }

  onMount(() => {
    chooser.open();
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
      <section class="card" aria-labelledby="who-turns-up-title">
        <CardHead id="who-turns-up-title" card={cards.who_turns_up} />
        {#if subset && table.roster.length === 0}
          <p class="dt-empty">{PLAYERS.noRoster}</p>
        {:else if subset}
          <DataTable
            className="dt-roster"
            caption={cards.who_turns_up.title}
            {columns}
            rows={table.rows}
            rowKey={(row) => row.player}
            {sort}
            onsort={(next) => (sort = next)}
            picked={table.selected}
            onpick={pick}
            rowClass={(row) => (table.top.has(row.player) ? 'rank-top3' : '')}
            foldLabel={(row) => PLAYERS.foldFor(row.player)}
          />
          {#if more}
            <button type="button" class="dt-button" onclick={() => (showingAll = !showingAll)}>{more}</button>
          {/if}
        {/if}
      </section>

      {#if !subset || table.profile}
        <div class="profile-block" bind:this={profileBlock}>
          <PlayerProfile
            profile={table.profile}
            matches={subset?.matches ?? []}
            {cards}
            note={table.selected ? `${table.selected}${cards.player.note_after_name}` : ''}
          />
        </div>
      {/if}
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
  .profile-block {
    scroll-margin-top: 8px;
  }

  .is-busy {
    opacity: 0.55;
  }

  @media (min-width: 768px) {
    .page-grid,
    .cards {
      gap: 16px;
    }
  }

  @media (prefers-reduced-motion: no-preference) {
    .cards {
      transition: opacity 0.15s ease;
    }
  }
</style>
