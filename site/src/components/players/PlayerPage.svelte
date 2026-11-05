<script>
  // One player on a page of their own, for sharing: the same profile as on Roster, for whichever
  // seasons are ticked. The name is the page's heading, in the static part above this.
  import { onMount, untrack } from 'svelte';

  import { PLAYERS } from '../../lib/labels.js';
  import { pickPlayers, playerPageView } from '../../lib/player-data.js';
  import { useSeasonScope } from '../../lib/use-season-scope.svelte.js';
  import ScopeMessage from '../ScopeMessage.svelte';
  import SeasonPicker from '../SeasonPicker.svelte';
  import PlayerProfile from './PlayerProfile.svelte';

  let { seasons, player, noMatches, cards } = $props();

  const scope = useSeasonScope(untrack(() => seasons), pickPlayers);

  const page = $derived(playerPageView(scope.state, player));

  onMount(() => {
    scope.start();
  });
</script>

<div class="page-grid">
  <section class="card controls">
    <SeasonPicker {seasons} ticks={scope.state.ticks} onchange={scope.changeTick} />
  </section>

  {#if page.view === 'no-matches'}
    <ScopeMessage message={noMatches} />
  {:else if page.view === 'failed'}
    <ScopeMessage onretry={scope.reload} />
  {:else if page.view === 'did-not-play'}
    <ScopeMessage message={PLAYERS.didNotPlay(player)} />
  {:else}
    <div class:is-busy={scope.state.busy} class="profile-block" aria-busy={scope.state.busy || !scope.state.subset}>
      <PlayerProfile profile={page.profile} matches={scope.state.subset?.matches ?? []} {cards} />
    </div>
  {/if}
</div>

<style>
  .page-grid {
    display: grid;
    gap: 10px;
    min-width: 0;
  }

  .page-grid > * {
    min-width: 0;
  }

  .controls {
    padding-top: 10px;
    padding-bottom: 12px;
  }

  .is-busy {
    opacity: 0.55;
  }

  @media (min-width: 768px) {
    .page-grid {
      gap: 16px;
    }
  }

  @media (prefers-reduced-motion: no-preference) {
    .profile-block {
      transition: opacity 0.15s ease;
    }
  }
</style>
