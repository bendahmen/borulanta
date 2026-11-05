<script>
  // One player over the ticked seasons, as player_page has it: the headline tiles and the
  // appearance strip, then the side with and without them. On Roster and on the player's own
  // page. Every string is R's, and with `profile` null the cards wait with their headings.
  import { PLAYERS } from '../../lib/labels.js';
  import { withWithoutRows } from '../../lib/player-data.js';
  import { withWithoutColumns } from '../../lib/player-tables.js';
  import CardHead from '../CardHead.svelte';
  import DataTable from '../DataTable.svelte';
  import AppearanceStrip from './AppearanceStrip.svelte';

  let { profile, matches, cards, note = null } = $props();

  // The note is R's wording to go with a table above, so a page with no table has no card head.
  const playerCard = $derived(note === null ? null : { ...cards.player, subtitle: note });
  const withWithoutCard = $derived({ ...cards.with_without, subtitle: profile?.differential_note ?? null });
</script>

<div class="profile">
  <section class="card" aria-labelledby={playerCard ? 'player-card-title' : undefined} aria-label={playerCard ? undefined : cards.player.tag}>
    {#if playerCard}
      <!-- Read out when a pick changes it, without moving anyone from the row they are on. -->
      <div aria-live="polite">
        <CardHead id="player-card-title" card={playerCard} />
      </div>
    {/if}
    {#if profile}
      <dl class="tiles">
        {#each profile.headline as tile (tile.label)}
          <div class="tile">
            <dt>{tile.label}</dt>
            <dd class="value">{tile.value}</dd>
            {#if tile.note}
              <dd class="note">{tile.note}</dd>
            {/if}
          </div>
        {/each}
      </dl>
      <!-- A new player starts with no square tapped and the dates closed. The heading follows
           the card's own: with no card head on the player's page, it is the card's first. -->
      {#key profile.player}
        <AppearanceStrip {profile} {matches} title={cards.player.appearances_title} keyWords={cards.player.strip_key} headingLevel={playerCard ? 'h3' : 'h2'} />
      {/key}
    {/if}
  </section>

  <section class="card" aria-labelledby="with-without-title">
    <CardHead id="with-without-title" card={withWithoutCard} />
    {#if profile}
      <DataTable
        className="dt-with-without"
        caption={cards.with_without.title}
        columns={withWithoutColumns(cards.with_without.headings, PLAYERS)}
        rows={withWithoutRows(profile, cards.with_without.sides)}
        rowKey={(row) => row.side}
      />
    {/if}
  </section>
</div>

<style>
  .profile {
    display: grid;
    gap: 10px;
    min-width: 0;
  }

  .profile > * {
    min-width: 0;
  }

  /* Two fit across a phone. In rem, so enlarged text stacks them instead. */
  .tiles {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(min(8.5rem, 100%), 1fr));
    gap: 8px;
    margin-top: 10px;
  }

  .tiles:first-child {
    margin-top: 0;
  }

  .tile {
    min-width: 0;
    padding: 8px 10px;
    border: 1px solid var(--border);
    border-radius: 12px;
    background: var(--bg);
  }

  dt {
    color: var(--ink-muted);
    font-size: 0.68rem;
    font-weight: 600;
    letter-spacing: 0.08em;
    line-height: 1.2;
    text-transform: uppercase;
  }

  .value {
    margin-top: 2px;
    font-family: var(--font-display);
    font-size: 2rem;
    font-weight: 700;
    font-variant-numeric: tabular-nums;
    line-height: 1.1;
  }

  .note {
    margin-top: 2px;
    color: var(--ink-muted);
    font-size: 0.875rem;
    line-height: 1.3;
  }

  /* Both under the table, which is where R's "pick another row above" needs them. */
  @media (min-width: 768px) {
    .profile {
      grid-template-columns: repeat(2, minmax(0, 1fr));
      align-items: start;
      gap: 16px;
    }
  }
</style>
