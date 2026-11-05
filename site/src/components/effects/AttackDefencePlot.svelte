<script>
  import { onMount } from 'svelte';

  import { attackDefenceAxes, attackDefenceRows, dotRadius, maxDotRadius, sizeKeyValues } from '../../lib/effects-data.js';
  import { EFFECTS } from '../../lib/labels.js';
  import { observeWidth } from '../../lib/plot-width.js';

  let { charts, players, labels, title, description } = $props();

  let slot = $state.raw(null);
  let width = $state(0);
  let fontsLoaded = $state(0);

  const maxAppearances = $derived(players.length > 0 ? Math.max(...players.map((player) => player.appearances)) : 0);
  const key = $derived(
    width > 0 && maxAppearances > 0
      ? sizeKeyValues(maxAppearances).map((value) => ({
          value,
          r: dotRadius(value, maxAppearances, maxDotRadius(width)),
        }))
      : [],
  );
  const keyHeight = $derived(key.length > 0 ? Math.ceil(key.at(-1).r * 2) + 2 : 0);
  const scaleNote = $derived(attackDefenceAxes(attackDefenceRows(players)).note);

  onMount(() => {
    // Label widths are measured, so measure again once the real fonts are in.
    document.fonts?.ready.then(() => {
      fontsLoaded += 1;
    });
    return observeWidth(slot, (next) => {
      width = next;
    });
  });

  $effect(() => {
    fontsLoaded;
    if (!charts || !slot || width === 0 || players.length === 0) return;
    slot.replaceChildren(charts.attackDefenceChart({ players, width, labels, title, description }));
    // For timing the first view. See the performance targets in the migration plan.
    slot.setAttribute('data-plot-ready', '');
  });
</script>

<div class="slot" bind:this={slot}></div>

<div class="key" class:is-empty={key.length === 0}>
  <span>{EFFECTS.appearances}</span>
  {#each key as item (item.value)}
    <span class="key-item">
      <svg width={item.r * 2 + 2} height={keyHeight} aria-hidden="true" focusable="false">
        <circle cx={item.r + 1} cy={keyHeight / 2} r={item.r} />
      </svg>
      {item.value}
    </span>
  {/each}
</div>

{#if scaleNote}
  <p class="scale-note">{scaleNote}</p>
{/if}

<style>
  /* Reserves the plot's square before it draws, so nothing below it moves. */
  .slot {
    width: 100%;
    max-width: 30rem;
    aspect-ratio: 1;
    margin: 8px auto 0;
  }

  .slot:empty {
    border-radius: 12px;
    background: var(--bg);
  }

  .slot :global(svg) {
    display: block;
  }

  .key {
    display: flex;
    flex-wrap: wrap;
    align-items: center;
    justify-content: center;
    gap: 2px 12px;
    min-height: 32px;
    margin-top: 2px;
    color: var(--ink-muted);
    font-size: 0.8125rem;
    line-height: 1.2;
  }

  .key.is-empty {
    visibility: hidden;
  }

  .key-item {
    display: inline-flex;
    align-items: center;
    gap: 4px;
    font-variant-numeric: tabular-nums;
  }

  .scale-note {
    margin-top: 6px;
    color: var(--ink-muted);
    font-size: 0.875rem;
    line-height: 1.35;
    text-align: center;
  }

  circle {
    fill: var(--blue);
    fill-opacity: 0.8;
  }
</style>
