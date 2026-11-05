<script>
  import { onMount } from 'svelte';

  import { coefficientHeight, coefficientRows, TYPICAL_PLAYER_COUNT } from '../../lib/effects-data.js';
  import { observeWidth } from '../../lib/plot-width.js';

  let { charts, players, outcome, axisLabel, title } = $props();

  let slot = $state.raw(null);
  let width = $state(0);
  let fontsLoaded = $state(0);

  const rows = $derived(coefficientRows(players, outcome));
  const height = $derived(coefficientHeight(rows.length > 0 ? rows.length : TYPICAL_PLAYER_COUNT));

  onMount(() => {
    document.fonts?.ready.then(() => {
      fontsLoaded += 1;
    });
    return observeWidth(slot, (next) => {
      width = next;
    });
  });

  $effect(() => {
    fontsLoaded;
    if (!charts || !slot || width === 0 || rows.length === 0) return;
    slot.replaceChildren(charts.coefficientChart({ rows, width, title, description: axisLabel }));
  });
</script>

{#if axisLabel}
  <p class="axis-label">{axisLabel}</p>
{/if}
<div class="slot" bind:this={slot} style:height="{height}px"></div>

<style>
  /* HTML rather than SVG, because the longest of these is wider than a phone's plot. */
  .axis-label {
    margin-top: 10px;
    color: var(--ink-muted);
    font-size: 0.8125rem;
    line-height: 1.3;
  }

  .slot {
    width: 100%;
    margin-top: 4px;
  }

  .slot:empty {
    border-radius: 12px;
    background: var(--bg);
  }

  .slot :global(svg) {
    display: block;
  }
</style>
