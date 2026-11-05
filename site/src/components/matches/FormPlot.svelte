<script>
  // The Season form plot. It keeps its height whichever metric is showing and before it draws,
  // and redraws when the width changes. The axis label and the key are HTML, so they grow with
  // the text, and a change of metric moves nothing below the plot.
  import { onMount } from 'svelte';

  import { observeWidth } from '../../lib/plot-width.js';
  import { formMetric, formPoints } from '../../lib/season-form.js';

  let { charts, seasonForm, metric, labels, title } = $props();

  let slot = $state.raw(null);
  let width = $state(0);

  const spec = $derived(formMetric(metric, labels));
  const lines = $derived(formPoints(seasonForm, spec));

  onMount(() =>
    observeWidth(slot, (next) => {
      width = next;
    }),
  );

  $effect(() => {
    if (!charts || !slot || width === 0 || seasonForm.length === 0) return;
    slot.replaceChildren(charts.seasonFormChart({ spec, lines, width, height: slot.clientHeight, title }));
    // For timing the first view, as on Effects.
    slot.setAttribute('data-plot-ready', '');
  });
</script>

<p class="axis-label">{spec.yLabel}</p>
<div class="slot" bind:this={slot}></div>
<ul class="key">
  {#each spec.series.filter((line) => line.label) as line (line.column)}
    <li>
      <svg width="22" height="10" aria-hidden="true" focusable="false">
        <line x1="0" y1="5" x2="22" y2="5" stroke={line.colour} stroke-width="2" />
        <circle cx="11" cy="5" r="3.5" fill={line.colour} />
      </svg>
      {line.label}
    </li>
  {/each}
</ul>

<style>
  .axis-label {
    margin-top: 10px;
    color: var(--ink-muted);
    font-size: 0.8125rem;
    line-height: 1.3;
  }

  .slot {
    width: 100%;
    height: 16.25rem;
    margin-top: 4px;
  }

  .slot:empty {
    border-radius: 12px;
    background: var(--bg);
  }

  .slot :global(svg) {
    display: block;
  }

  /* Empty on three metrics out of four, and its height stays reserved there. */
  .key {
    display: flex;
    flex-wrap: wrap;
    justify-content: center;
    gap: 2px 16px;
    min-height: 1.5rem;
    margin-top: 4px;
    padding: 0;
    list-style: none;
    color: var(--ink);
    font-size: 0.8125rem;
    line-height: 1.5rem;
  }

  .key li {
    display: flex;
    align-items: center;
    gap: 6px;
  }

  /* The height Shiny gives its plot. */
  @media (min-width: 768px) {
    .slot {
      height: 23.75rem;
    }
  }
</style>
