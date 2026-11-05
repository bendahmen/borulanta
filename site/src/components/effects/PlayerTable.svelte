<script>
  import { onMount } from 'svelte';

  import { EFFECTS } from '../../lib/labels.js';
  import { nextSort, sortOnShownColumn, sortRows } from '../../lib/table-sort.js';

  let { players, outcome, outcomeLabels, caption, sort, onsort, control } = $props();

  const outcomes = $derived(Object.keys(outcomeLabels));
  const rows = $derived(sortRows(players, sort));

  let expanded = $state.raw(new Set());
  let fit = $state.raw(null);

  function toggle(player) {
    const next = new Set(expanded);
    if (!next.delete(player)) next.add(player);
    expanded = next;
  }

  const ariaSort = (key) => (sort.key === key ? sort.direction : undefined);

  // The container queries below decide which columns show, so ask the page rather than
  // repeat their widths here.
  function isShown(key) {
    const heading = fit?.querySelector(`thead th[data-sort-key="${key}"]`);
    return !heading || getComputedStyle(heading).display !== 'none';
  }

  function settleSort() {
    const next = sortOnShownColumn(sort, isShown, outcome);
    if (next !== sort) onsort(next);
  }

  // Runs when the outcome changes, and after each change of sort.
  $effect(settleSort);

  // And when a turn of the phone or a change of text size hides a column. The frame's wait
  // is what keeps a sort made in here from tripping the observer's own loop check.
  onMount(() => {
    let frame = 0;
    const observer = new ResizeObserver(() => {
      cancelAnimationFrame(frame);
      frame = requestAnimationFrame(settleSort);
    });
    observer.observe(fit);
    return () => {
      cancelAnimationFrame(frame);
      observer.disconnect();
    };
  });
</script>

{#snippet heading(key, label, className)}
  <th scope="col" role="columnheader" class={className} data-sort-key={key} aria-sort={ariaSort(key)}>
    <button type="button" onclick={() => onsort(nextSort(sort, key))}>
      <span>{label}</span>
      <svg class="sort-mark" class:is-active={sort.key === key} class:is-up={sort.key === key && sort.direction === 'ascending'} viewBox="0 0 10 10" width="10" height="10" aria-hidden="true" focusable="false">
        <path d="M5 8 1.5 3h7z" />
      </svg>
    </button>
  </th>
{/snippet}

<div class="fit" bind:this={fit}>
  <div class="one-outcome">{@render control()}</div>

  <!-- The roles look redundant. They keep this a table to a screen reader when large text
       turns its parts into blocks and flex rows, which can otherwise drop the semantics. -->
  <!-- svelte-ignore a11y_no_redundant_roles -->
  <table role="table">
    <caption class="visually-hidden">{caption}</caption>
    <!-- svelte-ignore a11y_no_redundant_roles -->
    <thead role="rowgroup">
      <!-- svelte-ignore a11y_no_redundant_roles -->
      <tr role="row">
        {@render heading('player', EFFECTS.player, 'col-player')}
        {@render heading('appearances', EFFECTS.appearances, 'col-number')}
        {#each outcomes as key (key)}
          {@render heading(key, outcomeLabels[key], `col-number col-outcome${key === outcome ? ' is-selected' : ''}`)}
        {/each}
      </tr>
    </thead>
    <!-- svelte-ignore a11y_no_redundant_roles -->
    <tbody role="rowgroup">
      {#each rows as row (row.player)}
        {@const open = expanded.has(row.player)}
        <!-- svelte-ignore a11y_no_redundant_roles -->
        <tr class="row" role="row">
          <th scope="row" role="rowheader" class="col-player">
            <button
              type="button"
              class="row-toggle"
              aria-expanded={open}
              aria-label={EFFECTS.moreFor(row.player)}
              onclick={() => toggle(row.player)}
            >
              <svg class:is-open={open} viewBox="0 0 24 24" width="16" height="16" aria-hidden="true" focusable="false">
                <path d="m9 6 6 6-6 6" />
              </svg>
              <span>{row.player}</span>
            </button>
            <span class="row-name">{row.player}</span>
          </th>
          <!-- svelte-ignore a11y_no_redundant_roles -->
          <td class="col-number col-appearances" role="cell">{row.appearances}</td>
          {#each outcomes as key (key)}
            <!-- svelte-ignore a11y_no_redundant_roles -->
            <td class="col-number col-outcome" class:is-selected={key === outcome} role="cell">{row.outcomes[key].display}</td>
          {/each}
        </tr>
        {#if open}
          <!-- svelte-ignore a11y_no_redundant_roles -->
          <tr class="detail" role="row">
            <!-- svelte-ignore a11y_no_redundant_roles -->
            <td colspan={2 + outcomes.length} role="cell">
              <dl>
                <div class="detail-appearances">
                  <dt>{EFFECTS.appearances}</dt>
                  <dd>{row.appearances}</dd>
                </div>
                {#each outcomes.filter((key) => key !== outcome) as key (key)}
                  <div>
                    <dt>{outcomeLabels[key]}</dt>
                    <dd>{row.outcomes[key].display}</dd>
                  </div>
                {/each}
              </dl>
            </td>
          </tr>
        {/if}
      {/each}
    </tbody>
  </table>
</div>

<style>
  /* Container queries in rem rather than media queries, because a media query cannot see
     enlarged text. At normal text size the widest of these falls where 768 px of screen
     would and the narrowest below a 320 px screen, so only enlarged text changes the layout. */
  .fit {
    container-type: inline-size;
  }

  table {
    width: 100%;
    margin-top: 8px;
    border-collapse: collapse;
    font-size: 0.9375rem;
    line-height: 1.25;
  }

  thead th {
    padding: 0;
    border-bottom: 1px solid var(--border);
    vertical-align: bottom;
    text-align: left;
    font-weight: 600;
  }

  thead button {
    display: inline-flex;
    align-items: center;
    gap: 4px;
    width: 100%;
    min-height: 44px;
    padding: 4px 0;
    border: 0;
    background: none;
    color: var(--ink-muted);
    font: inherit;
    font-size: 0.8125rem;
    line-height: 1.15;
    text-align: left;
    cursor: pointer;
  }

  thead th[aria-sort='ascending'] button,
  thead th[aria-sort='descending'] button {
    color: var(--blue);
  }

  /* The mark goes on the far side, so the words line up with the numbers below them. */
  thead .col-number button {
    flex-direction: row-reverse;
    justify-content: flex-start;
    text-align: right;
  }

  .sort-mark {
    flex: 0 0 auto;
    visibility: hidden;
    fill: currentColor;
  }

  .sort-mark.is-active {
    visibility: visible;
  }

  .sort-mark.is-up {
    transform: rotate(180deg);
  }

  tbody tr.row > * {
    border-bottom: 1px solid var(--border);
  }

  tbody th {
    padding: 0;
    text-align: left;
    font-weight: 600;
  }

  td {
    padding: 8px 0 8px 10px;
  }

  .col-number {
    text-align: right;
    font-variant-numeric: tabular-nums;
  }

  /* Only the values. A heading wraps at its space if it must, as Goals conceded does on a
     small phone, rather than pushing the table past the edge of its card. */
  td.col-number {
    white-space: nowrap;
  }

  /* One outcome has a column and the others open under the row. */
  .col-outcome {
    display: none;
  }

  .col-outcome.is-selected {
    display: table-cell;
  }

  .row-name {
    display: none;
  }

  .row-toggle {
    display: flex;
    align-items: center;
    gap: 4px;
    width: 100%;
    min-height: 44px;
    padding: 0;
    border: 0;
    background: none;
    color: inherit;
    font: inherit;
    text-align: left;
    cursor: pointer;
  }

  .row-toggle svg {
    flex: 0 0 auto;
    margin-left: -3px;
    fill: none;
    stroke: var(--ink-muted);
    stroke-width: 2;
    stroke-linecap: round;
    stroke-linejoin: round;
  }

  .row-toggle svg.is-open {
    transform: rotate(90deg);
  }

  .detail td {
    padding: 0 0 8px 17px;
    border-bottom: 1px solid var(--border);
    text-align: left;
  }

  dl {
    margin: 0;
    display: grid;
    gap: 3px;
    font-size: 0.875rem;
  }

  dl div {
    display: flex;
    justify-content: space-between;
    gap: 12px;
  }

  /* It has a column of its own unless the text is large. */
  dl .detail-appearances {
    display: none;
  }

  dt {
    color: var(--ink-muted);
  }

  dd {
    white-space: nowrap;
    font-variant-numeric: tabular-nums;
  }

  /* Everything fits in columns from here. */
  @container (min-width: 43.875rem) {
    .one-outcome {
      display: none;
    }

    .col-outcome {
      display: table-cell;
    }

    .row-toggle,
    .detail {
      display: none;
    }

    .row-name {
      display: inline;
    }

    tbody th {
      padding: 9px 0;
    }

    td {
      padding: 9px 0 9px 14px;
    }

    thead .col-number button {
      padding-left: 14px;
    }
  }

  /* The last few pixels for three columns, on the smallest phones. */
  @container (max-width: 16.5rem) {
    td {
      padding-left: 6px;
    }
  }

  /* Large text. Three columns no longer fit, so Appearances moves under the row and keeps
     only its sort button up here. Each row wraps rather than scrolling sideways: a value
     takes the next line if it will not fit beside the name, and breaks only at its space,
     so a name or a number is never split. The width is what three columns need at their
     widest: the words scale with the text, and the marks and padding do not. */
  @container (max-width: calc(13.625rem + 35px)) {
    table,
    thead,
    tbody,
    tr.detail,
    tr.detail td {
      display: block;
    }

    thead tr,
    tr.row {
      display: flex;
      flex-wrap: wrap;
      justify-content: space-between;
      column-gap: 10px;
    }

    thead tr,
    tbody tr.row {
      border-bottom: 1px solid var(--border);
    }

    thead th,
    tbody tr.row > * {
      border-bottom: 0;
    }

    thead button {
      width: auto;
    }

    thead th.col-outcome,
    td.col-outcome {
      margin-left: auto;
    }

    td.col-number {
      white-space: normal;
    }

    /* Hidden from sight only. Removed outright, it leaves the estimate under the
       Appearances heading for anyone using a screen reader. */
    td.col-appearances {
      position: absolute;
      width: 1px;
      height: 1px;
      padding: 0;
      overflow: hidden;
      clip-path: inset(50%);
      white-space: nowrap;
    }

    td.col-outcome {
      align-self: center;
      padding: 0 0 8px;
    }

    dl .detail-appearances {
      display: flex;
    }

    dl div {
      flex-wrap: wrap;
    }

    dd {
      margin-left: auto;
      white-space: normal;
      text-align: right;
    }
  }

  @media (prefers-reduced-motion: no-preference) {
    .row-toggle svg {
      transition: transform 0.15s ease;
    }
  }
</style>
