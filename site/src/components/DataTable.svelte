<script>
  // A table a page changes in place: sorted from its headings, or with a row to pick. Keyed
  // rows, so a sort or a pick keeps focus where it was. The same classes and cells as
  // tableHtml() in src/lib/data-table.js, which draws a table once. See there for columns.
  import { cellClass, cellHtml, headingHtml, tapPicksRow } from '../lib/data-table.js';
  import { nextSort } from '../lib/table-sort.js';

  let { caption, columns, rows, rowKey, className = '', sort = null, onsort = null, picked = null, onpick = null, rowClass = null, foldLabel = null } = $props();

  // foldLabel(row) names a button beside the lead that opens the row's `folds` columns, for a
  // table whose styles put them under the row. It sits beside the row's other controls rather
  // than around them. Those cells get `is-sorted` while the table is sorted on them, for the
  // styles to keep them showing, so the rows are never in an order the reader cannot see.
  let opened = $state.raw(new Set());

  function toggleFold(key) {
    const next = new Set(opened);
    if (!next.delete(key)) next.add(key);
    opened = next;
  }

  const ariaSort = (column) => (sort?.key === column.key ? sort.direction : undefined);

  // The row is one big target, and the button in it is what a keyboard and a screen reader
  // use. A link in the row goes where it goes, and the button has already answered.
  function pickFromRow(event, row) {
    if (onpick && tapPicksRow(event.target)) onpick(row);
  }
</script>

<div class="dt-fit">
  <!-- The roles look redundant. See tableHtml(). -->
  <!-- svelte-ignore a11y_no_redundant_roles -->
  <table role="table" class={['dt', className]} class:dt-pickable={onpick}>
    <caption class="visually-hidden">{caption}</caption>
    <!-- svelte-ignore a11y_no_redundant_roles -->
    <thead role="rowgroup">
      <!-- svelte-ignore a11y_no_redundant_roles -->
      <tr role="row">
        {#each columns as column (column.key)}
          <th scope="col" role="columnheader" class={cellClass(column)} aria-sort={ariaSort(column)}>
            {#if onsort && !column.unsorted}
              <button type="button" class="dt-sort" onclick={() => onsort(nextSort(sort, column.key))}>
                <svg class="dt-sort-mark" class:is-active={sort?.key === column.key} class:is-up={ariaSort(column) === 'ascending'} viewBox="0 0 10 10" width="10" height="10" aria-hidden="true" focusable="false">
                  <path d="M5 8 1.5 3h7z" />
                </svg>
                <span>{column.label}</span>
              </button>
            {:else}
              {@html headingHtml(column)}
            {/if}
          </th>
        {/each}
      </tr>
    </thead>
    <!-- svelte-ignore a11y_no_redundant_roles -->
    <tbody role="rowgroup">
      {#each rows as row (rowKey(row))}
        {@const isPicked = onpick && rowKey(row) === picked}
        <!-- svelte-ignore a11y_click_events_have_key_events, a11y_no_noninteractive_element_interactions, a11y_no_redundant_roles -->
        <tr role="row" class={['dt-row', rowClass?.(row)]} class:is-picked={isPicked} class:is-open={opened.has(rowKey(row))} onclick={(event) => pickFromRow(event, row)}>
          {#each columns as column (column.key)}
            {#if column.lead}
              <th scope="row" role="rowheader" class={[cellClass(column), 'dt-lead']}>
                {#if foldLabel}
                  <button type="button" class="dt-unfold" aria-expanded={opened.has(rowKey(row))} aria-label={foldLabel(row)} onclick={() => toggleFold(rowKey(row))}>
                    <svg class="dt-chevron" viewBox="0 0 24 24" width="16" height="16" aria-hidden="true" focusable="false"><path d="m9 6 6 6-6 6" /></svg>
                  </button>
                {/if}
                {#if onpick}
                  <!-- Current rather than pressed: a second tap leaves it picked, so it is no toggle. -->
                  <button type="button" class="dt-pick" aria-current={isPicked ? 'true' : undefined} onclick={() => onpick(row)}>{column.value(row)}</button>
                {:else}
                  <span class="dt-name">{@html cellHtml(column, row)}</span>
                {/if}
                {#if column.cue?.(row)}
                  <span class="dt-cue">{column.cue(row)}</span>
                {/if}
              </th>
            {:else}
              <!-- svelte-ignore a11y_no_redundant_roles -->
              <td role="cell" class={[cellClass(column), foldLabel && sort?.key === column.key && 'is-sorted']} data-label={column.label}>{@html cellHtml(column, row)}</td>
            {/if}
          {/each}
        </tr>
      {/each}
    </tbody>
  </table>
</div>
