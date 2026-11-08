<script>
  // The home page's player table: goals, turnout and the goal-difference coefficient, in the
  // HTML already sorted on goals. Sorting from the headings needs the script; the rows do not.
  import { escapeHtml } from '../lib/data-table.js';
  import { HOME } from '../lib/labels.js';
  import { sortOnField } from '../lib/table-sort.js';
  import { formatTurnout } from '../lib/turnout.js';
  import CardHead from './CardHead.svelte';
  import DataTable from './DataTable.svelte';

  let { rows, subtitle } = $props();

  const words = HOME.stats;
  const columns = [
    {
      key: 'player',
      label: words.headings.player,
      lead: true,
      value: (row) => row.player,
      // A guest who only scored has no page of their own.
      html: (row) => (row.href ? `<a href="${escapeHtml(row.href)}">${escapeHtml(row.player)}</a>` : escapeHtml(row.player)),
    },
    { key: 'goals', label: words.headings.goals, value: (row) => row.goals, number: true },
    { key: 'attendance_rate', label: words.headings.turnout, value: (row) => formatTurnout(row.attendance_rate), number: true },
    {
      key: 'gd_effect',
      label: words.headings.gd_effect,
      value: (row) => row.gd_display?.estimate ?? '',
      number: true,
      html: (row) =>
        row.gd_display
          ? `${escapeHtml(row.gd_display.estimate)}${row.gd_display.p ? `<span class="dt-p">${escapeHtml(words.pValue(row.gd_display.p))}</span>` : ''}`
          : '',
    },
  ];

  // The rows arrive on goals, then turnout, then the name, and a stable sort on goals keeps that.
  let sort = $state.raw({ key: 'goals', direction: 'descending' });
  const shown = $derived(sortOnField(rows, sort));
</script>

<section class="card stats" aria-labelledby="home-stats-title">
  <CardHead id="home-stats-title" card={{ tag: words.tag, title: words.title, subtitle }} />
  <DataTable className="dt-stats" caption={words.caption} {columns} rows={shown} rowKey={(row) => row.player} {sort} onsort={(next) => (sort = next)} />
  <a class="more" href="/players/effects/">{words.effectsLink}</a>
</section>

<style>
  .more {
    display: inline-block;
    margin-top: 10px;
    color: var(--blue);
    font-size: 0.9375rem;
    text-underline-offset: 2px;
  }

  .stats :global(.dt-stats .col-player) {
    width: 100%;
    overflow-wrap: anywhere;
  }

  .stats :global(.dt-stats td.col-gd_effect) {
    min-width: 4.5rem;
  }

  .stats :global(.dt-p) {
    display: block;
    margin-top: 2px;
    color: var(--ink-muted);
    font-size: 0.75rem;
  }
</style>
