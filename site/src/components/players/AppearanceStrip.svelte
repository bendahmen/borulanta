<script>
  // Which matches a player played, as appearance_timeline_plot() draws them. A filled square
  // for played and a hollow one for missed, so colour is not what says which. Plain HTML rather
  // than Plot, which costs 93 KB and would be the only chart on the page.
  //
  // The squares are one picture to a screen reader and no Tab stops to a keyboard. The dates are
  // in words in the list under them, for everyone. A `title` shows on no phone, so a tap on a
  // square names it in the line below as well.
  import { PLAYERS } from '../../lib/labels.js';
  import { stripModel } from '../../lib/player-data.js';

  let { profile, matches, title, keyWords, headingLevel = 'h3' } = $props();

  const strip = $derived(stripModel(profile.timeline, matches, keyWords));

  let tappedDate = $state(null);
  const tapped = $derived(strip.squares.find((square) => square.date === tappedDate) ?? null);

  // Joined up so that no date breaks across two lines.
  const inWords = (dates) => dates.map((date) => date.replaceAll(' ', '\u00a0')).join(', ');

  function tapSquare(event) {
    tappedDate = event.target.closest('[data-date]')?.dataset.date ?? null;
  }
</script>

<svelte:element this={headingLevel} class="title">{title}</svelte:element>
<!-- svelte-ignore a11y_click_events_have_key_events, a11y_no_noninteractive_element_interactions -->
<div class="squares" role="img" aria-label={PLAYERS.stripSummary(profile.appearances, profile.matches)} onclick={tapSquare}>
  {#each strip.squares as square (square.date)}
    <span class={['square', square.played ? 'is-played' : 'is-missed']} class:is-tapped={square.date === tappedDate} data-date={square.date} title={square.label}></span>
  {/each}
</div>
<div class="below">
  <p class="key">
    <span class="key-item"><span class="square is-played"></span>{keyWords.played}</span>
    <span class="key-item"><span class="square is-missed"></span>{keyWords.missed}</span>
  </p>
  {#if strip.first}
    <p class="span">{tapped ? tapped.label : PLAYERS.stripSpan(strip.first, strip.last)}</p>
  {/if}
</div>
<details class="dates">
  <summary>
    <svg viewBox="0 0 24 24" width="16" height="16" aria-hidden="true" focusable="false"><path d="m9 6 6 6-6 6" /></svg>
    {PLAYERS.stripDates}
  </summary>
  <dl>
    {#if strip.playedDates.length > 0}
      <dt>{keyWords.played}</dt>
      <dd>{inWords(strip.playedDates)}</dd>
    {/if}
    {#if strip.missedDates.length > 0}
      <dt>{keyWords.missed}</dt>
      <dd>{inWords(strip.missedDates)}</dd>
    {/if}
  </dl>
</details>

<style>
  .title {
    margin-top: 12px;
    font-size: 0.9375rem;
    font-weight: 600;
  }

  .squares {
    display: grid;
    grid-template-columns: repeat(auto-fill, 0.875rem);
    gap: 3px;
    margin-top: 6px;
    cursor: pointer;
  }

  .square {
    display: block;
    box-sizing: border-box;
    width: 0.875rem;
    height: 0.875rem;
    border: 1.5px solid var(--blue);
    border-radius: 3px;
  }

  .is-played {
    background: var(--blue);
  }

  .is-missed {
    border-color: var(--ink-muted);
  }

  .is-tapped {
    outline: 2px solid var(--ink);
    outline-offset: 1px;
  }

  /* Forced colours would take the fill away, and with it the difference between the two. */
  @media (forced-colors: active) {
    .is-played {
      background: CanvasText;
      forced-color-adjust: none;
    }
  }

  .below {
    display: flex;
    flex-wrap: wrap;
    justify-content: space-between;
    gap: 2px 16px;
    margin-top: 6px;
    font-size: 0.875rem;
  }

  .key {
    display: flex;
    flex-wrap: wrap;
    gap: 2px 12px;
  }

  .key-item {
    display: inline-flex;
    align-items: center;
    gap: 5px;
  }

  .span {
    color: var(--ink-muted);
  }

  summary {
    display: inline-flex;
    align-items: center;
    gap: 2px;
    min-height: 44px;
    margin-bottom: -10px;
    color: var(--blue);
    font-size: 0.875rem;
    font-weight: 600;
    list-style: none;
    cursor: pointer;
  }

  summary::-webkit-details-marker {
    display: none;
  }

  summary svg {
    flex: 0 0 auto;
    margin-left: -3px;
    fill: none;
    stroke: currentColor;
    stroke-width: 2;
    stroke-linecap: round;
    stroke-linejoin: round;
  }

  details[open] summary {
    margin-bottom: 0;
  }

  details[open] summary svg {
    transform: rotate(90deg);
  }

  dl {
    font-size: 0.875rem;
    line-height: 1.35;
  }

  dt {
    color: var(--ink-muted);
    font-weight: 600;
  }

  dd + dt {
    margin-top: 6px;
  }

  @media (prefers-reduced-motion: no-preference) {
    summary svg {
      transition: transform 0.15s ease;
    }
  }
</style>
