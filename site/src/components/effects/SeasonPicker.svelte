<script>
  import { EFFECTS } from '../../lib/labels.js';

  let { seasons, ticks, onchange } = $props();
</script>

<fieldset>
  <legend class="tag">{EFFECTS.seasons}</legend>
  <div class="seasons">
    {#each seasons as season (season.season_id)}
      <label>
        <!-- data-season-tick lets the inline script set these before first paint. -->
        <input
          class="visually-hidden"
          type="checkbox"
          autocomplete="off"
          data-season-tick
          value={season.season_id}
          checked={ticks.includes(season.season_id)}
          onchange={(event) => onchange(season.season_id, event.currentTarget.checked)}
        />
        <span class="chip">
          <span class="name">
            <svg viewBox="0 0 16 16" width="16" height="16" aria-hidden="true" focusable="false">
              <rect class="box" x="1" y="1" width="14" height="14" rx="4" />
              <path class="check" d="m4.4 8.2 2.4 2.4 4.8-5" />
            </svg>
            {season.label}
          </span>
          <span class="span">{season.span}</span>
        </span>
      </label>
    {/each}
  </div>
</fieldset>

<style>
  fieldset {
    min-width: 0;
    margin: 0;
    padding: 0;
    border: 0;
  }

  legend {
    padding: 0;
    margin-bottom: 4px;
  }

  /* Three across on a phone. A later season wraps to a new row rather than squeezing. */
  .seasons {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(5.75rem, 1fr));
    gap: 6px;
  }

  label {
    position: relative;
    display: flex;
    cursor: pointer;
  }

  .chip {
    flex: 1 1 auto;
    display: flex;
    flex-direction: column;
    gap: 2px;
    min-height: 54px;
    padding: 6px 8px;
    border: 1px solid var(--border);
    border-radius: 12px;
    background: var(--surface);
  }

  .name {
    display: flex;
    align-items: center;
    gap: 5px;
    font-size: 0.9375rem;
    font-weight: 600;
    line-height: 1.2;
    white-space: nowrap;
  }

  .span {
    color: var(--ink-muted);
    font-size: 0.72rem;
    line-height: 1.2;
    text-wrap: balance;
  }

  svg {
    flex: 0 0 auto;
  }

  .box {
    fill: none;
    stroke: var(--ink-muted);
    stroke-width: 1.5;
  }

  .check {
    fill: none;
    stroke: var(--on-cta);
    stroke-width: 2;
    stroke-linecap: round;
    stroke-linejoin: round;
    visibility: hidden;
  }

  input:checked + .chip {
    border-color: var(--green);
    background: var(--green-soft);
  }

  input:checked + .chip .box {
    fill: var(--cta);
    stroke: var(--cta);
  }

  input:checked + .chip .check {
    visibility: visible;
  }

  input:focus-visible + .chip {
    outline: 2px solid var(--blue);
    outline-offset: 2px;
  }

  @media (prefers-reduced-motion: no-preference) {
    .chip {
      transition:
        background-color 0.15s ease,
        border-color 0.15s ease;
    }
  }
</style>
