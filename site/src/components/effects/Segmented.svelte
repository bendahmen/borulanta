<script>
  // Native radios, so arrow keys, focus and the screen reader all work as usual.
  let { legend, name, options, value, onchange, layout = 'row' } = $props();
</script>

<fieldset class="segmented">
  <legend class="tag">{legend}</legend>
  <div class="fit">
    <div class="options" class:grid={layout === 'grid'}>
      {#each options as option (option.value)}
        <label>
          <input
            class="visually-hidden"
            type="radio"
            autocomplete="off"
            {name}
            value={option.value}
            checked={option.value === value}
            onchange={() => onchange(option.value)}
          />
          <span class="option">{option.label}</span>
        </label>
      {/each}
    </div>
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

  /* The rem widths below only matter once text is enlarged: a media query cannot see that. */
  .fit {
    container-type: inline-size;
  }

  .options {
    display: flex;
    gap: 3px;
    padding: 3px;
    border: 1px solid var(--border);
    border-radius: 12px;
    background: var(--bg);
  }

  label {
    position: relative;
    flex: 1 1 auto;
    display: flex;
    min-width: 0;
    cursor: pointer;
  }

  /* In a row the first option is the short one. It keeps one line, and the long ridge
     label takes the rest of the width and wraps once. */
  .options:not(.grid) label:first-child {
    flex: 0 0 auto;
  }

  .grid {
    display: grid;
    grid-template-columns: repeat(2, minmax(0, 1fr));
  }

  .option {
    flex: 1 1 auto;
    display: flex;
    align-items: center;
    justify-content: center;
    min-height: 44px;
    padding: 3px 8px;
    border: 2px solid transparent;
    border-radius: 9px;
    color: var(--ink);
    font-size: 0.9375rem;
    line-height: 1.2;
    text-align: center;
    text-wrap: balance;
  }

  /* The border is what marks the choice: the fill alone is too faint against the track. */
  input:checked + .option {
    border-color: var(--blue);
    background: var(--blue-soft);
    color: var(--blue);
    font-weight: 600;
  }

  input:focus-visible + .option {
    outline: 2px solid var(--blue);
    outline-offset: 1px;
  }

  @media (min-width: 768px) {
    @container (min-width: 20rem) {
      .grid {
        grid-template-columns: repeat(4, minmax(0, 1fr));
      }

      /* At 768 px "difference" only just fits its button, with this and no more. */
      .grid .option {
        padding-inline: 3px;
      }
    }
  }

  /* Large text: one option to a line, so no word runs out of its button. */
  @container (max-width: 15rem) {
    .options,
    .options.grid {
      display: flex;
      flex-direction: column;
    }

    .options:not(.grid) label:first-child {
      flex: 1 1 auto;
    }
  }

  @media (prefers-reduced-motion: no-preference) {
    .option {
      transition:
        background-color 0.15s ease,
        border-color 0.15s ease,
        color 0.15s ease;
    }
  }
</style>
