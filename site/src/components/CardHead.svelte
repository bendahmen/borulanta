<script>
  // The tag, title and subtitle come from wording.json and are shown as exported.
  let { card, id = undefined } = $props();
</script>

<header>
  <p class="tag">{card.tag}</p>
  <h2 {id}>{card.title}</h2>
  {#if card.subtitle}
    <p class="subtitle">{card.subtitle}</p>
  {/if}
</header>

<style>
  h2 {
    margin-top: 2px;
    font-family: var(--font-display);
    font-size: 1.5rem;
    font-weight: 700;
    letter-spacing: 0.01em;
    line-height: 1.1;
    text-transform: uppercase;
  }

  .subtitle {
    margin-top: 4px;
    color: var(--ink-muted);
    font-size: 0.9375rem;
    line-height: 1.35;
  }
</style>
