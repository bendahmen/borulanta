<script>
  // What a season-scoped page says instead of its cards. A message, or the failed fetch.
  import { SEASON_SCOPE } from '../lib/labels.js';

  let { message = null, onretry = null } = $props();
</script>

{#if onretry}
  <div class="card message" role="alert">
    <p>{SEASON_SCOPE.loadFailed}</p>
    <button type="button" class="retry" onclick={onretry}>{SEASON_SCOPE.retry}</button>
  </div>
{:else}
  <p class="card message" role="status">{message}</p>
{/if}

<style>
  .message {
    color: var(--ink-muted);
  }

  .retry {
    display: inline-flex;
    align-items: center;
    min-height: 44px;
    margin-top: 6px;
    padding: 0 16px;
    border: 1px solid var(--blue);
    border-radius: 12px;
    background: var(--surface);
    color: var(--blue);
    font: inherit;
    font-weight: 600;
    cursor: pointer;
  }
</style>
