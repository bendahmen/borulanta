import { fileURLToPath } from 'node:url';

import svelte from '@astrojs/svelte';
import { defineConfig } from 'astro/config';

import { checkDataHash } from './src/lib/data-hash.js';

const hashPaths = {
  repoRoot: fileURLToPath(new URL('..', import.meta.url)),
  metaPath: fileURLToPath(new URL('./src/generated/meta.json', import.meta.url)),
};

// A failed build keeps the previous version live, which is the point: old JSON
// must never publish under new CSVs. On astro:build:start so it holds however the
// build is invoked. In dev it only warns, because you may be mid-edit.
function staleDataCheck() {
  return {
    name: 'borulanta:stale-data-check',
    hooks: {
      'astro:build:start': () => checkDataHash(hashPaths),
      'astro:server:start': ({ logger }) => {
        try {
          checkDataHash(hashPaths);
        } catch (error) {
          logger.warn(error.message);
        }
      },
    },
  };
}

export default defineConfig({
  integrations: [svelte(), staleDataCheck()],
  // Its bar sits on top of the bottom tabs when testing on a phone.
  devToolbar: { enabled: false },
  // Most visits are first visits from a WhatsApp link, so skip the extra round trip.
  build: { inlineStylesheets: 'always' },
});
