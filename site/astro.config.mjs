import { existsSync, readFileSync, writeFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';

import svelte from '@astrojs/svelte';
import { defineConfig } from 'astro/config';

import { checkDataHash } from './src/lib/data-hash.js';
import { addModulePreloads } from './src/lib/module-preload.js';

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

// Only pages with an island are changed. See src/lib/module-preload.js for why.
function preloadIslandChunks() {
  return {
    name: 'borulanta:preload-island-chunks',
    hooks: {
      'astro:build:done': ({ dir, pages }) => {
        const readChunk = (name) => readFileSync(new URL(`_astro/${name}`, dir), 'utf8');
        for (const { pathname } of pages) {
          const file = new URL(`${pathname}index.html`, dir);
          if (!existsSync(file)) continue;
          const html = readFileSync(file, 'utf8');
          const updated = addModulePreloads(html, readChunk);
          if (updated !== html) writeFileSync(file, updated);
        }
      },
    },
  };
}

export default defineConfig({
  // The live address. Link previews need absolute image URLs, so change this with the domain.
  site: 'https://borulanta.bendahmen.workers.dev',
  integrations: [svelte(), staleDataCheck(), preloadIslandChunks()],
  // Its bar sits on top of the bottom tabs when testing on a phone.
  devToolbar: { enabled: false },
  // Most visits are first visits from a WhatsApp link, so skip the extra round trip.
  build: { inlineStylesheets: 'always' },
});
