// Lets a test import a .svelte file and render it to HTML through svelte/server, as the build
// does, with no browser. Import this first, then import the component with `await import()`:
// static imports are all loaded before any of them runs, which is too early for these hooks.
// Rendering runs no onMount and fires no events, so what a page does on a tap is tested through
// the plain modules behind it, and this covers the markup those decisions end up in.
import { readFileSync } from 'node:fs';
import { registerHooks } from 'node:module';
import { fileURLToPath } from 'node:url';

import { compile, compileModule } from 'svelte/compiler';

registerHooks({
  load(url, context, nextLoad) {
    const filename = url.startsWith('file:') ? fileURLToPath(url) : '';
    if (filename.endsWith('.svelte')) {
      const { js } = compile(readFileSync(filename, 'utf8'), { filename, generate: 'server' });
      return { format: 'module', source: js.code, shortCircuit: true };
    }
    if (filename.endsWith('.svelte.js')) {
      const { js } = compileModule(readFileSync(filename, 'utf8'), { filename, generate: 'server' });
      return { format: 'module', source: js.code, shortCircuit: true };
    }
    return nextLoad(url, context);
  },
});
