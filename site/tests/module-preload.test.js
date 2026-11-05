import assert from 'node:assert/strict';
import { test } from 'node:test';

import { addModulePreloads, chunkImports, reachableChunks } from '../src/lib/module-preload.js';

// The shapes the bundler really writes, static imports with quotes and dynamic with backticks.
const chunks = {
  'entry.js': 'import{a as b}from"./shared.js";import"./side.js";function c(){return d(()=>import(`./plot.js`).then(e=>e),[])}',
  'renderer.js': 'import{a}from"./shared.js";',
  'shared.js': 'var a=1;export{a};',
  'side.js': 'document.title=`x`;',
  'plot.js': 'import{a}from"./shared.js";export const p=a;',
};
const read = (name) => chunks[name];

test('finds static and dynamic imports', () => {
  assert.deepEqual(chunkImports(chunks['entry.js']), ['shared.js', 'side.js', 'plot.js']);
  assert.deepEqual(chunkImports('const text = "from nowhere";'), []);
});

test('walks the graph once, however many routes lead to a chunk', () => {
  assert.deepEqual(reachableChunks(['entry.js', 'renderer.js'], read).sort(), [
    'entry.js',
    'plot.js',
    'renderer.js',
    'shared.js',
    'side.js',
  ]);
});

test('a page with an island gets a link per chunk, in its head', () => {
  const html = '<html><head><title>x</title></head><body><astro-island component-url="/_astro/entry.js" renderer-url="/_astro/renderer.js"></astro-island></body></html>';
  const out = addModulePreloads(html, read);
  assert.equal((out.match(/rel="modulepreload"/g) ?? []).length, 5);
  assert.ok(out.includes('<link rel="modulepreload" href="/_astro/plot.js">'));
  assert.ok(out.indexOf('<head>') < out.indexOf('modulepreload'));
  assert.ok(out.lastIndexOf('modulepreload') < out.indexOf('<title>'));
});

test('the largest chunk comes first, so it never queues behind the small ones', () => {
  const sized = { 'a.js': 'import "./big.js";import "./small.js";', 'big.js': 'x'.repeat(500), 'small.js': 'y' };
  const out = addModulePreloads('<head><title>t</title><div component-url="/_astro/a.js"></div>', (name) => sized[name]);
  const order = [...out.matchAll(/modulepreload" href="\/_astro\/([^"]+)"/g)].map((match) => match[1]);
  assert.deepEqual(order, ['big.js', 'a.js', 'small.js']);
});

test('an island page drops its font preloads, and a page with no island keeps them', () => {
  const font = '<link rel="preload" href="/fonts/a.woff2" as="font" type="font/woff2" crossorigin>';
  const sized = { 'a.js': 'x' };
  const island = `<head>${font}<title>t</title><div component-url="/_astro/a.js"></div>`;
  assert.ok(!addModulePreloads(island, (name) => sized[name]).includes('as="font"'));
  const plain = `<head>${font}<title>t</title>`;
  assert.equal(addModulePreloads(plain, (name) => sized[name]), plain);
});

test('a page with no island is unchanged, so Plot cannot load on it', () => {
  const html = '<html><head></head><body><p>Home</p></body></html>';
  assert.equal(addModulePreloads(html, read), html);
});
