import assert from 'node:assert/strict';
import { test } from 'node:test';

import { createSubsetLoader } from '../src/lib/subset-loader.js';

// A request that never settles should fail its test, not leave the run waiting on it.
const timed = { timeout: 2000 };

const reply = (body, ok = true, status = 200) => ({ ok, status, json: async () => body });
const file = (id) => ({ has_matches: true, regression: { id }, matches: ['dropped'] });

// A fetch that answers only when told to, so a test can choose the order of replies.
function controlledFetch() {
  const pending = new Map();
  const calls = [];
  const fetchFile = (path) => {
    calls.push(path);
    return new Promise((resolve, reject) => pending.set(path, { resolve, reject }));
  };
  return { fetchFile, pending, calls };
}

test('keeps only what the page uses', timed, async () => {
  const loader = createSubsetLoader(async () => reply(file('a')));
  const { stale, data } = await loader.load('a.json');
  assert.equal(stale, false);
  assert.deepEqual(data, { has_matches: true, regression: { id: 'a' } });
});

test('a second load of a file is served from memory', timed, async () => {
  // Answers every call, so that a loader with no cache fails the count instead of waiting.
  const calls = [];
  const loader = createSubsetLoader(async (path) => {
    calls.push(path);
    return reply(file('a'));
  });
  await loader.load('a.json');
  const second = await loader.load('a.json');
  assert.equal(second.stale, false);
  assert.equal(second.data.regression.id, 'a');
  assert.deepEqual(calls, ['a.json']);
});

test('two loads of a file in flight share one request', timed, async () => {
  const { fetchFile, pending, calls } = controlledFetch();
  const loader = createSubsetLoader(fetchFile);
  const first = loader.load('a.json');
  const second = loader.load('a.json');
  assert.deepEqual(calls, ['a.json']);
  pending.get('a.json').resolve(reply(file('a')));
  assert.equal((await first).stale, true);
  assert.equal((await second).stale, false);
});

test('a reply for ticks no longer current comes back stale', timed, async () => {
  const { fetchFile, pending } = controlledFetch();
  const loader = createSubsetLoader(fetchFile);
  const older = loader.load('a.json');
  const newer = loader.load('b.json');
  pending.get('b.json').resolve(reply(file('b')));
  assert.equal((await newer).stale, false);
  pending.get('a.json').resolve(reply(file('a')));
  assert.equal((await older).stale, true);
});

test('cancel makes a pending reply stale', timed, async () => {
  const { fetchFile, pending } = controlledFetch();
  const loader = createSubsetLoader(fetchFile);
  const request = loader.load('a.json');
  loader.cancel();
  pending.get('a.json').resolve(reply(file('a')));
  assert.equal((await request).stale, true);
});

test('a failure throws, and a retry fetches again', timed, async () => {
  let attempts = 0;
  const loader = createSubsetLoader(async () => {
    attempts += 1;
    return attempts === 1 ? reply(null, false, 503) : reply(file('a'));
  });
  await assert.rejects(loader.load('a.json'));
  const { data } = await loader.load('a.json');
  assert.equal(attempts, 2);
  assert.equal(data.regression.id, 'a');
});

test('a network error throws, and malformed JSON throws', timed, async () => {
  const offline = createSubsetLoader(async () => {
    throw new TypeError('Failed to fetch');
  });
  await assert.rejects(offline.load('a.json'));

  const broken = createSubsetLoader(async () => ({
    ok: true,
    status: 200,
    json: async () => {
      throw new SyntaxError('Unexpected token');
    },
  }));
  await assert.rejects(broken.load('a.json'));
});

test('a stale failure is stale, not an error', timed, async () => {
  const { fetchFile, pending } = controlledFetch();
  const loader = createSubsetLoader(fetchFile);
  const older = loader.load('a.json');
  const newer = loader.load('b.json');
  pending.get('a.json').reject(new TypeError('Failed to fetch'));
  assert.deepEqual(await older, { stale: true, data: null });
  pending.get('b.json').resolve(reply(file('b')));
  assert.equal((await newer).stale, false);
});
