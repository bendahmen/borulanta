import assert from 'node:assert/strict';
import { test } from 'node:test';

import { createSeasonScope, readEarlyTicks, scopeView } from '../src/lib/season-scope.js';
import { settle } from './helpers/fake-dom.js';

const KNOWN = ['s3', 's2', 's1'];
const ALL = ['s1', 's2', 's3'];

function fakeWin({ search = '', stored = null } = {}) {
  const win = {
    location: { search, hash: '' },
    localStorage: {
      value: stored,
      getItem() {
        return this.value;
      },
      setItem(_key, value) {
        this.value = value;
      },
    },
    history: {
      state: null,
      replaceState(_state, _title, url) {
        win.location.search = url;
      },
    },
  };
  return win;
}

// A fetch that answers only when told to, so a test can choose the order of replies.
function deferredFetch() {
  const calls = [];
  return {
    calls,
    fetchFile: (file) =>
      new Promise((resolve, reject) => {
        calls.push({ file, resolve, reject });
      }),
    reply: async (file, body) => {
      calls.findLast((call) => call.file === file).resolve({ ok: true, status: 200, json: async () => body });
      await settle();
    },
    fail: async (file) => {
      calls.findLast((call) => call.file === file).reject(new Error('offline'));
      await settle();
    },
  };
}

function setUp(t, options = {}) {
  const win = fakeWin(options);
  const net = deferredFetch();
  const states = [];
  const scope = createSeasonScope({
    knownIds: KNOWN,
    fetchFile: net.fetchFile,
    pick: (file) => file,
    onchange: (state) => states.push(state),
    win,
  });
  return { win, net, scope, states };
}

const ALL_FILE = '/data/seasons/s1-s2-s3.json';

test('starts with everything ticked and nothing loaded, for the first render', (t) => {
  const { scope } = setUp(t);
  assert.deepEqual(scope.state, { ticks: ALL, subset: null, failed: false, busy: false });
});

test('a visitor with nothing remembered gets every season, and the URL stays clean', async (t) => {
  const { scope, net, win } = setUp(t);
  scope.start(null);
  assert.deepEqual(net.calls.map((call) => call.file), [ALL_FILE]);
  await net.reply(ALL_FILE, { has_matches: true });
  assert.deepEqual(scope.state.subset, { has_matches: true });
  assert.equal(win.location.search, '');
});

test('the URL wins over storage, and storage goes in the URL', async (t) => {
  const fromUrl = setUp(t, { search: '?seasons=s1', stored: 's2' });
  fromUrl.scope.start(null);
  assert.deepEqual(fromUrl.scope.state.ticks, ['s1']);
  assert.equal(fromUrl.net.calls[0].file, '/data/seasons/s1.json');

  const fromStorage = setUp(t, { stored: 's2' });
  fromStorage.scope.start(null);
  assert.deepEqual(fromStorage.scope.state.ticks, ['s2']);
  assert.equal(fromStorage.win.location.search, '?seasons=s2');
});

test('ticks from a shared link are remembered on arrival, and follow to the next page', (t) => {
  const arrival = setUp(t, { search: '?seasons=s3', stored: 's1,s2' });
  arrival.scope.start(null);
  assert.deepEqual(arrival.scope.state.ticks, ['s3']);
  assert.equal(arrival.win.localStorage.value, 's3');
  assert.equal(arrival.win.location.search, '?seasons=s3');

  // The sub-navigation's links carry no query, so the next page has only storage to go on.
  const nextPage = setUp(t, { stored: arrival.win.localStorage.value });
  nextPage.scope.start(null);
  assert.deepEqual(nextPage.scope.state.ticks, ['s3']);
  assert.equal(nextPage.net.calls[0].file, '/data/seasons/s3.json');
  assert.equal(nextPage.win.location.search, '?seasons=s3');
});

test('a link that ticks everything is remembered too', (t) => {
  const { scope, win } = setUp(t, { search: '?seasons=s1,s2,s3', stored: 's3' });
  scope.start(null);
  assert.equal(win.localStorage.value, 's1,s2,s3');
});

test('with nothing in the URL, nothing is written to storage on arrival', (t) => {
  const stored = setUp(t, { stored: 's2' });
  stored.scope.start(null);
  assert.equal(stored.win.localStorage.value, 's2');
  const nothing = setUp(t);
  nothing.scope.start(null);
  assert.equal(nothing.win.localStorage.value, null);
});

test('a tap before hydration counts, is remembered, and takes the preloaded file for the cache', async (t) => {
  const { scope, net, win } = setUp(t);
  scope.start(['s3']);
  assert.deepEqual(scope.state.ticks, ['s3']);
  assert.equal(win.location.search, '?seasons=s3');
  assert.equal(win.localStorage.value, 's3');
  assert.deepEqual(net.calls.map((call) => call.file), [ALL_FILE, '/data/seasons/s3.json']);
});

test('a tap that empties every box before hydration fetches nothing it needs', async (t) => {
  const { scope, net } = setUp(t);
  scope.start([]);
  assert.deepEqual(scope.state.ticks, []);
  assert.deepEqual(net.calls.map((call) => call.file), [ALL_FILE]);
  assert.equal(scopeView(scope.state), 'no-matches');
});

test('a change of ticks writes the URL and storage, and fetches the right file', async (t) => {
  const { scope, net, win } = setUp(t);
  scope.start(null);
  scope.changeTick('s1', false);
  assert.deepEqual(scope.state.ticks, ['s2', 's3']);
  assert.equal(win.location.search, '?seasons=s2,s3');
  assert.equal(win.localStorage.value, 's2,s3');
  assert.equal(net.calls.at(-1).file, '/data/seasons/s2-s3.json');
});

test('zero ticks has no file, and clears what was showing', async (t) => {
  const { scope, net } = setUp(t, { search: '?seasons=s1' });
  scope.start(null);
  await net.reply('/data/seasons/s1.json', { has_matches: true });
  const fetched = net.calls.length;
  scope.changeTick('s1', false);
  assert.equal(net.calls.length, fetched);
  assert.equal(scope.state.subset, null);
  assert.equal(scopeView(scope.state), 'no-matches');
});

test('a late reply for old ticks is not shown', async (t) => {
  const { scope, net } = setUp(t);
  scope.start(null);
  scope.changeTick('s1', false);
  await net.reply('/data/seasons/s2-s3.json', { has_matches: true, name: 'new' });
  await net.reply(ALL_FILE, { has_matches: true, name: 'old' });
  assert.equal(scope.state.subset.name, 'new');
});

test('the page keeps what it has while the next one loads', async (t) => {
  const { scope, net } = setUp(t);
  scope.start(null);
  await net.reply(ALL_FILE, { has_matches: true, name: 'first' });
  scope.changeTick('s1', false);
  assert.equal(scope.state.subset.name, 'first');
});

test('busy is only for a wait long enough to notice', async (t) => {
  t.mock.timers.enable({ apis: ['setTimeout'] });
  const { scope } = setUp(t);
  scope.start(null);
  assert.equal(scope.state.busy, false);
  t.mock.timers.tick(199);
  assert.equal(scope.state.busy, false);
  t.mock.timers.tick(1);
  assert.equal(scope.state.busy, true);
});

test('a failed fetch says so, and a reload can succeed', async (t) => {
  const { scope, net } = setUp(t);
  scope.start(null);
  await net.fail(ALL_FILE);
  assert.equal(scopeView(scope.state), 'failed');
  scope.reload();
  assert.equal(scope.state.failed, false);
  await net.reply(ALL_FILE, { has_matches: true });
  assert.equal(scopeView(scope.state), 'ready');
});

test('another part of the page can report a failure, and the next load clears it', async (t) => {
  const { scope, net } = setUp(t);
  scope.start(null);
  scope.fail();
  assert.equal(scopeView(scope.state), 'failed');
  scope.changeTick('s1', false);
  assert.equal(scope.state.failed, false);
  await net.reply('/data/seasons/s2-s3.json', { has_matches: true });
  assert.equal(scopeView(scope.state), 'ready');
});

test('every change reaches the page', async (t) => {
  const { scope, net, states } = setUp(t);
  scope.start(null);
  await net.reply(ALL_FILE, { has_matches: true });
  assert.ok(states.length >= 2);
  assert.deepEqual(states.at(-1), scope.state);
});

test('the view for each state', () => {
  assert.equal(scopeView({ ticks: [], failed: true, subset: { has_matches: true } }), 'no-matches');
  assert.equal(scopeView({ ticks: ['s1'], failed: true, subset: null }), 'failed');
  assert.equal(scopeView({ ticks: ['s1'], failed: false, subset: null }), 'loading');
  assert.equal(scopeView({ ticks: ['s1'], failed: false, subset: { has_matches: false } }), 'no-matches');
  assert.equal(scopeView({ ticks: ['s1'], failed: false, subset: { has_matches: true } }), 'ready');
});

function fakeDoc(boxes, ticksSet) {
  return {
    documentElement: { hasAttribute: (name) => ticksSet && name === 'data-ticks-set' },
    querySelectorAll: (selector) => {
      assert.equal(selector, 'input[data-season-tick]');
      return boxes;
    },
  };
}

test('the boxes are read once the inline script has set them, and not before', () => {
  const boxes = [
    { value: 's3', checked: true },
    { value: 's2', checked: false },
    { value: 's1', checked: true },
  ];
  assert.deepEqual(readEarlyTicks(fakeDoc(boxes, true), KNOWN), ['s1', 's3']);
  assert.equal(readEarlyTicks(fakeDoc(boxes, false), KNOWN), null);
});
