import './helpers/svelte-server.js';

import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { test } from 'node:test';
import vm from 'node:vm';

import { render } from 'svelte/server';

import { tapPicksRow } from '../src/lib/data-table.js';
import { MATCHES, NAV, PLAYERS } from '../src/lib/labels.js';
import { everyResultColumns } from '../src/lib/match-tables.js';
import { FIRST_ROSTER, ROSTER_SORT, createRosterSelection, findProfile, pickPlayers, playerPageView, rosterView } from '../src/lib/player-data.js';
import { rosterColumns } from '../src/lib/player-tables.js';
import { redirectScript } from '../src/lib/redirect.js';
import { createSeasonScope } from '../src/lib/season-scope.js';
import { fakeElement } from './helpers/fake-dom.js';

const { default: DataTable } = await import('../src/components/DataTable.svelte');
const { default: PlayerProfile } = await import('../src/components/players/PlayerProfile.svelte');
const { default: RosterPage } = await import('../src/components/players/RosterPage.svelte');
const { default: PlayerPage } = await import('../src/components/players/PlayerPage.svelte');

const read = (path) => readFileSync(new URL(path, import.meta.url), 'utf8');
const { players_cards: cards, matches_cards: matchCards } = JSON.parse(read('../src/generated/wording.json'));
const meta = JSON.parse(read('../src/generated/meta.json'));
const seasonFile = (id) => JSON.parse(read(`../public/data/seasons/${id}.json`));

// ---- The page's logic, on the same season scope the page uses ----

const row = (player, attendance_rate) => ({ player, attendance_rate, avg_points: 1, avg_goals_scored: 2, avg_goals_conceded: 3 });
const profile = (player) => ({ player, headline: [], timeline: [] });
const fileOf = (...names) => ({
  has_matches: true,
  matches: [],
  attendance_list: names.map((name, index) => row(name, 1 - index / 10)),
  profiles: [...names].sort().map(profile),
});
const FILES = { '/data/seasons/s1-s2.json': fileOf('Boris', 'Vitto', 'Ben', 'Anton'), '/data/seasons/s2.json': fileOf('Boris', 'Ben') };

// Roster with `stored` as the fee picker's person, recording every write to storage.
function roster(stored) {
  const writes = [];
  let value = stored;
  const storage = {
    getItem: (key) => (key === 'borulanta.player' ? value : null),
    setItem: (key, written) => writes.push(key === 'borulanta.player' ? ['player', written] : ['other', key]),
    move: (next) => {
      value = next;
    },
  };
  const win = {
    location: { search: '', hash: '' },
    localStorage: storage,
    history: {
      state: null,
      replaceState(_state, _title, url) {
        win.location.search = url;
      },
    },
  };
  let scopeState;
  const scope = createSeasonScope({
    knownIds: ['s2', 's1'],
    pick: pickPlayers,
    fetchFile: async (file) => ({ ok: true, status: 200, json: async () => FILES[file] }),
    onchange: (next) => {
      scopeState = next;
    },
    win,
  });
  const chooser = createRosterSelection({ getStorage: () => storage });
  return {
    scope,
    chooser,
    storage,
    playerWrites: () => writes.filter(([kind]) => kind === 'player'),
    allWrites: () => writes,
    view: () => rosterView(scopeState.subset, chooser.state, ROSTER_SORT, false),
    open: async () => {
      chooser.open();
      await scope.start(null);
    },
  };
}

test('it opens on the stored player, with that player profile under the table', async () => {
  const page = roster('Ben');
  await page.open();
  assert.equal(page.view().selected, 'Ben');
  assert.equal(page.view().profile.player, 'Ben');
});

test('with nobody stored, or a stored player not in the window, it opens on R first row', async () => {
  for (const stored of [null, 'Akash']) {
    const page = roster(stored);
    await page.open();
    assert.equal(page.view().selected, 'Boris');
    assert.equal(page.view().profile.player, 'Boris');
  }
});

test('picking moves the table and the profile together', async () => {
  const page = roster('Ben');
  await page.open();
  page.chooser.pick('Vitto');
  assert.equal(page.view().selected, 'Vitto');
  assert.equal(page.view().profile.player, 'Vitto');
});

test('picking does not write borulanta.player, and nor does a tick change', async () => {
  const page = roster('Ben');
  await page.open();
  page.chooser.pick('Vitto');
  await page.scope.changeTick('s1', false);
  await page.scope.changeTick('s1', true);
  assert.deepEqual(page.playerWrites(), []);
  assert.ok(page.allWrites().length > 0, 'the ticks are written, so the recorder is working');
});

test('the stored player is read on arrival and not again', async () => {
  const page = roster('Ben');
  await page.open();
  page.storage.move('Vitto');
  await page.scope.changeTick('s1', false);
  await page.scope.changeTick('s1', true);
  assert.equal(page.view().selected, 'Ben');
});

test('a player dropped by the ticks gives way to the stored player, then to the first row', async () => {
  const withStored = roster('Ben');
  await withStored.open();
  withStored.chooser.pick('Anton');
  await withStored.scope.changeTick('s1', false);
  assert.equal(withStored.view().selected, 'Ben');

  const withNobody = roster(null);
  await withNobody.open();
  withNobody.chooser.pick('Anton');
  await withNobody.scope.changeTick('s1', false);
  assert.equal(withNobody.view().selected, 'Boris');
});

test('zero ticks leaves nobody selected and asks for no file', async () => {
  const page = roster('Ben');
  await page.open();
  await page.scope.changeTick('s1', false);
  await page.scope.changeTick('s2', false);
  assert.equal(page.view().selected, null);
  assert.equal(page.view().profile, null);
  assert.deepEqual(page.view().rows, []);
});

// ---- A tap on a row ----

const tappedOn = (...matching) =>
  fakeElement({
    closest: (selector) => (selector.split(',').some((part) => matching.includes(part.trim())) ? {} : null),
  });

test('a chevron tap does not pick, because it landed on a button in the row', () => {
  assert.equal(tapPicksRow(tappedOn('button')), false);
});

test('a tap on the row link does not pick either', () => {
  assert.equal(tapPicksRow(tappedOn('a')), false);
});

test('a tap anywhere else on the row does', () => {
  assert.equal(tapPicksRow(tappedOn()), true);
});

// ---- The markup, as the component renders it ----

const realRoster = seasonFile('s1-s2-s3');

function rosterHtml({ selected = 'Ben', sort = ROSTER_SORT, showingAll = false } = {}) {
  const view = rosterView(realRoster, { picked: selected, stored: null }, sort, showingAll);
  return render(DataTable, {
    props: {
      className: 'dt-roster',
      caption: 'c',
      columns: rosterColumns(cards.who_turns_up.headings, PLAYERS, view.top),
      rows: view.rows,
      rowKey: (entry) => entry.player,
      sort,
      onsort: () => {},
      picked: view.selected,
      onpick: () => {},
      rowClass: (entry) => (view.top.has(entry.player) ? 'rank-top3' : ''),
      foldLabel: (entry) => PLAYERS.foldFor(entry.player),
    },
  }).body;
}

const tableRows = (html) => html.split('<tr').slice(1);
const bodyRow = (html, player) => tableRows(html).find((part) => part.includes(`>${player}</button>`));
const columnsOf = (part) => [...part.matchAll(/<t[hd]\b[^>]*class="[^"]*\bcol-([a-z_-]+)/g)].map((match) => match[1]);

test('the picked row is marked, and only that row', () => {
  const html = rosterHtml({ selected: 'Vitto' });
  const marked = tableRows(html).filter((part) => /^[^>]*\bis-picked\b/.test(part));
  assert.equal(marked.length, 1);
  assert.ok(marked[0].includes('>Vitto</button>'));
  assert.equal(html.match(/aria-current="true"/g).length, 1);
  assert.match(bodyRow(html, 'Vitto'), /aria-current="true"[^>]*>Vitto</);
});

test('the top three carry the class and the word, whoever is picked and however it is sorted', () => {
  const html = rosterHtml({ selected: 'Oleg', sort: { key: 'player', direction: 'ascending' }, showingAll: true });
  const marked = tableRows(html).filter((part) => /^[^>]*\brank-top3\b/.test(part));
  assert.deepEqual(marked.map((part) => part.match(/>([^<]+)<\/button>/)[1]).sort(), realRoster.attendance_list.slice(0, 3).map((entry) => entry.player).sort());
  assert.ok(marked.every((part) => part.includes(`>${PLAYERS.topThree}<`)));
  assert.equal(html.split(`>${PLAYERS.topThree}<`).length - 1, 3);
});

test('every row has a cell under every heading, in the same order', () => {
  const [head, ...rows] = tableRows(rosterHtml({ showingAll: true }));
  const headings = columnsOf(head);
  assert.deepEqual(headings, ['player', 'attendance_rate', 'avg_points', 'avg_goals_scored', 'avg_goals_conceded', 'page']);
  for (const part of rows) assert.deepEqual(columnsOf(part), headings);
});

test('a chevron is a button of its own, beside the name and not around it', () => {
  const part = bodyRow(rosterHtml(), 'Boris');
  assert.match(part, /<button type="button" class="dt-unfold"[^>]*aria-expanded="false"[^>]*aria-label="Goals for Boris"/);
  assert.ok(part.indexOf('dt-unfold') < part.indexOf('dt-pick'));
  assert.ok(part.indexOf('</button>') < part.indexOf('dt-pick'), 'the chevron closes before the name starts');
});

test('a table with no foldLabel has no chevron, and no is-sorted cells', () => {
  const { body } = render(DataTable, {
    props: {
      className: 'dt-results',
      caption: 'c',
      columns: everyResultColumns(matchCards.every_result, MATCHES),
      rows: realRoster.matches.slice(0, 2),
      rowKey: (entry) => entry.date,
      picked: realRoster.matches[0].date,
      onpick: () => {},
    },
  });
  assert.ok(!body.includes('dt-unfold'));
  assert.ok(!body.includes('is-sorted'));
});

test('the rows sorted on a folded column say so to the styles, on that column alone', () => {
  const html = rosterHtml({ sort: { key: 'avg_goals_scored', direction: 'descending' } });
  const part = bodyRow(html, 'Boris');
  assert.match(part, /col-avg_goals_scored[^"]*is-sorted/);
  assert.equal(part.split('is-sorted').length - 1, 1);
});

test('the link column has no sort button, and the other five have one each', () => {
  const [head] = tableRows(rosterHtml());
  assert.equal(head.split('dt-sort"').length - 1, 5);
  assert.ok(!/col-page[\s\S]*dt-sort"/.test(head));
});

test('the first rows and a selected player past them are all that show', () => {
  const late = realRoster.attendance_list.at(-1).player;
  const rows = tableRows(rosterHtml({ selected: late })).slice(1);
  assert.equal(rows.length, FIRST_ROSTER + 1);
  assert.match(rows.at(-1), new RegExp(`>${late}</button>`));
});

// ---- The profile ----

const withNote = (player) => `${player}${cards.player.note_after_name}`;
const profileHtml = (id, player, note = null) => {
  const file = seasonFile(id);
  return render(PlayerProfile, { props: { profile: findProfile(file, player), matches: file.matches, cards, note } }).body;
};
const headingOrder = (html) => [...html.matchAll(/<(h[1-6])\b/g)].map((match) => match[1]);

test('headings go down one step at a time, on Roster and on the player page', () => {
  assert.deepEqual(headingOrder(profileHtml('s3', 'Boris', withNote('Boris'))), ['h2', 'h3', 'h2']);
  assert.deepEqual(headingOrder(profileHtml('s3', 'Boris')), ['h2', 'h2']);
});

test('the note about a table above is on Roster and not on the player page', () => {
  assert.ok(profileHtml('s3', 'Boris', withNote('Boris')).includes(withNote('Boris')));
  const onPage = profileHtml('s3', 'Boris');
  assert.ok(!onPage.includes(cards.player.note_after_name));
  assert.ok(!onPage.includes(cards.player.title));
});

test('the profile shown is the one passed, with R strings', () => {
  const html = profileHtml('s1-s3', 'Oleg');
  assert.ok(html.includes('3 of 24'));
  assert.ok(html.includes('12% turnout'));
  assert.ok(html.includes(findProfile(seasonFile('s1-s3'), 'Oleg').differential_note));
});

test('an ever-present has the no-comparison wording, and a blank rate on the Missed row', () => {
  const html = profileHtml('s3', 'Berghi');
  assert.ok(html.includes('No comparison to draw'));
  assert.ok(!html.includes('NaN'));
  const missed = tableRows(html).find((part) => part.includes(`>${cards.with_without.sides.missed}<`));
  assert.match(missed, /col-points_per_match[^>]*>(<!---->)*<\/td>/);
});

test('the strip is one picture to a screen reader, with no Tab stops', () => {
  const file = seasonFile('s1-s3');
  const oleg = findProfile(file, 'Oleg');
  const html = profileHtml('s1-s3', 'Oleg');
  const squares = html.slice(html.indexOf('class="squares'), html.indexOf('class="below'));
  assert.match(squares, /role="img"/);
  assert.match(squares, new RegExp(`aria-label="${PLAYERS.stripSummary(oleg.appearances, oleg.matches)}"`));
  assert.equal(squares.split('data-date=').length - 1, oleg.matches);
  assert.ok(!/<(a|button|input)\b|tabindex/.test(squares));
});

test('the dates are there in words, behind one control', () => {
  const file = seasonFile('s1-s3');
  const oleg = findProfile(file, 'Oleg');
  const html = profileHtml('s1-s3', 'Oleg');
  assert.equal(html.split('<summary').length - 1, 1);
  const dates = html.slice(html.indexOf('<details'), html.indexOf('</details>')).replaceAll('\u00a0', ' ');
  assert.ok(html.includes('24\u00a0Sep\u00a02025'), 'a date does not break across lines');
  const short = new Map(file.matches.map((match) => [match.date, match.date_short]));
  const on = (played) => oleg.timeline.filter((square) => square.played === played).map((square) => short.get(square.date)).join(', ');
  assert.ok(dates.includes(on(true)));
  assert.ok(dates.includes(on(false)));
});

test('someone who missed nothing has no Missed list', () => {
  const html = profileHtml('s3', 'Berghi');
  const dates = html.slice(html.indexOf('<details'), html.indexOf('</details>'));
  assert.ok(dates.includes(`>${cards.player.strip_key.played}<`));
  assert.ok(!dates.includes(`>${cards.player.strip_key.missed}<`));
});

test('with no profile yet, the cards wait with their headings and no figures', () => {
  const { body } = render(PlayerProfile, { props: { profile: null, matches: [], cards, note: '' } });
  assert.deepEqual(headingOrder(body), ['h2', 'h2']);
  assert.ok(!body.includes('class="tile'));
  assert.ok(!body.includes('squares'));
});

// ---- The pages, in the state they paint before the file arrives ----

test('Roster paints the picker and the card headings, and no row or figure', () => {
  const { body } = render(RosterPage, { props: { seasons: meta.seasons, noMatches: meta.empty_states.no_matches, cards } });
  assert.equal(body.split('data-season-tick').length - 1, meta.seasons.length);
  assert.ok(body.includes(cards.who_turns_up.title));
  assert.ok(body.includes(cards.with_without.title));
  assert.ok(!body.includes('dt-row'));
  assert.ok(!body.includes('class="tile'));
});

test('a player page paints the picker and no figures, and no other player', () => {
  const { body } = render(PlayerPage, { props: { seasons: meta.seasons, player: 'Oleg', noMatches: meta.empty_states.no_matches, cards } });
  assert.equal(body.split('data-season-tick').length - 1, meta.seasons.length);
  assert.ok(!body.includes('class="tile'));
  assert.ok(!body.includes(cards.player.note_after_name));
});

test('a player page finds its own player in a real file, wherever they sort', () => {
  const file = seasonFile('s1-s2-s3');
  const last = file.profiles.at(-1).player;
  assert.notEqual(last, file.profiles[0].player);
  const page = playerPageView({ ticks: ['s1'], failed: false, subset: file }, last);
  assert.equal(page.view, 'ready');
  assert.equal(page.profile.player, last);
});

// ---- The wiring, which rendering cannot reach ----
//
// A server render runs no onMount and fires no events, so these read the source. They fail when
// a page stops using the logic tested above, and say which line to look at.

test('Roster reads the stored player once, and a pick goes through the selection alone', () => {
  const source = read('../src/components/players/RosterPage.svelte');
  assert.match(source, /chooser\.open\(\)/);
  assert.match(source, /chooser\.pick\(row\.player\)/);
  assert.match(source, /rosterView\(subset, selection, sort, showingAll\)/);
  assert.ok(!/writePlayer|setItem|localStorage\.|borulanta\.player/.test(source));
});

test('Roster gives the table its selection and the profile its player', () => {
  const source = read('../src/components/players/RosterPage.svelte');
  assert.match(source, /picked=\{table\.selected\}/);
  assert.match(source, /onpick=\{pick\}/);
  assert.match(source, /foldLabel=/);
  assert.match(source, /profile=\{table\.profile\}/);
});

test('a row tap and a chevron tap do what they should in the table', () => {
  const source = read('../src/components/DataTable.svelte');
  assert.match(source, /if \(onpick && tapPicksRow\(event\.target\)\) onpick\(row\)/);
  assert.match(source, /class="dt-unfold"[^>]*onclick=\{\(\) => toggleFold\(rowKey\(row\)\)\}>/);
});

test('a player page looks up the player it was built for', () => {
  assert.match(read('../src/components/players/PlayerPage.svelte'), /playerPageView\(scope\.state, player\)/);
  assert.match(read('../src/components/players/PlayerPage.svelte'), /profile=\{page\.profile\}/);
  const page = read('../src/pages/players/[slug].astro');
  assert.match(page, /params: \{ slug \}, props: \{ player \}/);
  assert.match(page, /<PlayerPage client:load seasons=\{meta\.seasons\} player=\{player\}/);
  assert.match(page, /<h1>\{player\}<\/h1>/);
  assert.match(page, /<Base title=\{player\} description=\{PLAYERS\.description\(player\)\}/);
});

// ---- The redirects ----

function arriveAt(target, search, hash) {
  const replaced = [];
  vm.runInNewContext(redirectScript(target), { location: { search, hash, replace: (url) => replaced.push(url) } });
  return replaced;
}

test('a redirect keeps the ticks and the hash, and goes where it is told', () => {
  assert.deepEqual(arriveAt('/players/effects/', '?seasons=s1,s3', '#x'), ['/players/effects/?seasons=s1,s3#x']);
  assert.deepEqual(arriveAt('/players/effects/', '', ''), ['/players/effects/']);
});

for (const [section, path] of [['players', '../src/pages/players/index.astro'], ['matches', '../src/pages/matches/index.astro']]) {
  test(`/${section}/ redirects through that script, to the tab Shiny opens on`, () => {
    const source = read(path);
    assert.match(source, /script = redirectScript\(target\)/);
    assert.match(source, new RegExp(`tab\\.id === '${section}'`));
    assert.match(source, /set:html=\{script\}/);
    assert.ok(!source.includes('location.replace'));
  });
}

test('the tabs open where Shiny does', () => {
  assert.equal(NAV.find((tab) => tab.id === 'players').href, '/players/effects/');
  assert.equal(NAV.find((tab) => tab.id === 'matches').href, '/matches/trends/');
});

// ---- The table's styles ----

test('nothing in Who turns up is removed with display: none, which would misplace cells for a screen reader', () => {
  const css = read('../src/styles/data-table.css');
  const removed = [...css.matchAll(/([^{}]*\.dt-roster[^{}]*)\{[^{}]*display:\s*none/g)].map((match) => match[1].trim());
  assert.deepEqual(removed, []);
});
