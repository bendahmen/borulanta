import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { test } from 'node:test';

import { cellHtml, escapeHtml, headingHtml, moreRowsLabel, tableHtml } from '../src/lib/data-table.js';
import { moduleToInline } from '../src/lib/inline.js';
import { leagueColumns, runInColumns } from '../src/lib/league-table.js';

const read = (path) => JSON.parse(readFileSync(new URL(path, import.meta.url)));
const count = (html, pattern) => (html.match(pattern) ?? []).length;

const columns = [
  { key: 'name', label: 'Name', value: (row) => row.name, lead: true },
  { key: 'more', label: 'More', value: (row) => row.more, folds: true, number: true },
  { key: 'text', label: 'Text', value: (row) => row.text, folds: true, long: true },
  { key: 'kept', label: 'Kept', value: (row) => row.kept, number: true },
];
const rows = [
  { name: 'A', more: 1, text: 'one', kept: '£1.00' },
  { name: 'B', more: 2, text: 'two', kept: '£2.00' },
];

test('escapeHtml handles markup, and null is an empty cell', () => {
  assert.equal(escapeHtml(`<b>&"'`), '&lt;b&gt;&amp;&quot;&#39;');
  assert.equal(escapeHtml(null), '');
  assert.equal(escapeHtml(undefined), '');
  assert.equal(escapeHtml(0), '0');
});

test('nothing from a row or a heading can write markup', () => {
  const html = tableHtml({
    caption: '</caption>',
    columns: [
      { key: 'a', label: '<i>', value: (row) => row.a, lead: true },
      { key: 'b', label: 'b', value: (row) => row.a, folds: true },
    ],
    rows: [{ a: '</td><script>alert(1)</script>' }],
  });
  assert.doesNotMatch(html, /<script/);
  assert.doesNotMatch(html, /<i>/);
  assert.doesNotMatch(html, /<\/caption><\/caption>/);
});

test('a row keeps every column twice over: its own cell, and under the row when it opens', () => {
  const html = tableHtml({ caption: 'c', columns, rows });
  assert.equal(count(html, /<tr role="row" class="dt-row"/g), 2);
  assert.equal(count(html, /<td role="cell" class="col-more dt-num dt-fold">/g), 2);
  assert.equal(count(html, /<td role="cell" class="col-kept dt-num">/g), 2);
  assert.equal(count(html, /<details class="dt-more">/g), 2);
  assert.match(html, /<dt>More<\/dt><dd>1<\/dd>/);
  assert.match(html, /<div class="dt-long"><dt>Text<\/dt><dd>two<\/dd><\/div>/);
  assert.doesNotMatch(html, /<dt>Kept<\/dt>/);
  assert.match(html, /<th scope="col" role="columnheader" class="col-kept dt-num">Kept<\/th>/);
});

test('a table with nothing to fold has no details and no folding class', () => {
  const html = tableHtml({ caption: 'c', columns: [columns[0], columns[3]], rows, className: 'x' });
  assert.match(html, /<table role="table" class="dt x">/);
  assert.doesNotMatch(html, /<details/);
});

test('row classes, row attributes and the cue reach the row', () => {
  const html = tableHtml({
    caption: 'c',
    columns: [{ ...columns[0], cue: (row) => (row.name === 'A' ? 'To pay' : '') }, columns[1]],
    rows,
    rowClass: (row) => (row.name === 'A' ? 'is-owed' : ''),
    rowAttributes: (row, index) => (row.name === 'B' ? { 'data-date': `d"${index}`, hidden: true } : { 'data-date': `d"${index}` }),
  });
  assert.match(html, /class="dt-row is-owed" data-date="d&quot;0">/);
  assert.match(html, /class="dt-row" data-date="d&quot;1" hidden>/);
  assert.equal(count(html, /class="dt-cue">To pay</g), 2);
  // Both places the name is drawn, or a screen reader says "ATo pay".
  assert.equal(count(html, /<span class="dt-name">A<\/span> <span class="dt-cue">/g), 2);
});

test('the league table draws every figure R exported, and picks out only our row', () => {
  const league = read('../src/generated/league.json');
  const wording = read('../src/generated/wording.json');
  const { our_team: ourTeam } = read('../src/generated/meta.json');
  const html = tableHtml({
    caption: league.title,
    columns: leagueColumns(wording.league_cards.headings),
    rows: league.rows,
    rowClass: (row) => (row.team === ourTeam ? 'is-us' : ''),
  });
  for (const heading of ['Pos', 'Team', 'P', 'W', 'D', 'L', 'GD', 'Pts']) {
    assert.match(html, new RegExp(`>${heading}</th>`));
  }
  for (const row of league.rows) {
    const [cells] = html.split('</tr>').filter((part) => part.includes(`>${escapeHtml(row.team)}<`));
    const expected = [row.position, row.played, row.won, row.drawn, row.lost, row.goal_difference, row.points];
    const shown = [...cells.matchAll(/<td role="cell"[^>]*>([^<]*)<\/td>/g)].map((match) => match[1]);
    assert.deepEqual(shown, expected.map(String), row.team);
  }
  assert.equal(count(html, /is-us/g), league.rows.filter((row) => row.team === ourTeam).length);
});

test('an opponent the standings do not carry keeps its row with the league column empty', () => {
  const wording = read('../src/generated/wording.json');
  const html = tableHtml({
    caption: 'c',
    columns: runInColumns(wording.league_cards.run_in.headings),
    rows: [{ date_short: '11 Nov 2026', opponent: 'Newcomers FC', position: null }],
  });
  assert.match(html, /Newcomers FC/);
  assert.match(html, /<td role="cell" class="col-position dt-num"><\/td>/);
});

test('moduleToInline drops imports and exports, and the table source can sit in a script tag', () => {
  const source = readFileSync(new URL('../src/lib/data-table.js', import.meta.url), 'utf8');
  assert.doesNotMatch(moduleToInline(source), /^(import|export) /m);
  assert.doesNotMatch(source, /<\/script/i);
  assert.equal(moduleToInline("import { a } from './a.js';\nexport const b = 1;\n"), 'const b = 1;');
});

test('a value with an href is a link, and both are escaped', () => {
  const column = { value: (row) => row.name, href: (row) => `/p/${row.slug}/` };
  assert.equal(cellHtml(column, { name: 'A&B', slug: '"x"' }), '<a href="/p/&quot;x&quot;/">A&amp;B</a>');
});

test('html stands in for the value, as written', () => {
  const column = { value: () => 'x', html: () => '<span>y</span>' };
  assert.equal(cellHtml(column, {}), '<span>y</span>');
});

test('a hidden heading is still there for a screen reader', () => {
  assert.equal(headingHtml({ label: 'Page', hideLabel: true }), '<span class="visually-hidden">Page</span>');
  assert.equal(headingHtml({ label: 'A&B' }), 'A&amp;B');
  const html = tableHtml({ caption: 'c', rows: [], columns: [{ key: 'k', label: 'Page', hideLabel: true, value: () => '' }] });
  assert.ok(html.includes('<span class="visually-hidden">Page</span></th>'));
});

test('the button under a long table', () => {
  const words = { showAll: 'Show all {count}', showFewer: 'Show the first {count}' };
  assert.equal(moreRowsLabel(10, 10, false, words), null);
  assert.equal(moreRowsLabel(11, 10, false, words), 'Show all 11');
  assert.equal(moreRowsLabel(11, 10, true, words), 'Show the first 10');
});

test('a lead that links keeps the link out of the part you tap to open the row', () => {
  const linked = [
    { key: 'player', label: 'Player', value: (row) => row.name, lead: true, href: (row) => `/p/${row.slug}/` },
    { key: 'more', label: 'More', value: (row) => row.more, folds: true },
  ];
  const html = tableHtml({ caption: 'c', columns: linked, rows: [{ name: 'Berghi', slug: 'berghi', more: 1 }] });
  const summary = html.match(/<summary>.*<\/summary>/)[0];
  assert.ok(summary.includes('Berghi'));
  assert.ok(!summary.includes('<a '));
  const panel = html.match(/<dl>.*<\/dl>/)[0];
  assert.ok(panel.includes('<dt>Player</dt><dd><a href="/p/berghi/">Berghi</a></dd>'));
  const whole = html.match(/class="dt-whole">.*?<\/span><\/span>/)[0];
  assert.ok(whole.includes('<a href="/p/berghi/">Berghi</a>'));
});

test('a lead that links, in a table that never folds, is just the link', () => {
  const html = tableHtml({ caption: 'c', rows: [{ name: 'A' }], columns: [{ key: 'player', label: 'P', value: (row) => row.name, lead: true, href: () => '/p/a/' }] });
  assert.ok(!html.includes('<details'));
  assert.ok(html.includes('<a href="/p/a/">A</a>'));
});
