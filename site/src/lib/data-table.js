// The markup of every table on Fees and League. A table returns as a string, so the build
// (set:html) and the browser (innerHTML) draw the same thing from one copy. Styles are in
// src/styles/data-table.css. Inlined by moduleToInline(), so keep it plain.
//
// A column is { key, label, value(row) } and any of:
//   lead    the row's header, and the part you tap to open the row
//   folds   a column of its own when there is room, otherwise under the row when it opens
//   number  right-aligned figures
//   long    takes the full width when it opens under the row
//   cue(row) a word beside the lead, so a row's meaning is not carried by colour alone
//   href(row) makes the value a link, but never inside the part you tap to open the row
//   html(row) markup for the cell, in place of the value, already escaped by whoever writes it
//   hideLabel keeps the heading for a screen reader only
//   unsorted gives it no sort button, in a table that has them
//
// A row that opens is a <details>, so it needs no script at all. Nothing here formats a
// number: value() returns R's own strings. A table a page changes in place, by sorting it or
// picking a row, is src/components/DataTable.svelte, with the same classes and cells.

const ESCAPES = { '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' };

export function escapeHtml(value) {
  if (value === null || value === undefined) return '';
  return String(value).replace(/[&<>"']/g, (character) => ESCAPES[character]);
}

const CHEVRON = '<svg class="dt-chevron" viewBox="0 0 24 24" width="16" height="16" aria-hidden="true" focusable="false"><path d="m9 6 6 6-6 6"/></svg>';

export const cellClass = (column) => `col-${column.key}${column.number ? ' dt-num' : ''}${column.folds ? ' dt-fold' : ''}`;

export function cellHtml(column, row) {
  if (column.html) return column.html(row);
  const text = escapeHtml(column.value(row));
  return column.href ? `<a href="${escapeHtml(column.href(row))}">${text}</a>` : text;
}

// A tap on a row picks it, unless it landed on a control in the row, which has answered it.
export const tapPicksRow = (target) => !target.closest('a, button');

export const headingHtml = (column) => (column.hideLabel ? `<span class="visually-hidden">${escapeHtml(column.label)}</span>` : escapeHtml(column.label));

// A summary is a control, so it cannot hold a link as well: a screen reader finds one inside
// the other, and the keyboard stops twice on each row. A lead that links shows its name as plain
// text there, and opens onto the link. The space before the cue is for a screen reader, which
// otherwise reads "BerghiTo pay".
function leadCell(column, row, folded) {
  const cue = column.cue ? column.cue(row) : '';
  const named = (inner) => `<span class="dt-name">${inner}</span>${cue ? ` <span class="dt-cue">${escapeHtml(cue)}</span>` : ''}`;
  const whole = named(cellHtml(column, row));
  const link = column.href ? `<div class="dt-link"><dt>${escapeHtml(column.label)}</dt><dd>${cellHtml(column, row)}</dd></div>` : '';
  const open = folded.map((other) => `<div${other.long ? ' class="dt-long"' : ''}><dt>${escapeHtml(other.label)}</dt><dd>${cellHtml(other, row)}</dd></div>`).join('');
  const summary = named(column.href ? escapeHtml(column.value(row)) : cellHtml(column, row));
  const inner = folded.length > 0 ? `<details class="dt-more"><summary>${CHEVRON}<span class="dt-label">${summary}</span></summary><dl>${link}${open}</dl></details><span class="dt-whole">${whole}</span>` : whole;
  return `<th scope="row" role="rowheader" class="${cellClass(column)} dt-lead">${inner}</th>`;
}

// Attributes are for a page's script to find its rows. Hidden rows are the page's to show.
function rowHtml(columns, folded, row, index, options) {
  const extra = options.rowClass ? options.rowClass(row) : '';
  const attributes = options.rowAttributes ? options.rowAttributes(row, index) : {};
  const attributeText = Object.entries(attributes).map(([name, value]) => (value === true ? ` ${name}` : ` ${name}="${escapeHtml(value)}"`)).join('');
  const cells = columns.map((column) => (column.lead ? leadCell(column, row, folded) : `<td role="cell" class="${cellClass(column)}">${cellHtml(column, row)}</td>`)).join('');
  return `<tr role="row" class="dt-row${extra ? ` ${extra}` : ''}"${attributeText}>${cells}</tr>`;
}

// The roles look redundant. They keep this a table to a screen reader when a phone's
// layout turns its rows into grids, which can otherwise drop the semantics.
export function tableHtml(options) {
  const { className = '', caption, columns, rows } = options;
  const folded = columns.filter((column) => column.folds);
  const classes = ['dt', folded.length > 0 ? 'dt-folding' : '', className].filter(Boolean).join(' ');
  const head = columns.map((column) => `<th scope="col" role="columnheader" class="${cellClass(column)}">${headingHtml(column)}</th>`).join('');
  const body = rows.map((row, index) => rowHtml(columns, folded, row, index, options)).join('');
  return `<div class="dt-fit"><table role="table" class="${classes}"><caption class="visually-hidden">${escapeHtml(caption)}</caption><thead role="rowgroup"><tr role="row">${head}</tr></thead><tbody role="rowgroup">${body}</tbody></table></div>`;
}

// A long table's rows to show, and the words on the button under it, or null when every row
// already shows. `words` hold {count}: showAll takes the whole count and showFewer the first.
export function moreRowsLabel(count, first, showingAll, words) {
  if (count <= first) return null;
  return showingAll ? words.showFewer.replace('{count}', String(first)) : words.showAll.replace('{count}', String(count));
}
