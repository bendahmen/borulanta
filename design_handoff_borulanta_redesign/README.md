# Handoff: Borulanta Redesign

## Overview
A visual refresh of the Borulanta football-team tracker (an R Shiny app
built with `bslib`/`DT`) covering the Fees, Matches, and Attendance tabs.
New palette (navy blue + green, in the spirit of the user's team colors),
condensed sporty type, a proper crest badge, clearer fee-balance states,
win/draw/loss badges on match results, and an attendance leaderboard.

## About the design files
The file in `design_reference/Borulanta.dc.html` is a **design reference
built in HTML** — a prototype showing the intended look, layout, and
interaction, populated with the app's real CSV data so you can check the
fee math against it. **It is not code to drop into the Shiny app.** The
target codebase is an R Shiny app (`app.R` + `bslib` theme +
`www/borulanta.css`), and the real implementation work is in that stack.

Because the existing app already separates structure (R/`bslib`) from
style (`www/borulanta.css`) and reuses a small, consistent set of CSS
class names, this handoff is mostly a **drop-in CSS replacement** plus a
handful of small, well-isolated R edits — see `app.R.patch-notes.md` for
the exact diffs.

## Fidelity
**High-fidelity.** Colors, typography, spacing, and copy in the design
reference are final — recreate them as closely as the Shiny/bslib/DT
stack allows.

## Files in this bundle
- `borulanta.css` — full replacement for `www/borulanta.css`. Same class
  names as the current file (`app-hero`, `hero-title`, `nav-pill-shell`,
  `panel-card`, `payment-card`, `table-card`, `fee-amount`, etc.) — only
  colors/type/surfaces changed, plus a few new classes noted below.
- `app.R.patch-notes.md` — every R-side change needed: theme colors,
  crest markup, conditional fee-balance color, match result badges,
  attendance sort/rank highlight. Each is a small, copy-pasteable snippet.
- `design_reference/Borulanta.dc.html` — the interactive HTML mock, real
  data wired up, open directly in a browser for visual reference.
- `design_reference/borulanta-crest.png` — the team crest asset; copy to
  `www/borulanta-crest.png`.
- `screenshots/` — rendered screenshots of the three tabs (Fees, Matches,
  Attendance) for quick visual reference without opening the HTML file.

## Design tokens

**Colors** (written as `oklch()`, valid CSS — supported by all current
browsers; use as-is or convert to hex if the toolchain needs it):
- Background: `oklch(97.3% 0.007 150)` (near-white, faint cool tint)
- Surface (cards): `#ffffff`
- Ink (text): `oklch(20% 0.02 210)`
- Ink muted: `oklch(40% 0.02 210 / 0.75)`
- Navy (hero/active-tab gradient): `oklch(24% 0.07 258)` → `oklch(19% 0.05 250)` → `oklch(27% 0.09 200)`
- Blue (section labels, draw badge, table hover): `oklch(45% 0.16 258)`
- Green (win badge, settled/credit balance, Monzo button): `oklch(46% 0.15 148)` / bright variant `oklch(72% 0.15 148)`
- Amber (owed balance, loss badge): `oklch(52% 0.15 45)`
- Border: `oklch(92% 0.01 150)`

**Typography**
- Headings: Barlow Condensed, weight 800, uppercase, tight letter-spacing
- Body: Barlow, weights 400–700
- Google Fonts import: `Barlow+Condensed:wght@600;700;800` and `Barlow:wght@400;500;600;700`

**Radii / shadows**
- Cards: 22–24px radius, `0 10px 28px rgba(16,34,29,0.08)`
- Hero/payment card: 28px radius, `0 16px 40px rgba(0,0,0,0.28)`
- Pills/buttons: fully rounded (999px) or 13–14px for rectangular buttons

## Screens

### 1. Hero
Navy gradient band, full width, rounded 28px corners. Circular white
crest badge (92–104px) on the left, team name in large condensed
uppercase type, a green kicker label above it ("Wednesday Football"),
and a one-line description below. See `app.R.patch-notes.md` §2.

### 2. Tab nav
White rounded pill container holding 3 segmented tabs (Fees / Matches /
Attendance). Selected tab uses the same navy gradient as the hero — not
a standalone blue button — so the palette reads as one system.

### 3. Fees tab
Two cards side by side (stack on narrow screens): a white "Fee Check"
card with player selector and a large balance figure (green = settled/
credit, amber = owed — see patch notes §3), and a navy "Settle Up" card
with the Monzo payment button (bright green) and bank transfer details.

### 4. Matches tab
A single card listing matches most-recent-first: date, score, and a
colored WIN/DRAW/LOSS pill (green/blue/amber). See patch notes §4 for
adding the badge column to the existing DT table.

### 5. Attendance tab
Leaderboard-style table: rank, player, attendance %, average points,
average goals for–against. Sorted by attendance rate descending by
default; **column headers are already clickable to re-sort** — DT
supports this natively, no new code required, just restyled via CSS
(patch notes §5). Top 3 rows get a subtle green tint.

## Interactions & behavior
- Tab switching: standard Shiny `navset_pill` — no changes needed.
- Attendance/Matches table column sort: native DT click-to-sort
  (`ordering` isn't disabled on the attendance table) — just needs the
  new CSS for the cursor/caret styling.
- Fee balance color: needs the small conditional-class change in
  `output$fees_owed` (patch notes §3).
- Hover states: buttons/rows darken slightly (`filter: brightness(0.95)`
  on the Monzo button, light blue tint on table row hover) — all in CSS.

## Assets
- `borulanta-crest.png` — user-provided crest artwork (Britannia figure,
  trident, Union Jack shield, "BORULANTA · FOOTBALL CLUB" ring text).
  Copy directly into `www/`.

## Next steps for the developer
1. Copy `borulanta.css` over `www/borulanta.css`.
2. Copy `design_reference/borulanta-crest.png` to `www/borulanta-crest.png`.
3. Apply the R snippets in `app.R.patch-notes.md` (theme colors are
   required; crest markup, fee-color class, match badges, and rank
   highlight are recommended but optional polish).
4. Run the app and compare against `design_reference/Borulanta.dc.html`
   open in a browser tab side by side.
