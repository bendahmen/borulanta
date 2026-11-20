# R formats values, the site writes the words

R exports each value, and beside each figure the site shows rounded or as money, a `*_display` string from a small formatter in R, so the site sorts on the value and shows R's string. Every word on the page belongs to the site, which let Shiny's presentation code go without the export depending on it. Decided on 18 November 2026 in issue 6.

## Consequences

Three exceptions follow. First, dates are exported as ISO strings and formatted by `site/src/lib/dates.js`, by hand and without `Intl`, because Node's en-GB gives `Wed, 09 Sept 2026` where R gives `Wed 09 Sep 2026`. Second, R keeps two kinds of text because they are data: the `validate_app_data()` messages, which the export's stop rule matches on (ADR 0004), and the charge explanations, which closed seasons' ledgers already store. Finally, the Turnout column in the tables is rounded in JS by `site/src/lib/turnout.js`, as the tables always rounded it, so a turnout of 12.5% shows as 13% in a row and 12% in R's headline tile. Ben kept that difference on 5 November 2026.
