# A static Astro site replaces Shiny, with no backend

Most visitors arrive on a phone from a WhatsApp link, wanting to know who is next, what happened and what they owe, and nothing they do needs a server. So in November 2026 we replaced the Shiny app on Posit Connect Cloud with a static Astro site on Cloudflare, because design and usability on a phone mattered most. Svelte draws only the parts that change on the page, and Observable Plot draws the charts in the browser. Each week R recomputes everything on the Mac and writes JSON, which is committed, and Cloudflare builds the pages from it. There are no logins, database, functions or writes.

## Considered options

Quarto, Django and Supabase were rejected in the same debate. webR, which would have run R in the browser, was rejected because its smallest useful setup downloads about 29 MB.

## Consequences

- The site is only as fresh as the last publish. Which fixture is next would freeze at build time, so it is the one piece of date logic worked out in the browser (see ADR 0002).
- Every combination of seasons is precomputed as its own file. The count doubles with each season, and around season 10, about 2030, it would be limited to consecutive seasons.
- The site and the repository are public. Pages carry `noindex` and `robots.txt` disallows everything. Cloudflare Access was weighed and rejected on 18 November 2026: while the repository is public it would hide nothing the repository does not already show, and it would put a login in front of every WhatsApp link.
