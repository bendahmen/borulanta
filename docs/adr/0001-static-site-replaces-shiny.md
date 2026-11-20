# A static Astro site replaces Shiny, with no backend

Most visitors open a WhatsApp link on a phone to see who is next, what happened and what they owe, and none of that needs a server. So in November 2026 we replaced the Shiny app on Posit Connect Cloud with a static Astro site on Cloudflare, with Svelte for the parts that change on the page and Observable Plot for the charts. Each week R recomputes everything on the Mac and writes JSON, which is committed, and Cloudflare builds the pages from it, with no logins, database, functions or writes.

## Considered options

Quarto, Django and Supabase were rejected. So was webR, which would run R in the browser, because its smallest useful setup downloads about 29 MB.

## Consequences

The site is only as fresh as the last publish, so which fixtures are still to come is worked out in the browser (see ADR 0002). Every combination of seasons is precomputed as its own file, and the count doubles with each season: around season 10, about 2030, it would be limited to consecutive seasons. The site and the repository stay public, with `noindex` and a disallowing `robots.txt`. Cloudflare Access was rejected on 18 November 2026, because it would hide nothing the public repository does not show and would put a login in front of every WhatsApp link.
