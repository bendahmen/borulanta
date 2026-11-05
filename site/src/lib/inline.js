// Turns a module's raw source into something a classic inline script can run, so
// the page and the unit tests share one copy of the logic.

export function moduleToInline(source) {
  return source
    .replace(/^\s*\/\/.*$/gm, '')
    .replace(/^export /gm, '')
    .replace(/^\s+/gm, '')
    .trim();
}

// For JSON placed inside a script tag: a name scraped from the league page could
// otherwise close the tag.
export function scriptSafeJson(value) {
  return JSON.stringify(value).replace(/</g, '\\u003c');
}
