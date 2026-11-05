// A section's address that sends you on to the view Shiny opens on, keeping the ticks in the URL.
export const redirectScript = (target) => `location.replace(${JSON.stringify(target)}+location.search+location.hash);`;
