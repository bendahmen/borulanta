// The chosen player, restored before first paint so a returning player lands on
// their own balance. Inlined into the page by moduleToInline(), so keep it plain.

const PLAYER_KEY = 'borulanta.player';

// Takes a function because merely touching localStorage can throw when it is blocked.
export function readPlayer(getStorage) {
  try {
    return getStorage().getItem(PLAYER_KEY);
  } catch {
    return null;
  }
}

export function writePlayer(getStorage, player) {
  try {
    getStorage().setItem(PLAYER_KEY, player);
  } catch {
    // Blocked storage only means the choice is not remembered next time.
  }
}

export function mountPlayerPicker(root, getStorage) {
  const select = root.querySelector('select');
  // Each slot names the data attribute on the option that fills it, so everything shown
  // for a player belongs to that player's option and cannot outlast the choice.
  const slots = root.querySelectorAll('[data-fee]');

  const show = () => {
    const chosen = select.options[select.selectedIndex];
    const picked = Boolean(chosen && chosen.value);
    root.classList.toggle('has-player', picked);
    root.classList.toggle('is-owed', picked && chosen.dataset.owed === '1');
    slots.forEach((slot) => {
      slot.textContent = picked ? chosen.dataset[slot.dataset.fee] : '';
    });
  };

  // A saved name no longer on the roster leaves the picker on its placeholder.
  const restore = () => {
    const saved = readPlayer(getStorage);
    if (saved && Array.from(select.options).some((option) => option.value === saved)) {
      select.value = saved;
    }
    show();
  };

  select.addEventListener('change', () => {
    writePlayer(getStorage, select.value);
    show();
  });
  addEventListener('pageshow', (event) => {
    if (event.persisted) restore();
  });
  restore();
}
