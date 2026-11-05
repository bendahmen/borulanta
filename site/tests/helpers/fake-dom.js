// Hand-written stand-ins for the few parts of the DOM the mount functions touch. Each one
// holds only what those functions read or write, so a new use of the DOM fails loudly.

export function fakeElement(props = {}) {
  const listeners = new Map();
  return {
    hidden: false,
    innerHTML: '',
    textContent: '',
    dataset: {},
    addEventListener(type, listener) {
      if (!listeners.has(type)) listeners.set(type, []);
      listeners.get(type).push(listener);
    },
    fire(type, event = {}) {
      for (const listener of listeners.get(type) ?? []) listener(event);
    },
    replaceChildren() {
      this.innerHTML = '';
      this.textContent = '';
    },
    ...props,
  };
}

// The global addEventListener and document, which Node does not have, gone again when the
// test ends. `focus` here keeps the browser's rule that a hidden element cannot take focus.
export function fakeWindow(t) {
  const win = fakeElement();
  const doc = fakeElement({ hidden: false, activeElement: null });
  globalThis.addEventListener = win.addEventListener;
  globalThis.document = doc;
  t.after(() => {
    delete globalThis.addEventListener;
    delete globalThis.document;
  });
  const focusable = (props = {}) => {
    const element = fakeElement(props);
    element.focus = () => {
      if (!element.hidden) doc.activeElement = element;
    };
    return element;
  };
  return { win, doc, focusable };
}

// A select: its value and selectedIndex stay in step, as a real one's do.
export function fakeSelect(options) {
  const select = fakeElement({ options, selectedIndex: 0 });
  Object.defineProperty(select, 'value', {
    get: () => options[select.selectedIndex]?.value ?? '',
    set: (value) => {
      select.selectedIndex = options.findIndex((option) => option.value === value);
    },
  });
  select.choose = (value) => {
    select.value = value;
    select.fire('change');
  };
  return select;
}

export function fakeClassList() {
  const names = new Set();
  return {
    toggle: (name, on) => (on ? names.add(name) : names.delete(name)),
    contains: (name) => names.has(name),
  };
}

// Waits out everything already answered: the loader's promises take several turns.
export const settle = () => new Promise((resolve) => setImmediate(resolve));
