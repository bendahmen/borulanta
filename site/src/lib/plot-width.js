// Tells a plot its container's width, once now and then whenever the width changes. A
// phone's address bar sliding away changes the height only, so it causes no redraw.
export function observeWidth(element, onWidth) {
  let last = element.clientWidth;
  let frame = 0;
  if (last > 0) onWidth(last);
  const observer = new ResizeObserver(() => {
    cancelAnimationFrame(frame);
    frame = requestAnimationFrame(() => {
      const width = element.clientWidth;
      if (width > 0 && width !== last) {
        last = width;
        onWidth(width);
      }
    });
  });
  observer.observe(element);
  return () => {
    cancelAnimationFrame(frame);
    observer.disconnect();
  };
}
