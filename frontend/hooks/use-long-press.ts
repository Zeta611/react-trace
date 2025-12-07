import { useRef, useCallback } from "react";

export function useLongPress(
  callback: () => void,
  options: {
    delay?: number; // Initial delay before repeat starts (ms)
    interval?: number; // Repeat interval (ms)
  } = {}
) {
  const { delay = 500, interval = 1000 } = options;
  const timeoutRef = useRef<ReturnType<typeof setTimeout> | null>(null);
  const intervalRef = useRef<ReturnType<typeof setInterval> | null>(null);

  const start = useCallback(() => {
    callback(); // Fire immediately
    timeoutRef.current = setTimeout(() => {
      callback(); // Fire again when delay ends
      intervalRef.current = setInterval(callback, interval);
    }, delay);
  }, [callback, delay, interval]);

  const stop = useCallback(() => {
    if (timeoutRef.current) clearTimeout(timeoutRef.current);
    if (intervalRef.current) clearInterval(intervalRef.current);
    timeoutRef.current = null;
    intervalRef.current = null;
  }, []);

  return {
    onMouseDown: start,
    onMouseUp: stop,
    onMouseLeave: stop,
    onTouchStart: start,
    onTouchEnd: stop,
  };
}
