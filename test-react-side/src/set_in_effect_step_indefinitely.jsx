import React from "react";
import { useState, useEffect } from "react";
import { ErrorBoundary } from "react-error-boundary";
import print from "./print";

// Add a counter to limit the number of renders and prevent infinite loops
let renderCount = 0;
const MAX_FUEL = 100;

function C() {
  const [s, setS] = useState(() => 42);

  useEffect(() => {
    print("C");

    // Only increment state if we haven't reached the maximum render count
    renderCount++;
    if (renderCount < MAX_FUEL) {
      setS((s) => s + 1);
    } else {
      print(`Reached MAX_FUEL (${MAX_FUEL})`);
    }
  });

  return <div></div>;
}

function Error() {
  return <div>Error</div>;
}

// Reset the counter before rendering
renderCount = 0;

export default function RenderCycleSetInEffectIndefinite() {
  return (
    <ErrorBoundary fallback={<Error />}>
      <C />
    </ErrorBoundary>
  );
}
