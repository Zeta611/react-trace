import React from "react";
import { useState, useEffect } from "react";
import { ErrorBoundary } from "react-error-boundary";
import print from "./print";

// Add a counter to limit the number of renders and prevent infinite loops
let renderCount = 0;
const MAX_FUEL = 100;

function C({ setS }) {
  useEffect(() => {
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

function D() {
  const [s, setS] = useState(() => 42);

  return (
    <div>
      <C setS={setS} />
    </div>
  );
}

function Error() {
  return <div>Error</div>;
}

// Reset the counter before rendering
renderCount = 0;

export default function RenderCycleSetPassedIndefinite() {
  return (
    <ErrorBoundary fallback={<Error />}>
      <D />
    </ErrorBoundary>
  );
}

