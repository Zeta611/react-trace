import React from "react";
import { useState, useEffect } from "react";
import print from "./print";

function C() {
  const [s, setS] = useState(() => 42);

  useEffect(() => {
    print("C");

    if (s === 42) {
      setS(() => 43);
    }
  });

  return <div></div>;
}

export default function RenderCycleSetInEffectGuardedStep2() {
  return <C />;
}

