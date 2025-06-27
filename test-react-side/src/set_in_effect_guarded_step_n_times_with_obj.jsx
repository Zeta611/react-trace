import React from "react";
import { useState, useEffect } from "react";
import print from "./print";

function C() {
  const [s, setS] = useState({ x: 42 });

  useEffect(() => {
    print("C");

    if (s.x <= 45) {
      setS(() => ({ x: s.x + 1 }));
    }
  });

  return <div>{s.x}</div>;
}

export default function RenderCycleSetStateGuardedWithObj() {
  return <C />;
}
