import React from "react";
import { useState, useEffect } from "react";
import print from "./print";

function C() {
  const [s, setS] = useState(() => 42);

  useEffect(() => {
    print("");

    if (s <= 45) {
      setS((s) => s + 1);
    }
  });

  return <div></div>;
}

export default function RenderCycleSetStateGuarded() {
  return <C />;
}
