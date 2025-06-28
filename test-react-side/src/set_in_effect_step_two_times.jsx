import React from "react";
import { useState, useEffect } from "react";
import print from "./print";

function C() {
  const [s, setS] = useState(() => 42);

  useEffect(() => {
    print("");

    setS(() => 43);
  });

  return <div></div>;
}

export default function RenderCycleSetInEffectStep2() {
  return <C />;
}
