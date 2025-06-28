import React from "react";
import { useState, useEffect } from "react";
import print from "./print";

function C() {
  const [s, setS] = useState(() => 42);

  useEffect(() => {
    print("");
  });

  return <div></div>;
}

export default function RenderCycleSetInEffectStep() {
  return <C />;
}
