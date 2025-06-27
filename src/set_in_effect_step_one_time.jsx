import React from "react";
import { useState, useEffect } from "react";
import print from "./print";

function C() {
  const [s, setS] = useState(() => 42);

  useEffect(() => {
    print("C");

    setS(() => 42);
  });

  return <div></div>;
}

export default function RenderCycleSetInEffectStep1() {
  return <C />;
}

