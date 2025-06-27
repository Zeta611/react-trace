import React from "react";
import { useState, useEffect } from "react";
import print from "./print";

function C() {
  const [s, setS] = useState(() => 42);

  useEffect(() => {
    print("C");

    // These two state updates compose to an identity
    setS(() => 43);
    setS(() => 42);
  });

  return <div>{s}</div>;
}

export default function RenderCycleSetInEffectTwiceStep1() {
  return <C />;
}

