import React from "react";
import { useState, useEffect } from "react";
import print from "./print";

function C({ x }) {
  const [s, setS] = useState(() => 42);

  useEffect(() => {
    print("");

    if (s !== x) {
      setS(() => x);
    }
  });

  return <div>{s}</div>;
}

export default function RenderCycleSetInEffectWithArgStep2() {
  return <C x={0} />;
}
