import React from "react";
import { useState, useEffect } from "react";
import print from "./print";

function C() {
  const [s, setS] = useState(() => 0);

  useEffect(() => {
    print("");

    if (s < 25) {
      setS((s) => s + 1);
    }
  });

  return <div role="counter">{s}</div>;
}

export default function Rerender() {
  return <C />;
}
