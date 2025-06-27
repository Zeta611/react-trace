import React from "react";
import { useState, useEffect } from "react";
import print from "./print";

function C() {
  const [s, setS] = useState(() => 42);
  if (s === 42) {
    setS((s) => 43);
  }
  useEffect(() => {
    print("");
  });
  return <div></div>;
}

export default function RenderCycleSetInBodyGuarded() {
  return <C />;
}

