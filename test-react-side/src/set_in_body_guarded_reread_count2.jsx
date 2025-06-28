import React from "react";
import { useState, useEffect } from "react";
import print from "./print";

function C() {
  print("C");
  const [s, setS] = useState(() => 0);
  if (s < 25) {
    setS((s) => s + 1);
  }
  return <div></div>;
}

export default function RenderCycleSetInBodyGuarded2() {
  return <C />;
}
