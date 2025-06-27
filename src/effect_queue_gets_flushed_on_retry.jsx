import React from "react";
import { useState, useEffect } from "react";
import print from "./print";

function C() {
  const [s, setS] = useState(() => 0);

  print("C");

  if (s === 0) {
    setS((s) => s + 1);
  }

  useEffect(() => {
    print("useEffect");
    setS(() => 42);
  });

  return <div>{s}</div>;
}

export default function SideEffectFlushQueue() {
  return <C />;
}
