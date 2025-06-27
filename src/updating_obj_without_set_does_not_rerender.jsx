import React from "react";
import { useState, useEffect } from "react";
import print from "./print";

function C() {
  const [s, setS] = useState({ x: 42 });
  useEffect(() => {
    print("C");
    s.x = 43;
  });

  return <div></div>;
}

export default function RenderCycleUpdateObjNoRerender() {
  return <C />;
}

