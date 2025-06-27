import React from "react";
import { useState, useEffect } from "react";
import print from "./print";

function C() {
  const [setter, setSetter] = useState(() => null);
  const [render, setRender] = useState(() => 0);
  if (render < 3) {
    setRender((r) => r + 1);
  }

  if (setter !== null && render < 3) {
    setter(() => 1);
  }
  const [value, setValue] = useState(() => 0);
  if (setter === null && render < 3) {
    setSetter(() => setValue);
  }

  print(render, value);

  return <div>{value}</div>;
}

export default function RenderCycleSetStateBeforeBind() {
  return <C />;
}

