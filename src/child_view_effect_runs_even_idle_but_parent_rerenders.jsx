import React from "react";
import { useState, useEffect } from "react";
import print from "./print";

function C({ x }) {
  useEffect(() => {
    print("C");
  });

  return <div>C</div>;
}

function D() {
  const [x, setX] = useState(() => 0);

  useEffect(() => {
    setX(() => 42);
  });

  return (
    <div>
      <C x={0} />
    </div>
  );
}

export default function SideEffectChildEffects() {
  return <D />;
}
