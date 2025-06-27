import React from "react";
import { useState, useEffect } from "react";
import print from "./print";

function C() {
  const [s, setS] = useState(() => 1);
  if (s > 3) {
    setS(() => 3);
  }
  if (s < 1) {
    setS(() => 1);
  }
  useEffect(() => {
    print(s);
  });
  const increment = () => {
    setS((s) => s + 1);
  };
  const decrement = () => {
    setS((s) => s - 1);
  };
  return (
    <div>
      <button onClick={increment}>+</button>
      <button onClick={decrement}>-</button>
      <div>{s}</div>
    </div>
  );
}

export default function EventCounter() {
  return <C />;
}
