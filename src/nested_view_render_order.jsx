import React from "react";
import { useState, useEffect } from "react";
import print from "./print";

function C({ x }) {
  useEffect(() => {
    print(x);
  });

  return x;
}

function D() {
  const [x, setX] = useState(() => 0);

  useEffect(() => {
    setX(() => 42);
  });

  useEffect(() => {
    print("D");
  });

  return (
    <div>
      <C x="0" />
      <div>
        <C x="1" />
        <C x="2" />
      </div>
    </div>
  );
}

function E() {
  useEffect(() => {
    print("E");
  });

  return (
    <div>
      <D />
      <C x="3" />
    </div>
  );
}

export default function SideEffectNestedViewOrder() {
  return <E />;
}
