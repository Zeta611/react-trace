import React from "react";
import { useState, useEffect } from "react";
import print from "./print";

function C() {
  const [s, setS] = useState(() => 42);

  useEffect(() => {
    setS(() => 0);
    print("C");
  });

  return <div>{s}</div>;
}

function D() {
  const [s, setS] = useState(() => true);

  useEffect(() => {
    setS(() => false);
  });

  if (s) {
    return (
      <div>
        <C />
      </div>
    );
  } else {
    return (
      <div>
        <C />
        <C />
      </div>
    );
  }
}

export default function ComponentCreateNew() {
  return <D />;
}
