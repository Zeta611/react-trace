import React from "react";
import { useState, useEffect } from "react";
import print from "./print";

function C() {
  const [s, setS] = useState(() => 42);
  useEffect(() => {
    setS(0);
  });
  return <div></div>;
}

function D() {
  const [s, setS] = useState(() => true);
  useEffect(() => {
    print("");
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
      </div>
    );
  }
}

export default function ComponentStatePersistence() {
  return <D />;
}
