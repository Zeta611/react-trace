import React from "react";
import { useState, useEffect } from "react";
import print from "./print";

function C({ setS }) {
  useEffect(() => {
    setS(() => 0);
    print("");
  });

  return <div></div>;
}

function D() {
  const [s, setS] = useState(() => 42);

  return (
    <div>
      <C setS={setS} />
    </div>
  );
}

export default function RenderCycleSetPassedStep2() {
  return <D />;
}
