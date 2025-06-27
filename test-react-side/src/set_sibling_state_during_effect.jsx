import React from "react";
import { useState, useEffect } from "react";
import print from "./print";

function D({ setF }) {
  const [s, setS] = useState(() => 0);

  useEffect(() => {
    setF(() => setS);
  });

  useEffect(() => {
    print("D");
  });

  return <div></div>;
}

function E({ setS }) {
  useEffect(() => {
    setS(() => 42);
  });

  return <div></div>;
}

function C() {
  const [f, setF] = useState(() => null);

  return (
    f === null ? (
      <div>
        <E setS={() => {}} />
        <D setF={setF} />
      </div>
    ) : (
      <div>
        <E setS={f} />
        <D setF={() => {}} />
      </div>
    )
  );
}

export default function RenderCycleSiblingSetter() {
  return <C />;
}

