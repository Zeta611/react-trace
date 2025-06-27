import React from "react";
import { useState, useEffect } from "react";
import print from "./print";

function Child() {
  useEffect(() => {
    print("C");
  });

  return <div>C</div>;
}

function Parent() {
  const [s, setS] = useState(() => 0);

  useEffect(() => {
    print("P");

    if (s < 10) {
      setS((s) => s + 1);
    }
  });

  return (
    <div>
      <div>{s}</div>
      <Child />
    </div>
  );
}

export default function ParentChild() {
  return <Parent />;
}
