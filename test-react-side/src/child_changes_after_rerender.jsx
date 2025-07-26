import React from "react";
import { useState, useEffect } from "react";
import print from "./print";

function Child1() {
  useEffect(() => {
    print(1);
  });

  return 1;
}

function Child2() {
  useEffect(() => {
    print(2);
  });

  return 2;
}

function Parent() {
  const [s, setS] = useState(true);
  useEffect(() => {
    print("P");
    if (s) {
      setS(() => false);
    }
  });

  return s ? <Child1 /> : <Child2 />;
}

export default function ChildChangesAfterRerender() {
  return <Parent />;
}
