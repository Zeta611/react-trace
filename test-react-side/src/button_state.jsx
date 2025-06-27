import React from "react";
import { useState } from "react";
import print from "./print";

function C() {
  print("C");
  const [s, setS] = useState(0);

  const handleClick = () => {
    print("B");
    setS((s) => {
      print(s);
      return s + 1;
    });
  };

  return (
    <div>
      <button onClick={handleClick}>button</button>
      <div>{s}</div>
    </div>
  );
}

export default function ButtonState() {
  return <C />;
}
