import React from "react";
import print from "./print";

function C() {
  const handleClick = () => {
    print("B");
  };

  return <button onClick={handleClick}>button</button>;
}

export default function Button() {
  return <C />;
}
