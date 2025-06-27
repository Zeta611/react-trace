import React from "react";
import print from "./print";

function Button({ onClick, children }) {
  return <button onClick={onClick}>{children}</button>;
}

function C() {
  return (
    <div>
      <Button onClick={() => print("0")}>0</Button>
      <Button onClick={() => print("1")}>1</Button>
    </div>
  );
}

export default function SideEffectEventHandler() {
  return <C />;
}
