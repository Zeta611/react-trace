import React from "react";

function C() {
  return <div>C</div>;
}
function B() {
  return (
    <div>
      B
      <C />
    </div>
  );
}
function A() {
  return (
    <div>
      A
      <B />
    </div>
  );
}

export default function ABC() {
  return <A />;
}
