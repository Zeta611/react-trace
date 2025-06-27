import React from "react";

function C({ n }) {
  if (n <= 0) {
    return <div>0</div>;
  }
  return (
    <div>
      {n}
      <C n={n - 1} />
    </div>
  );
}

export default function Chain() {
  return <C n={5} />;
}
