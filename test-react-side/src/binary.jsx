import React from "react";

function C({ n }) {
  if (n <= 0) {
    return <div>0</div>;
  }
  return (
    <div>
      <C n={n - 1} />
      <C n={n - 1} />
    </div>
  );
}

export default function Binary() {
  return <C n={3} />;
}
