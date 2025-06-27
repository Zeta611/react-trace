import React from "react";
import { render } from "@testing-library/react";
import "@testing-library/jest-dom";
import Binary from "./binary";

test("Binary renders a binary tree structure", () => {
  const { container } = render(<Binary />);

  const zeros = container.textContent.match(/0/g);
  expect(zeros).toHaveLength(8);
});
