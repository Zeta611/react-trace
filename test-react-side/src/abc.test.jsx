import React from "react";
import { render } from "@testing-library/react";
import "@testing-library/jest-dom";
import ABC from "./abc";

test("ABC has A, B, and C", async () => {
  const { container } = render(<ABC />);

  expect(container).toHaveTextContent("A");
  expect(container).toHaveTextContent("B");
  expect(container).toHaveTextContent("C");
});
