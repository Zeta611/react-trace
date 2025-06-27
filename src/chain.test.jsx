import React from "react";
import { render } from "@testing-library/react";
import "@testing-library/jest-dom";
import Chain from "./chain";

test("Chain renders numbers from 5 to 0", () => {
  const { container } = render(<Chain />);

  expect(container).toHaveTextContent("5");
  expect(container).toHaveTextContent("4");
  expect(container).toHaveTextContent("3");
  expect(container).toHaveTextContent("2");
  expect(container).toHaveTextContent("1");
  expect(container).toHaveTextContent("0");
});
