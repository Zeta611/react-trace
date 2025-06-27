import React from "react";
import { render } from "@testing-library/react";
import "@testing-library/jest-dom";
import RenderCycleSetInBodyUnguarded from "./set_in_body_unguarded_nonterminate";

jest.mock("./print", () => ({
  __esModule: true,
  default: jest.fn(),
}));

test("Infinite retries when top-level setter is unguarded", () => {
  const originalConsoleError = console.error;
  console.error = jest.fn();

  const { container } = render(<RenderCycleSetInBodyUnguarded />);

  expect(container).toHaveTextContent("Error");

  console.error = originalConsoleError;
});
