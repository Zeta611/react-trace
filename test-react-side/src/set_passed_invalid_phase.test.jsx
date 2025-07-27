import React from "react";
import { render } from "@testing-library/react";
import "@testing-library/jest-dom";
import RenderCycleSetPassedInvalidPhase from "./set_passed_invalid_phase";
import print from "./print";

jest.mock("./print", () => ({
  __esModule: true,
  default: jest.fn(),
}));

// In React, setting state during render phase generates a warning
// but doesn't actually error - this is different from the trace interpreter
test("Setting state in render phase works in React but logs invalid phase", () => {
  const originalConsoleError = console.error;
  console.error = jest.fn();

  print.mockClear();

  render(<RenderCycleSetPassedInvalidPhase />);

  expect(console.error.mock.calls).toHaveLength(1);
  expect(console.error.mock.calls[0][0]).toMatch(
    "Cannot update a component (`%s`) while rendering a different component (`%s`).",
  );

  console.error = originalConsoleError;
});
