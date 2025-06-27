import React from "react";
import { render, act } from "@testing-library/react";
import "@testing-library/jest-dom";
import RenderCycleSetPassedIndefinite from "./set_passed_step_indefinitely";
import print from "./print";

jest.mock("./print", () => ({
  __esModule: true,
  default: jest.fn(),
}));

test("Would render indefinitely when diverging setter is passed to child", async () => {
  // Suppress console errors for this test
  const originalConsoleError = console.error;
  console.error = jest.fn();

  print.mockClear();

  await act(async () => {
    render(<RenderCycleSetPassedIndefinite />);
  });

  expect(print).toHaveBeenCalledWith("Reached MAX_FUEL (100)");

  console.error = originalConsoleError;
});
