import React from "react";
import { render, act } from "@testing-library/react";
import "@testing-library/jest-dom";
import RenderCycleSetInEffectIndefinite from "./set_in_effect_step_indefinitely";
import print from "./print";

jest.mock("./print", () => ({
  __esModule: true,
  default: jest.fn(),
}));

test("Would render indefinitely when diverging setter is called in useEffect", async () => {
  // Suppress console errors for this test
  const originalConsoleError = console.error;
  console.error = jest.fn();

  print.mockClear();

  // Use act to ensure all effects and re-renders complete
  await act(async () => {
    render(<RenderCycleSetInEffectIndefinite />);
  });

  // Check that we reached MAX_FUEL
  expect(print).toHaveBeenCalledWith("Reached MAX_FUEL (100)");

  // Restore console.error
  console.error = originalConsoleError;
});

