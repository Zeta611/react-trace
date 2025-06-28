import React from "react";
import { render, act } from "@testing-library/react";
import "@testing-library/jest-dom";
import RenderCycleSiblingSetter from "./set_sibling_state_during_effect";
import print from "./print";

jest.mock("./print", () => ({
  __esModule: true,
  default: jest.fn(),
}));

test("Set sibling state during effect works correctly", async () => {
  print.mockClear();

  await act(async () => {
    render(<RenderCycleSiblingSetter />);
  });

  expect(print).toHaveBeenCalledTimes(3);
  expect(print.mock.calls[0][0]).toBe("D");
  expect(print.mock.calls[1][0]).toBe("D");
  expect(print.mock.calls[2][0]).toBe("D");
});
