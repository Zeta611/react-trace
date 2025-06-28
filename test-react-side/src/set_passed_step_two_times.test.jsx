import React from "react";
import { render } from "@testing-library/react";
import "@testing-library/jest-dom";
import RenderCycleSetPassedStep2 from "./set_passed_step_two_times";
import print from "./print";

jest.mock("./print", () => ({
  __esModule: true,
  default: jest.fn(),
}));

test("Re-render 1 time when setter is called in child component", () => {
  print.mockClear();

  render(<RenderCycleSetPassedStep2 />);

  expect(print.mock.calls.length).toBe(2);

  expect(print.mock.calls[0][0]).toBe("");
  expect(print.mock.calls[1][0]).toBe("");
});
