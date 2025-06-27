import React from "react";
import { render } from "@testing-library/react";
import "@testing-library/jest-dom";
import RenderCycleSetInBodyGuarded from "./set_in_body_guarded_no_rerender";
import print from "./print";

jest.mock("./print", () => ({
  __esModule: true,
  default: jest.fn(),
}));

test("Guarded top-level setState stops re-rendering", () => {
  print.mockClear();

  render(<RenderCycleSetInBodyGuarded />);

  expect(print.mock.calls.length).toBe(1);
  expect(print.mock.calls[0][0]).toBe("");
});

