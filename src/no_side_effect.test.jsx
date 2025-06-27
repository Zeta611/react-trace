import React from "react";
import { render } from "@testing-library/react";
import "@testing-library/jest-dom";
import NoSideEffects from "./no_side_effect";
import print from "./print";

jest.mock("./print", () => ({
  __esModule: true,
  default: jest.fn(),
}));

test("No side effects implies not re-render", () => {
  print.mockClear();

  render(<NoSideEffects />);

  expect(print).toHaveBeenCalledTimes(1);
});
