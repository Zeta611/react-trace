import React from "react";
import { render } from "@testing-library/react";
import "@testing-library/jest-dom";
import ComponentCreateNew from "./new_child_steps_again";
import print from "./print";

jest.mock("./print", () => ({
  __esModule: true,
  default: jest.fn(),
}));

test("A new child component triggers a re-render", () => {
  print.mockClear();

  render(<ComponentCreateNew />);
  expect(print.mock.calls.length).toEqual(4);
});
