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
  expect(print.mock.calls[0][0]).toBe("C");
  expect(print.mock.calls[1][0]).toBe("C");
  expect(print.mock.calls[2][0]).toBe("C");
  expect(print.mock.calls[3][0]).toBe("C");
});
