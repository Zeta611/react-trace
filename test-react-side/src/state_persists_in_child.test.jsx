import React from "react";
import { render } from "@testing-library/react";
import "@testing-library/jest-dom";
import ComponentStatePersistence from "./state_persists_in_child";
import print from "./print";

jest.mock("./print", () => ({
  __esModule: true,
  default: jest.fn(),
}));

test("Child state persists when parent re-renders with same component type", () => {
  print.mockClear();

  render(<ComponentStatePersistence />);

  expect(print.mock.calls.length).toEqual(2);
  expect(print.mock.calls[0][0]).toBe("");
  expect(print.mock.calls[1][0]).toBe("");
});
