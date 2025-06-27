import React from "react";
import { render } from "@testing-library/react";
import "@testing-library/jest-dom";
import ParentChild from "./parent_child";
import print from "./print";

jest.mock("./print", () => ({
  __esModule: true,
  default: jest.fn(),
}));

test("Child component re-renders whenever Parent component re-renders", () => {
  print.mockClear();

  const { container } = render(<ParentChild />);

  expect(container).toHaveTextContent("10");
  const parentCalls = print.mock.calls.filter((call) => call[0] === "P").length;
  const childCalls = print.mock.calls.filter((call) => call[0] === "C").length;
  expect(parentCalls).toBe(11);
  expect(childCalls).toBe(11);
});
