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
  expect(print.mock.calls.length).toEqual(22);
  for (let i = 0; i < 22; i += 2) {
    expect(print.mock.calls[i][0]).toBe("C");
    expect(print.mock.calls[i + 1][0]).toBe("P");
  }
});
