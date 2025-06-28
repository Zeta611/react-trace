import React from "react";
import { render, fireEvent } from "@testing-library/react";
import "@testing-library/jest-dom";
import EventCounter from "./counter";
import print from "./print";

jest.mock("./print", () => ({
  __esModule: true,
  default: jest.fn(),
}));

test("Counter test with increment then decrement [0; 1]", () => {
  print.mockClear();

  const { container } = render(<EventCounter />);

  const buttons = container.querySelectorAll("button");

  fireEvent.click(buttons[0]);
  fireEvent.click(buttons[1]);

  expect(print).toHaveBeenCalledTimes(3);
  expect(print.mock.calls[0][0]).toBe(1);
  expect(print.mock.calls[1][0]).toBe(2);
  expect(print.mock.calls[2][0]).toBe(1);
});
