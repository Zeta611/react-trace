import React from "react";
import { render, fireEvent } from "@testing-library/react";
import "@testing-library/jest-dom";
import Button from "./button";
import print from "./print";

jest.mock("./print", () => ({
  __esModule: true,
  default: jest.fn(),
}));

test("Button prints 'B' when clicked", () => {
  print.mockClear();

  const { container } = render(<Button />);

  const button = container.querySelector("button");

  expect(print).not.toHaveBeenCalled();

  fireEvent.click(button);
  expect(print).toHaveBeenCalledWith("B");
  expect(print).toHaveBeenCalledTimes(1);

  fireEvent.click(button);
  expect(print).toHaveBeenCalledTimes(2);

  fireEvent.click(button);
  expect(print).toHaveBeenCalledTimes(3);
});
