import React from "react";
import { render, fireEvent } from "@testing-library/react";
import "@testing-library/jest-dom";
import SideEffectEventHandler from "./event_handler_prints";
import print from "./print";

jest.mock("./print", () => ({
  __esModule: true,
  default: jest.fn(),
}));

test("Event handlers print correct values when clicked", () => {
  print.mockClear();

  const { container } = render(<SideEffectEventHandler />);

  const buttons = container.querySelectorAll("button");
  const button0 = buttons[0];
  const button1 = buttons[1];

  expect(print).not.toHaveBeenCalled();

  fireEvent.click(button0);
  expect(print).toHaveBeenCalledWith("0");

  fireEvent.click(button1);
  expect(print).toHaveBeenCalledWith("1");
});
