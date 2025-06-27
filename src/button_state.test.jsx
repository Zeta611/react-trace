import React from "react";
import { render, fireEvent } from "@testing-library/react";
import "@testing-library/jest-dom";
import ButtonState from "./button_state";
import print from "./print";

jest.mock("./print", () => ({
  __esModule: true,
  default: jest.fn(),
}));

test("ButtonState prints C, B, and current count when clicked in some order", () => {
  print.mockClear();

  const { container } = render(<ButtonState />);

  expect(print.mock.calls[0][0]).toBe("C");

  const button = container.querySelector("button");

  fireEvent.click(button);
  expect(print.mock.calls[1][0]).toBe("B");
  // NOTE: React optimization strikes here;
  // setS body is run eagerly without being queued
  expect(print.mock.calls[2][0]).toBe(0);
  expect(print.mock.calls[3][0]).toBe("C");

  fireEvent.click(button);
  expect(print.mock.calls[4][0]).toBe("B");
  expect(print.mock.calls[5][0]).toBe("C");
  expect(print.mock.calls[6][0]).toBe(1);

  fireEvent.click(button);
  expect(print.mock.calls[7][0]).toBe("B");
  expect(print.mock.calls[8][0]).toBe("C");
  expect(print.mock.calls[9][0]).toBe(2);
});
