import React from "react";
import { render } from "@testing-library/react";
import "@testing-library/jest-dom";
import ChildChangesAfterRerender from "./child_changes_after_rerender";
import print from "./print";

jest.mock("./print", () => ({
  __esModule: true,
  default: jest.fn(),
}));

test("child changes after rerender", () => {
  print.mockClear();

  render(<ChildChangesAfterRerender />);

  expect(print.mock.calls[0][0]).toBe(1);
  expect(print.mock.calls[1][0]).toBe("P");
  expect(print.mock.calls[2][0]).toBe(2);
  expect(print.mock.calls[3][0]).toBe("P");
});
