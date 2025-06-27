import React from "react";
import { render } from "@testing-library/react";
import "@testing-library/jest-dom";
import RenderCycleSetInBodyGuarded from "./set_in_body_guarded_reread_count";
import print from "./print";

jest.mock("./print", () => ({
  __esModule: true,
  default: jest.fn(),
}));

test("Retry renders counter with value 2", () => {
  print.mockClear();

  render(<RenderCycleSetInBodyGuarded />);

  expect(print.mock.calls[0][0]).toBe("C");
  expect(print.mock.calls[1][0]).toBe("C");
  expect(print.mock.calls[2][0]).toBe("");
});
