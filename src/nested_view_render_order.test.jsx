import React from "react";
import { render } from "@testing-library/react";
import "@testing-library/jest-dom";
import SideEffectNestedViewOrder from "./nested_view_render_order";
import print from "./print";

jest.mock("./print", () => ({
  __esModule: true,
  default: jest.fn(),
}));

test("Nested views effects run in correct order", () => {
  print.mockClear();

  render(<SideEffectNestedViewOrder />);

  expect(print.mock.calls[0][0]).toBe("0");
  expect(print.mock.calls[1][0]).toBe("1");
  expect(print.mock.calls[2][0]).toBe("2");
  expect(print.mock.calls[3][0]).toBe("D");
  expect(print.mock.calls[4][0]).toBe("3");
  expect(print.mock.calls[5][0]).toBe("E");
  expect(print.mock.calls[6][0]).toBe("0");
  expect(print.mock.calls[7][0]).toBe("1");
  expect(print.mock.calls[8][0]).toBe("2");
  expect(print.mock.calls[9][0]).toBe("D");
});
