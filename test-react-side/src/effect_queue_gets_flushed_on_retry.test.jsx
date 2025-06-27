import React from "react";
import { render } from "@testing-library/react";
import "@testing-library/jest-dom";
import SideEffectFlushQueue from "./effect_queue_gets_flushed_on_retry";
import print from "./print";

jest.mock("./print", () => ({
  __esModule: true,
  default: jest.fn(),
}));

test("Effect queue gets flushed on retry", () => {
  print.mockClear();

  render(<SideEffectFlushQueue />);
  expect(print.mock.calls[0][0]).toBe("C");
  expect(print.mock.calls[1][0]).toBe("C");
  expect(print.mock.calls[2][0]).toBe("useEffect");
  expect(print.mock.calls[3][0]).toBe("C");
  expect(print.mock.calls[4][0]).toBe("useEffect");
  expect(print.mock.calls[5][0]).toBe("C");
});
