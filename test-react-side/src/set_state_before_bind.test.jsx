import React from "react";
import { render, act } from "@testing-library/react";
import "@testing-library/jest-dom";
import RenderCycleSetStateBeforeBind from "./set_state_before_bind";
import print from "./print";

jest.mock("./print", () => ({
  __esModule: true,
  default: jest.fn(),
}));

test("Set state before bind causes correct render sequence", async () => {
  print.mockClear();

  // Use act to ensure all effects and re-renders complete
  await act(async () => {
    render(<RenderCycleSetStateBeforeBind />);
  });

  expect(print).toHaveBeenCalledTimes(8);
  expect(print.mock.calls[0]).toEqual([0]);
  expect(print.mock.calls[1]).toEqual([0]);
  expect(print.mock.calls[2]).toEqual([1]);
  expect(print.mock.calls[3]).toEqual([1]);
  expect(print.mock.calls[4]).toEqual([2]);
  expect(print.mock.calls[5]).toEqual([1]);
  expect(print.mock.calls[6]).toEqual([3]);
  expect(print.mock.calls[7]).toEqual([1]);
});
