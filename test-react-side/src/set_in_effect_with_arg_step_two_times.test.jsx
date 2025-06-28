import React from "react";
import { render } from "@testing-library/react";
import "@testing-library/jest-dom";
import RenderCycleSetInEffectWithArgStep2 from "./set_in_effect_with_arg_step_two_times";
import print from "./print";

jest.mock("./print", () => ({
  __esModule: true,
  default: jest.fn(),
}));

test("Re-render 1 time when setter with different arg is called in useEffect", () => {
  print.mockClear();

  render(<RenderCycleSetInEffectWithArgStep2 />);

  expect(print).toHaveBeenCalledTimes(2);
  expect(print.mock.calls[0][0]).toBe("");
  expect(print.mock.calls[1][0]).toBe("");
});
