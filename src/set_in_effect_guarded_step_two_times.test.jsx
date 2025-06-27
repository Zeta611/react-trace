import React from "react";
import { render } from "@testing-library/react";
import "@testing-library/jest-dom";
import RenderCycleSetInEffectGuardedStep2 from "./set_in_effect_guarded_step_two_times";
import print from "./print";

jest.mock("./print", () => ({
  __esModule: true,
  default: jest.fn(),
}));

test("Re-render 2 times when setter is called in useEffect with guard", () => {
  print.mockClear();

  const { container } = render(<RenderCycleSetInEffectGuardedStep2 />);

  expect(print).toHaveBeenCalledTimes(2);
  expect(print.mock.calls[0][0]).toBe("C");
  expect(print.mock.calls[1][0]).toBe("C");
});

