import React from "react";
import { render } from "@testing-library/react";
import "@testing-library/jest-dom";
import RenderCycleSetInEffectStep from "./no_set_in_effect_step_one_time";
import print from "./print";

jest.mock("./print", () => ({
  __esModule: true,
  default: jest.fn(),
}));

test("No re-render when identity setter is called in useEffect", () => {
  print.mockClear();

  const { container } = render(<RenderCycleSetInEffectStep />);

  expect(print).toHaveBeenCalledTimes(1);
  expect(print.mock.calls[0][0]).toBe("C");
});
