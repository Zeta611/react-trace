import React from "react";
import { render } from "@testing-library/react";
import "@testing-library/jest-dom";
import RenderCycleSetInEffectTwiceStep1 from "./set_in_effect_twice_step_one_time";
import print from "./print";

jest.mock("./print", () => ({
  __esModule: true,
  default: jest.fn(),
}));

test("No re-render when setters compose to identity in useEffect", () => {
  print.mockClear();

  const { container } = render(<RenderCycleSetInEffectTwiceStep1 />);

  // React will batch the state updates, so the component renders once with 42
  expect(print).toHaveBeenCalledTimes(1);
  expect(print.mock.calls[0][0]).toBe("C");
});

