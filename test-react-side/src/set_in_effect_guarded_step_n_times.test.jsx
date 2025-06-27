import React from "react";
import { render } from "@testing-library/react";
import "@testing-library/jest-dom";
import RenderCycleSetStateGuarded from "./set_in_effect_guarded_step_n_times";
import print from "./print";

jest.mock("./print", () => ({
  __esModule: true,
  default: jest.fn(),
}));

test("Re-render 5 times when setter is called in useEffect with guard", () => {
  print.mockClear();

  const { container } = render(<RenderCycleSetStateGuarded />);

  expect(print).toHaveBeenCalledTimes(5);

  expect(print.mock.calls[0][0]).toBe("C");
  expect(print.mock.calls[1][0]).toBe("C");
  expect(print.mock.calls[2][0]).toBe("C");
  expect(print.mock.calls[3][0]).toBe("C");
  expect(print.mock.calls[4][0]).toBe("C");
});

