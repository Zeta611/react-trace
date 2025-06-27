import React from "react";
import { render } from "@testing-library/react";
import "@testing-library/jest-dom";
import Rerender from "./set_in_effect_guarded_step_n_times2";
import print from "./print";

jest.mock("./print", () => ({
  __esModule: true,
  default: jest.fn(),
}));

test("Rerender counts up to 25 with useEffect", () => {
  print.mockClear();

  const { container } = render(<Rerender />);

  expect(container).toHaveTextContent("25");
  expect(print).toHaveBeenCalledTimes(26);
  expect(print.mock.calls[25][0]).toBe("25");
});
