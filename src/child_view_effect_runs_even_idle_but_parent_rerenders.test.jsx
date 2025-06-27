import React from "react";
import { render } from "@testing-library/react";
import "@testing-library/jest-dom";
import SideEffectChildEffects from "./child_view_effect_runs_even_idle_but_parent_rerenders";
import print from "./print";

jest.mock("./print", () => ({
  __esModule: true,
  default: jest.fn(),
}));

test("Idle child's effects are run when parent re-renders", () => {
  print.mockClear();

  render(<SideEffectChildEffects />);

  expect(print.mock.calls[0][0]).toBe("C");
  expect(print.mock.calls[1][0]).toBe("C");
});
