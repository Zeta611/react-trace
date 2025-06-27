import React from "react";
import { Routes, Route, Link } from "react-router-dom";

// View Structure Tests
import Chain from "./chain";
import Binary from "./binary";
import ABC from "./abc";

// Render Cycle Tests
import NoSideEffects from "./no_side_effect";
import Rerender from "./set_in_effect_guarded_step_n_times2";
import ParentChild from "./parent_child";

// Side Effect Tests
import Button from "./button";
import ButtonState from "./button_state";

// Render Cycle Tests
import RenderCycleSetStateGuarded from "./set_in_effect_guarded_step_n_times";
import RenderCycleSetStateGuardedWithObj from "./setStateGuardedWithObj";
import RenderCycleSetInBodyGuarded from "./set_in_body_guarded_no_rerender";
import RenderCycleSetInBodyGuarded2 from "./set_in_body_guarded_reread_count2";
import RenderCycleSetInBodyGuarded3 from "./set_in_body_guarded_reread_count";
import RenderCycleSetInBodyUnguarded from "./set_in_body_unguarded_nonterminate";
import RenderCycleSetInEffectStep from "./no_set_in_effect_step_one_time";
import RenderCycleSetInEffectStep1 from "./set_in_effect_step_one_time";
import RenderCycleSetInEffectStep2 from "./set_in_effect_step_two_times";
import RenderCycleSetInEffectGuardedStep2 from "./set_in_effect_guarded_step_two_times";
import RenderCycleSetInEffectTwiceStep1 from "./setInEffectTwiceStep1";
import RenderCycleSetInEffectWithArgStep1 from "./setInEffectWithArgStep1";
import RenderCycleSetInEffectWithArgStep2 from "./setInEffectWithArgStep2";
import RenderCycleSetInEffectIndefinite from "./set_in_effect_step_indefinitely";
import RenderCycleSetPassedStep2 from "./setPassedStep2";
import RenderCycleSetPassedInvalidPhase from "./set_passed_invalid_phase";
import RenderCycleSetPassedIndefinite from "./setPassedIndefinite";
import RenderCycleUpdateObjNoRerender from "./updateObjNoRerender";
import RenderCycleSetStateBeforeBind from "./set_state_before_bind";
import RenderCycleSiblingSetter from "./set_sibling_state_during_effect";

// Component Tests
import ComponentStateInRemoved from "./stateInRemoved";
import ComponentStatePersistence from "./statePersistence";
import ComponentCreateNew from "./createNew";

// Side Effect Tests
import SideEffectFlushQueue from "./effect_queue_gets_flushed_on_retry";
import SideEffectChildEffects from "./child_view_effect_runs_even_idle_but_parent_rerenders";
import SideEffectNestedViewOrder from "./nested_view_render_order";
import SideEffectEventHandler from "./event_handler_prints";

// Event Tests
import EventCounter from "./counter";

function App() {
  return (
    <>
      <nav>
        <ul>
          <li>
            <Link to="/">Home</Link>
          </li>
          <ol>
            <li>
              <Link to="/view-structure/abc">ABC</Link>
            </li>
            <li>
              <Link to="/view-structure/chain">Chain</Link>
            </li>
            <li>
              <Link to="/view-structure/binary-tree">Binary Tree</Link>
            </li>
            <li>
              <Link to="/render-cycle/no-side-effects">
                No re-render when side effect is absent
              </Link>
            </li>
            <li>
              <Link to="/render-cycle/retry">
                No re-render with top-level guarded sets
              </Link>
            </li>
            <li>
              <Link to="/render-cycle/rerender">
                Re-render when sets are called in an Effect
              </Link>
            </li>
            <li>
              <Link to="/render-cycle/parent-child">
                Re-render when parent re-renders
              </Link>
            </li>
            <li>
              <Link to="/render-cycle/set-state-in-body-guarded">
                Guarded setState in render phase
              </Link>
            </li>
            <li>
              <Link to="/render-cycle/set-state-in-body-guarded2">
                Guarded setState in render phase (2)
              </Link>
            </li>
            <li>
              <Link to="/render-cycle/set-state-in-body-unguarded">
                Unguarded setState in render phase
              </Link>
            </li>
            <li>
              <Link to="/render-cycle/set-state-guarded">
                Re-render 5 times with guarded setState in Effect
              </Link>
            </li>
            <li>
              <Link to="/render-cycle/set-state-guarded-obj">
                Re-render 5 times with guarded setState on object
              </Link>
            </li>
            <li>
              <Link to="/render-cycle/set-in-effect-step1">
                No re-render with identity setState in Effect
              </Link>
            </li>
            <li>
              <Link to="/render-cycle/set-in-effect-step2">
                Re-render once with setState in Effect
              </Link>
            </li>
            <li>
              <Link to="/render-cycle/set-in-effect-guarded-step2">
                Re-render once with guarded setState in Effect
              </Link>
            </li>
            <li>
              <Link to="/render-cycle/set-in-effect-twice-step1">
                No re-render when setters compose to identity
              </Link>
            </li>
            <li>
              <Link to="/render-cycle/set-in-effect-indefinite">
                Would render indefinitely with diverging setState
              </Link>
            </li>
            <li>
              <Link to="/render-cycle/set-in-effect-with-arg-step1">
                No re-render when identity setState with arg
              </Link>
            </li>
            <li>
              <Link to="/render-cycle/set-in-effect-with-arg-step2">
                Re-render once when setState with different arg
              </Link>
            </li>
            <li>
              <Link to="/render-cycle/set-passed-step2">
                Re-render once when setter called in child
              </Link>
            </li>
            <li>
              <Link to="/render-cycle/set-passed-invalid-phase">
                Setting state in invalid phase
              </Link>
            </li>
            <li>
              <Link to="/render-cycle/set-passed-indefinite">
                Would render indefinitely with passed setter
              </Link>
            </li>
            <li>
              <Link to="/render-cycle/set-state-before-bind">
                Set state before binding
              </Link>
            </li>
            <li>
              <Link to="/render-cycle/set-sibling-during-effect">
                Set sibling state during effect
              </Link>
            </li>
            <li>
              <Link to="/render-cycle/update-obj-no-rerender">
                No re-render with direct object mutation
              </Link>
            </li>
            <li>
              <Link to="/component/state-in-removed">
                State update in removed child
              </Link>
            </li>
            <li>
              <Link to="/component/state-persistence">
                State persistence across renders
              </Link>
            </li>
            <li>
              <Link to="/component/create-new">
                New child components with fresh state
              </Link>
            </li>
            <li>
              <Link to="/side-effect/button">Button Click</Link>
            </li>
            <li>
              <Link to="/side-effect/button-state">Button with State</Link>
            </li>
            <li>
              <Link to="/side-effect/flush-queue">Effect Queue Flushing</Link>
            </li>
            <li>
              <Link to="/side-effect/child-effects">
                Child Component Effects
              </Link>
            </li>
            <li>
              <Link to="/side-effect/nested-view-order">
                Nested View Effect Order
              </Link>
            </li>
            <li>
              <Link to="/side-effect/event-handler">Event Handler Prints</Link>
            </li>
            <li>
              <Link to="/event/counter">Counter with Limits (3 tests)</Link>
            </li>
            <li>
              <Link to="/event/setter-in-setter">Setter in Setter</Link>
            </li>
          </ol>
        </ul>
      </nav>
      <Routes>
        <Route index element={<h1>React-tRrace Testsuites</h1>} />
        <Route path="view-structure">
          <Route path="abc" element={<ABC />} />
          <Route path="chain" element={<Chain />} />
          <Route path="binary-tree" element={<Binary />} />
        </Route>
        <Route path="render-cycle">
          <Route path="no-side-effects" element={<NoSideEffects />} />
          <Route path="rerender" element={<Rerender />} />
          <Route path="parent-child" element={<ParentChild />} />

          {/* State updates in render phase */}
          <Route
            path="set-state-in-body-guarded"
            element={<RenderCycleSetInBodyGuarded />}
          />
          <Route
            path="set-state-in-body-guarded2"
            element={<RenderCycleSetInBodyGuarded2 />}
          />
          <Route
            path="set-state-in-body-guarded3"
            element={<RenderCycleSetInBodyGuarded3 />}
          />
          <Route
            path="set-state-in-body-unguarded"
            element={<RenderCycleSetInBodyUnguarded />}
          />

          {/* State updates in effect */}
          <Route
            path="set-state-guarded"
            element={<RenderCycleSetStateGuarded />}
          />
          <Route
            path="set-state-guarded-obj"
            element={<RenderCycleSetStateGuardedWithObj />}
          />
          <Route
            path="no-set-in-effect-step"
            element={<RenderCycleSetInEffectStep />}
          />
          <Route
            path="set-in-effect-step1"
            element={<RenderCycleSetInEffectStep1 />}
          />
          <Route
            path="set-in-effect-step2"
            element={<RenderCycleSetInEffectStep2 />}
          />
          <Route
            path="set-in-effect-guarded-step2"
            element={<RenderCycleSetInEffectGuardedStep2 />}
          />
          <Route
            path="set-in-effect-twice-step1"
            element={<RenderCycleSetInEffectTwiceStep1 />}
          />
          <Route
            path="set-in-effect-with-arg-step1"
            element={<RenderCycleSetInEffectWithArgStep1 />}
          />
          <Route
            path="set-in-effect-with-arg-step2"
            element={<RenderCycleSetInEffectWithArgStep2 />}
          />
          <Route
            path="set-in-effect-indefinite"
            element={<RenderCycleSetInEffectIndefinite />}
          />

          {/* Passing setState to children */}
          <Route
            path="set-passed-step2"
            element={<RenderCycleSetPassedStep2 />}
          />
          <Route
            path="set-passed-invalid-phase"
            element={<RenderCycleSetPassedInvalidPhase />}
          />
          <Route
            path="set-passed-indefinite"
            element={<RenderCycleSetPassedIndefinite />}
          />
          <Route
            path="set-state-before-bind"
            element={<RenderCycleSetStateBeforeBind />}
          />
          <Route
            path="set-sibling-during-effect"
            element={<RenderCycleSiblingSetter />}
          />

          {/* Object mutation tests */}
          <Route
            path="update-obj-no-rerender"
            element={<RenderCycleUpdateObjNoRerender />}
          />
        </Route>

        <Route path="component">
          <Route
            path="state-in-removed"
            element={<ComponentStateInRemoved />}
          />
          <Route
            path="state-persistence"
            element={<ComponentStatePersistence />}
          />
          <Route path="create-new" element={<ComponentCreateNew />} />
        </Route>

        <Route path="side-effect">
          <Route path="button" element={<Button />} />
          <Route path="button-state" element={<ButtonState />} />
          <Route path="flush-queue" element={<SideEffectFlushQueue />} />
          <Route path="child-effects" element={<SideEffectChildEffects />} />
          <Route
            path="nested-view-order"
            element={<SideEffectNestedViewOrder />}
          />
          <Route path="event-handler" element={<SideEffectEventHandler />} />
        </Route>

        <Route path="event">
          <Route path="counter" element={<EventCounter />} />
        </Route>
      </Routes>
    </>
  );
}

export default App;
