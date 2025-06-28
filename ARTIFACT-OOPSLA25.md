# Overview of the Artifact for "React-tRace: A Semantics for Understanding React Hooks"

## Introduction
We introduce React-tRace, a formalization of the semantics of the essence of React Hooks.

This artifact contains the implementation of the React-tRace definitional interpreter and the web frontend that visualizes the execution of React programs (Section 5). The conformance test suite is also included, which consists of the [React-tRace side](./test/) and the [React side](./test-react-side/) (Section 6.2). Example React programs presented in the paper can be found inside [`samples/oopsla25/`](./samples/oopsla25/) (Sections 2, 3, and 4.3).

## Hardware Dependencies
We have developed React-tRace on Apple Silicon Macs and a Linux (NixOS) machine running an Intel CPU. We have not tested on Windows, but our dependencies OCaml 5.3 [fully supports Windows](https://ocaml.org/docs/ocaml-on-windows) and Node.js 22 [supports Windows](https://nodejs.org/en/download) in principle.

## Getting Started
From hereon, we use `𝝺` to denote a prompt symbol to differentiate from an expected output (if any).

### Dependencies
OCaml 5.3 is required to build React-tRace, and Node.js 22 is required to run the visualizer frontend and run our empirical test suite for the React-side.

If you use Nix, you can simply run the following to set up the dependencies:
```sh
𝝺 nix develop
# or if you haven't already enabled nix-command and flakes,
𝝺 nix develop --extra-experimental-features nix-command --extra-experimental-features flakes
```

We also provide a small [`Dockerfile`](./Dockerfile) that wraps the Nix environment. Use [`setup-docker.sh`](./setup-docker.sh) to build the image and run the container:
```sh
𝝺 ./setup-docker.sh
# or if you are not in the docker group
𝝺 sudo ./setup-docker.sh
```
Port 3000 is mapped to the host's port 3000 for hosting the visualizer frontend. You may edit this in `setup-docker.sh` if you prefer to use some other port.

If you prefer to manually install dependencies, install [OCaml](https://ocaml.org/install) and [Node.js](https://nodejs.org/en/download) following the official guidelines. You need to manually install dependencies with opam (this is automatically done with Nix if you use Nix or the Docker wrapper):
```sh
𝝺 opam install . --deps-only --with-test
```

### Running
If the dependencies are correctly set up, you can run React-tRace with Dune:
```sh
𝝺 dune exec react_trace -- samples/hello.ml
react_trace: [INFO] Step init 1
react_trace: [INFO] Step effect 2
react_trace: [INFO] Step check 2
react_trace: [INFO] Terminate


Steps: 1
```

To run the frontend, go inside `frontend/` by
```sh
cd frontend/
```
and
```sh
𝝺 npm install && npm run res:build && npm run dev
...
> next dev --turbopack

   ▲ Next.js 15.2.3 (Turbopack)
   - Local:        http://localhost:3000
   - Network:      http://192.168.50.27:3000

 ✓ Starting...
 ✓ Ready in 740ms
```
Now visit the visualizer at `http://localhost:3000` in your favorite browser.


## Step-by-Step Instructions
Use the artifact to run the examples from the paper, host the visualizer web interface locally, and reproduce the conformance test suite.

### Examples from the Paper
The examples have the `.ml` extension to leverage existing syntax highlighting facilities for OCaml in text editors.

#### An Example in **Section 2.1** (L186--192)
The console output for `Counter` is reproduced when provided with the `-events=0` option to trigger the first event handler to emulate the button click:
```sh
𝝺 dune exec react_trace -- samples/oopsla25/peculiarity-usestate.ml -events=0
react_trace: [INFO] Step init 1
react_trace: [INFO] Step effect 2
react_trace: [INFO] Step check 2
react_trace: [INFO] Step loop [event: 0]
react_trace: [INFO] Step check 2
react_trace: [INFO] Step effect 3
react_trace: [INFO] Step check 3
react_trace: [INFO] Terminate
Counter
Return
Counter
Update
Return


Steps: 2
```


#### An Example in **Section 2.2** (L221--231)
The console output for `SelfCounter` is reproduced, showing the excess render cycles:
```sh
𝝺 dune exec react_trace -- samples/oopsla25/peculiarity-useeffect.ml
react_trace: [INFO] Step init 1
react_trace: [INFO] Step effect 2
react_trace: [INFO] Step check 2
react_trace: [INFO] Step effect 3
react_trace: [INFO] Step check 3
react_trace: [INFO] Step effect 4
react_trace: [INFO] Step check 4
react_trace: [INFO] Step effect 5
react_trace: [INFO] Step check 5
react_trace: [INFO] Terminate
0
Return
Effect
1
Return
Effect
2
Return
Effect
3
Return
Effect


Steps: 4
```


#### An Example in **Section 3.1.1** (L265--268)
`Inf` completely depletes the fuel for any `n` (by providing the `-fuel=n` option to limit the render cycles), meaning that it renders indefinitely:
```sh
𝝺 dune exec react_trace -- samples/oopsla25/infinite-render.ml -fuel=10
react_trace: [INFO] Step init 1
react_trace: [INFO] Step effect 2
react_trace: [INFO] Step check 2
react_trace: [INFO] Step effect 3
react_trace: [INFO] Step check 3
react_trace: [INFO] Step effect 4
react_trace: [INFO] Step check 4
react_trace: [INFO] Step effect 5
react_trace: [INFO] Step check 5
react_trace: [INFO] Step effect 6
react_trace: [INFO] Step check 6
react_trace: [INFO] Step effect 7
react_trace: [INFO] Step check 7
react_trace: [INFO] Step effect 8
react_trace: [INFO] Step check 8
react_trace: [INFO] Step effect 9
react_trace: [INFO] Step check 9
react_trace: [INFO] Step effect 10
react_trace: [INFO] Step check 10


Steps: 10
```
You can also omit the option and manually terminate the process using `ctrl-c`.


#### An Example in **Section 3.1.2** (L281--285)
`Inf2` retries indefinitely without reaching the first render cycle. For any `n`, providing the `-re-render-limit=n` option (the confusing option name is due to the overloading of `re-render` from the React documentation---we refer to this as a *retry* in our paper) leads to an exception:
```sh
𝝺 dune exec react_trace -- samples/oopsla25/top-level-setter.ml -re-render-limit=25
react_trace: [INFO] Step init 1
Fatal error: exception React_trace.Interp_effects.Too_many_re_renders
Raised at React_trace__Interp.eval_mult in file "lib/interp.ml", line 366, characters 52-77
Called from React_trace__Interp.eval_mult in file "lib/interp.ml", line 384, characters 4-54
...
```
You can also omit the option and manually terminate the process using `ctrl-c`.


#### Two Examples in **Section 3.2**
The components `Flicker` and `Parent` re-render (therefore reaching the step count of two) without user interaction, which is a symptom of unnecessary re-rendering.
##### L288--292
```sh
𝝺 dune exec --release react_trace -- samples/oopsla25/unnecessary-re-render.ml
react_trace: [INFO] Step init 1
react_trace: [INFO] Step effect 2
react_trace: [INFO] Step check 2
react_trace: [INFO] Step effect 3
react_trace: [INFO] Step check 3
react_trace: [INFO] Terminate


Steps: 2
```

##### L301--306
```sh
𝝺 dune exec --release react_trace -- samples/oopsla25/unnecessary-re-render-inter.ml
react_trace: [INFO] Step init 1
react_trace: [INFO] Step effect 2
react_trace: [INFO] Step check 2
react_trace: [INFO] Step effect 3
react_trace: [INFO] Step check 3
react_trace: [INFO] Terminate


Steps: 2
```


#### An Illustrative Example in **Section 4.3** (L741--748)
```sh
𝝺 dune exec --release react_trace -- samples/oopsla25/demo.ml
react_trace: [INFO] Step init 1
react_trace: [INFO] Step effect 2
react_trace: [INFO] Step check 2
react_trace: [INFO] Step effect 3
react_trace: [INFO] Step check 3
react_trace: [INFO] Terminate


Steps: 2
```

### The Visualizer
Inside the [`frontend/`](./frontend/) directory, run
```sh
𝝺 npm install && npm run res:build && npm run dev
```
to host the web frontend. The latest version is hosted [online](https://react-trace.vercel.app) as well.

The top-left pane is the editor (with an optional Vim keymap support), and the top-right pane represents the tree memory hierarchy. The bottom pane includes a slider to step through (and *back!*) each execution step of the render cycle and an explanation of each step.

As mentioned in the paper, experimental JS translation is in progress (note the toggle button for JS mode shown in the UI).

### Conformance Test Suite
We provide up-to-date test suite scenarios below.

| #   | Scenarios                                                       | Tests # | Rtrace    |
|:----|:-----------------------------------------------------------------|--------:|:---------:|
| 1   | No re-render w/o a setter call                                   |       6 |     ✓     |
| 2   | Retries (0 < n < 25) w/ setter call during body eval             |       4 |     ✓     |
| 3   | Infinite retries (n ≥ 25) w/ setter call during body eval        |       1 |     ✓     |
| 4   | No re-render w/o Effects w/ setter call during body eval         |       1 |     ✓     |
| 5   | No re-render w/ Effect w/o setter call                           |       2 |     ✓     |
| 6   | No re-render w/ Effect w/ id setter call                         |       1 |     ✓     |
| 7   | No re-render w/ Effect w/ setter calls composing to id           |       2 |     ✓     |
| 8   | Re-renders (0 < n < 100) w/ Effect w/ setter call                |      15 |     ✓     |
| 9   | Infinite re-renders (n ≥ 100) w/ Effect w/ diverging setter call |       2 |     ✓     |
| 10  | Re-render w/ child updating parent during Effect                 |       2 |     ✓     |
| 11  | Re-render w/ sibling updating another during Effect              |       1 |     ✓     |
| 12  | Error w/ child updating parent during body eval                  |       1 |     ✓     |
| 13  | Non-trivial reconciliation                                        |       3 |     ✓     |
| 14  | No re-render w/ direct object update                             |       1 |     ✓     |
| 15  | Re-render w/ idle but parent updates                             |       2 |     ✓     |
| 16  | User event sequence                                              |       6 |     ✓     |
| 17  | Re-render w/ setter call from user event                         |       4 |     ✓[^2] |
| 18  | Recursive view hierarchy                                         |       2 |     ✓     |
|     |                                                                  | 37[^1]  |     ✓     |

[^1]: Some tests cover multiple scenarios.
[^2]: React’s optimization changes some execution orders.

The [`button_state` test case](./test/test_react_trace.ml) of scenario 17 is where the execution order deviates between React-tRace and React.

#### React-tRace Side
Under [`test/`](./test/), run the following to execute the React-tRace side of the test suite.
```sh
𝝺 dune test
```
There are 37 test cases (test case 0 to 36) in the `React-tRace` test suite, and the corresponding scenarios are labeled in the description of each test case.

Note that there are additional test suites other than `React-tRace` to test the parsing and the JS conversion.

#### React Side
Under [`test-react-side/`](./test-react-side/), run the following to execute the React side of the test suite.
```sh
𝝺 npm install && npm test
```
Again, there are 37 test cases, and the names of the test files match exactly with the corresponding testing function in the React-tRace side. For example, [`set_in_removed_child_step_two_times.test.jsx`](./test-react-side/src/set_in_removed_child_step_two_times.test.jsx) corresponds to [`set_in_removed_child_step_two_times`](./test/test_react_trace.ml) function in [line 1006 in test/test_react_trace.ml](./test/test_react_trace.ml).

For tests that measure the number of render cycles, we indirectly measure the count by counting the prints inside an Effect.


## Reusability Guide
The React-tRace implementation can function both as a command-line definitional interpreter and as a reusable library for inclusion in other programs. The frontend web interface (under [`frontend/`](./frontend/)) is an example of how to use it in other programs.

React-tRace can be included in web projects using the Js_of_ocaml compiler. Under [`bin/js/`](./bin/js/) is an example of how React-tRace can be exported as a JS library. The output JS file is included in the frontend interface under [`frontend/shared/react-trace/react-trace.bc.js`](./frontend/shared/react-trace/react-trace.bc.js). Since Js_of_ocaml uses its internal representations for encoding data, JSON serialization is required to interface with typical JS applications.

We provide [`Recorder_intf`](./lib/recorder_intf/recorder_intf.ml) for developers to implement custom logging facilities to "hook into" (as a general term, not React Hook) the definitional interpreter. This is how we provide different interfaces ourselves---a command-line interface via the `-report` command-line option (implemented in [`bin/native/report_box_recorder/`](./bin/native/report_box_recorder/)) and a web GUI interface (implemented in [`bin/js/recorder/`](./bin/js/recorder/)).
