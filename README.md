# React-tRace Test Suite (React-side)

## Testing
To run all the tests, run

```sh
npm test
```

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
