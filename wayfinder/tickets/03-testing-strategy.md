# Testing strategy

- **Type:** `wayfinder:grilling`
- **Status:** open
- **Assignee:** unclaimed
- **Blocked by:** #1 (testability audit)
- **Blocks:** —
- **Parent map:** [MAP](../MAP.md)

## Question

Given the testability audit (#1), decide the testing approach for the pipeline:

- `{testthat}` unit tests for pure/data-transform logic (which functions, at what granularity)?
- Snapshot/regression tests for outputs — extend the existing `test/test_crime_rate_regression.R` pattern to other scripts?
- How to treat DB/LAN-coupled code: skip it in CI? mock the DB layer behind a seam? run LAN-only integration tests manually on the secure environment?

Define what "tested" concretely means for this pipeline and what coverage is achievable given the secure-environment constraint. Resolve via `/grilling` + `/domain-modeling`; record the answer as a resolution comment, then close and add a one-line gist to the map's Decisions so far.
