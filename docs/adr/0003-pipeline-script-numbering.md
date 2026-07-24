# Pipeline script numbering: stage-based, minimal renumber

Scripts are numbered by pipeline stage: **Cleaning `01`–`14`**, **Suppression `15`–`16`**, **Delivery `17`–`18`**. We deliberately keep the established `01`–`14` per-dataset numbers — including the unused `02` slot — and renumber only the colliding late-stage scripts, to remove the `14_` and `15_` pile-ups introduced by merging the feature branches.

**Considered options**:
- Full contiguous re-sequence (`01`..`NN`, closing the `02` gap) — rejected: disrupts familiar numbers and any external references for little gain.
- Stage folders (`src/clean/`, `src/suppress/`, `src/deliver/`) — rejected: a bigger restructure than the cleanup warrants.

**Consequences**: The gap at `02` is intentional, not an oversight. New cleaning datasets take the next free number in `01`–`14`; new suppression or delivery work extends `15`+ or `17`+. `utils.R` (uppercase extension) is the canonical helper module, sourced consistently as `./src/utils.R`.
