# DOE Orthogonal `extdata` Standards

This directory stores canonical, runtime data for Taguchi/Tsui orthogonal-array support.

## File Types

- **OA matrices**
  - Primary format: `L*.OA` (space-delimited with header line `n_runs n_cols`)
  - Exception: `L81.csv` (comma-delimited, no header; `81 x 40`)
- **Interaction/Triangle tables**
  - `triangle_table_power_of_2.csv`: Taguchi 2-level interaction triangle
  - `L81_interaction_table.csv`: 3-level L81 two-factor interaction mapping

## Required Invariants

- `L4.OA` = `4 x 3`
- `L8.OA` = `8 x 7`
- `L12.OA` = `12 x 11`
- `L16.OA` = `16 x 15`
- `L18.OA` = `18 x 8`
- `L27.OA` = `27 x 13`
- `L32.OA` = `32 x 31`
- `L64.OA` = `64 x 63`
- `L9.OA` = `9 x 4`
- `L81.csv` = `81 x 40`

- `L81_interaction_table.csv` must include columns: `i`, `j`, `col_a`, `col_b`
  - exactly `780` unique pairs (`40 choose 2`) where `i < j`
  - all indices in `1..40`
  - each pair maps to exactly two output columns (`col_a`, `col_b`)

## Design Rule

- `L27` interaction mapping is treated as a subset of `L81` (`columns 1..13`) and should not be maintained as a duplicate source table.

## Validation

Use:

- `modules/statistical/doe_orthogonal/tools/validate_extdata.R`

to validate OA dimensions and interaction-table integrity.

