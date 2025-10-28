# epocakir 1.0.0

## Bug Fixes

- `units` 1.0.0 support (#45)

## Breaking Changes

- `ellipsis` replaced with `rlang` (#43)

# epocakir 0.9.9

## Bug Fixes

- Use correctly typed missing values for upcoming dplyr 1.1.0 (#41)

# epocakir 0.9.8

Update README and formatting

## Bug Fixes

- Update readme and description
- Addressed comments from `goodpractice::gp()`

# epocakir 0.9.7

CRAN re-submission

## Bug Fixes

- Added reference link in the description field of the DESCRIPTION file
- Reduced the length of the title to less than 65 characters
- aki_staging.Rd: Added @return value explaining the functions results
- pipe.Rd: Removed code that re-exported %>% and used @importFrom instead

# epocakir 0.9.6

Initial CRAN submission

## Improvements

- Added vignette *Introduction to epocakir* with basic usage examples
- Updated README
- Added sample `clinical_obvs` to demonstrate utility functions

## Bug fixes

- Exported `conversion_factors`, similar to ordered factors like `aki_stages`
- Corrected mis-spelled words

# epocakir 0.9.5

Added sample data and better examples

## Improvements

- Added sample data based on test data
- Cleaned up code comments
- Added examples to methods

## Breaking Changes

- Renamed `aki()` to `aki_staging()` for more consistent method names

## Bug fixes

- Fixed bug when n < m for combn() in combn_changes()
- Changed methods from `method.default()` to `method.data.frame()` to prevent infinite recursion on incorrectly specified function arguments

# epocakir 0.9.0

Improved high level API functions

## Improvements

- Wrote new combined `eGFR()` function that automatically selects the appropriate `eGFR_method()` formula to use
- Wrote new combined `aki()` function that automatically calculates AKI based on all criteria and determines most severe stage
- Added additional tests including warnings

## Breaking Changes

- Added `No AKI` and `No Albuminuria` to stages ordered factors to prevent users have to handle `NAs` mixed with ordered factors

## Bug fixes

- Fixed bug in `eGFR_internal()` where it was looking for `!is.na(black)` to select `eGFR_adult_SCysC()`, when `black` was not required

# epocakir 0.8.0

Initial full-featured release of epocakir methods

## Improvements

- Wrote `aki_SCr()` function
- Methods now include `aki_bCr`, `aki_SCr`, `aki_UO`, `anemia`, `eGFR_adult_SCr`, `eGFR_adult_SCysC`, `eGFR_adult_SCr_SCysC`, `eGFR_child_SCr`, `eGFR_child_SCr_BUN`, `eGFR_child_SCysC`, `GFR_staging`, `Albuminuria_staging_AER`, `Albuminuria_staging_ACR` and `combn_changes`

## Bug fixes

- Used `tibble::tibble()` instead of `data.frame()` to prevent issue with `stringsasfactors` difference between R 3.x and R 4.x
- Fixed sorting error in `aki_UO()

# epocakir 0.7.0

Re-wrote methods for consistency

## Improvements

- Removed duplicate `GFR_staging()` code
- Stricter `expect_identical()` instead of `expect_equal()` assertions
- Rewrote `anemia()` to make method selection consistent with other functions

## Bug fixes

- Consistent parameter naming

# epocakir 0.6.0

Re-wrote all eGFR functions as S3 methods

## Improvements

- Wrote `eGFR_adult_SCr`, `eGFR_adult_SCysC`, `eGFR_adult_SCr_SCysC`, `eGFR_child_SCr`, `eGFR_child_SCr_BUN`, `eGFR_child_SCysC` methods

## Breaking changes

- Instead of individual functions, use methods for all low-level functions
- Renamed functions to snake_case, e.g. `eGFR.adult.SCr_SCysC` to `eGFR_adult_SCr_SCysC`

# epocakir 0.5.0

Improved handling of function arguments

## Improvements

- Changed from `forcats::fct_c` to `vctrs::vec_c` for improved `NA` handling (<https://github.com/tidyverse/forcats/issues/250>)
- Wrote `aki_bCr()` method
- Allow symbols to be passed to methods

## Breaking changes

- Added version numbers to dependencies

# epocakir 0.4.0

Added testing and additional utility functions to package

## Improvements

- Testing of edge-cases for `as_metric(NULL)`, `as_metric(1)` and unknown measurements
- Wrote `as_metric()`
- Additional KDIGO guidelines (<https://kdigo.org/guidelines/>) added
- Added Albuminuria levels as an ordered factor

## Bug fix

- Correctly use `pmin` instead of `min` in eGFR calculation

# epocakir 0.1.5

dplyr compatible functions

## Improvements

- Proper handling of column names using dplyr programming (<https://dplyr.tidyverse.org/articles/programming.html>)
- Wrote `aki_bCr` calculation as part of `aki()` staging
- Wrote `.dob2age()` function to calculate a patient's age

# epocakir 0.1.0

Initial release

## Improvements

- Wrote `combn_changes()`
- Wrote `conversion_factors()`
- Wrote initial functions based on KDIGO guidelines (<https://kdigo.org/guidelines/>)
