# Release Summary

Submission of `epocakir`, version bumped to v1.0.0. In this version:

- `units` 1.0.0 support (#45)
- `ellipsis` replaced with `rlang` (#43)

## R CMD check results

```r
rcmdcheck::rcmdcheck(args = c("--no-manual", "--as-cran"), error_on = "warning", check_dir = "check")
#> -- R CMD check results ----------------------------------- epocakir 1.0.0 ----
#> Duration: 36.7s
#>
#> 0 errors v | 0 warnings v | 0 notes v
```

## revdepcheck results

We checked 0 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

- We saw 0 new problems
- We failed to check 0 packages

## Test Environments

- Local
  - Windows 10 install: R 4.5.1 (2025-06-13 ucrt) with Rtools45

- GitHub Actions
  - macOS Sequoia 15.7.1: R 4.5.1 (2025-06-13)
  - Windows Server 2022 x64 (build 26100): R 4.5.1 (2025-06-13 ucrt) with Rtools45
  - Ubuntu 24.04.3 LTS: R-devel (2025-10-25 r88970), R 4.5.1 (2025-06-13), R 4.4.3 (2025-02-28)

- win-builder:
  - R-devel, R-release, R-oldrelease

- R-hub:
  - Windows R-devel, Ubuntu R-release, Fedora R-devel

- Checks:
  - `goodpractice::gp()`,
  - `rcmdcheck::rcmdcheck(args = c("--no-manual", "--as-cran"), error_on = "warning", check_dir = "check")`,
  - `devtools::spell_check()`,
  - `devtools::check_rhub()`,
  - `devtools::check_win_devel()`,
  - `codemetar::write_codemeta()`

## Revdep Checks

No published downstream dependencies
