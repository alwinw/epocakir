# Resubmission

Resubmission of `epocakir`, version bumped to v0.9.7. In this version:

- Added reference link in the description field of the DESCRIPTION file
- Reduced the length of the title to less than 65 characters
- Added \value to .Rd files regarding exported methods and explain the functions results in the documentation
  - aki_staging.Rd: Added return value
  - pipe.Rd: Removed code that re-exported %>% and used @importFrom instead

## R CMD check results

> checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Alwin Wang <alwin.wang@austin.org.au>'

  New submission

0 errors v | 0 warnings v | 1 note x

- Note due to new submission
- Note possibly mis-spelled words 'AKI', 'CKD', 'KDIGO', are all medical abbreviations spelt correctly

## Test Environments

- Local Windows 10 install: R 4.0.3

- macOS Catalina (GitHub Actions): R 4.1.0
- Windows Server (GitHub Actions): R 4.1.0, R 3.6.3,
- Ubuntu 20.04.2 (GitHub Actions): R-devel, 4.1.0
- Ubuntu 16.04.7 (GitHub Actions): R 3.5.3

- win-builder: R-devel, R-release, R-oldrelease
- R-hub: Windows R-devel, Ubuntu R-release, Fedora R-devel

- Checks: `devtools::spell_check()`, `devtools::check_rhub()`, `devtools::check_win_devel()`, `goodpractice::gp()`,
          `rcmdcheck::rcmdcheck(args = c("--no-manual", "--as-cran"), error_on = "warning", check_dir = "check")`

## Revdep Checks

No published downstream dependencies
