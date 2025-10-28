# Contributing

Welcome! Happy to have you here. Thank you in advance for your contribution and interest in `epocakir`.

## The Basics

Contributions are welcome in the form of pull requests.

## Prerequisites

`epocakir` is written in pure R. You'll need the [R>=4.0][1] and [dev packages][2]. Currently R Tools is not required.

```R
install.packages(c("devtools", "roxygen2", "lintr", "styler", "testthat", "knitr"))
```

## Project Structure

`epocakir` is a standard, single [R package][3] with R code in the [`R/` directory][4], data in the [`data/`
directory][5] and testing using [testthat][6].

```ascii
epocakir/
├── .github/                # GitHub Actions
├── R/                      # R source code
│   ├── aki.R
│   ├── ...
│
├── data/                   # .rda Data files
│   ├── aki_pt_data.rda
│   ├── ...
│
├── data-raw/               # Data origin
│   ├── aki-data.R
│   ├── ...
│
├── man/                    # Docs auto-generated with roxygen
│   ├── aki_bCr.Rd
│   ├── ...
│
├── tests/                  # Testing
│   ├── testthat.R
│   └── testthat
│       ├── test-aki.R
│       ├── ...
│
├── cran-comments.md        # CRAN submission comments
├── DESCRIPTION             # Package metadata
├── NAMESPACE               # Auto-generated with roxygen
├── README.Rmd              # Package README
├── README.md               # Auto-generated with knitr
├── ...
```

## Development

### Running Tests

If you're using RStudio, press Cmd/Ctrl + Shift + T (or run `devtools::test()` if not) to run all the tests in a
package.

```R
devtools::test()
#> ℹ Testing epocakir
#> ✔ | F W  S  OK | Context
#> ✔ |         24 | aki
#> ✔ |          6 | anemia
#> ✔ |        529 | ckd [1.0s]
#> ✔ |         44 | utils
#>
#> ══ Results ════════════════════════════
#> Duration: 2.1 s
#>
#> [ FAIL 0 | WARN 0 | SKIP 0 | PASS 603 ]
```

## Documentation

### Adding Documentation

Documentation is generated from the source code using [roxygen2][7]. To add documentation to a function, add a [comment
block][8] above the function.

### Building Documentation

To build the documentation, run the following command in the R console:

```R
devtools::document()
```

To build the [pkgdown website][9] run the following command in the R console:

```R
pkgdown::build_site()
```

## Release

Set up a GitHub [personal access token for the repository][10] and create a GitHub issue with a [checklist][11].
Run through the checks as required, and additional ones below:

```R
# Creat new GitHub issue
usethis::use_release_issue()
# Run additional checks
devtools::spell_check()
urlchecker::url_check()
rhub::check_for_cran()
rcmdcheck::rcmdcheck(args = c("--no-manual", "--as-cran"), error_on = "warning", check_dir = "check")
revdepcheck::revdep_check(num_workers = 4)
devtools::check_rhub()
devtools::check_mac_release()
devtools::check_win_devel()
```

Increment the version:

<!-- References -->

[1]: https://www.r-project.org/
[2]: https://r-pkgs.org/setup.html
[3]: https://r-pkgs.org/structure.html
[4]: https://r-pkgs.org/code.html
[5]: https://r-pkgs.org/data.html
[6]: https://testthat.r-lib.org/
[7]: https://roxygen2.r-lib.org/index.html
[8]: https://r-pkgs.org/man.html
[9]: https://pkgdown.r-lib.org/
[10]: https://happygitwithr.com/https-pat#get-a-pat
[11]: https://r-pkgs.org/release.html
