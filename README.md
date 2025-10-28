
<!-- README.md is generated from README.Rmd. Please edit that file -->

# epocakir

*/ˈiːpɒk ə keɪ aɪ ɑː/*

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/epocakir)](https://cran.r-project.org/package=epocakir)
[![R-CMD-check](https://github.com/alwinw/epocakir/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/alwinw/epocakir/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/alwinw/epocakir/graph/badge.svg)](https://app.codecov.io/gh/alwinw/epocakir)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![Downloads](https://cranlogs.r-pkg.org/badges/epocakir)](https://cran.r-project.org/package=epocakir)
<!-- badges: end -->

## Clinical Coding of Patients with Kidney Disease

The *epocakir* package makes clinical coding of patients with kidney
disease using clinical practice guidelines easy. The guidelines used are
the evidence-based [KDIGO guidelines](https://kdigo.org/guidelines/).
This package covers acute kidney injury (AKI), anemia, and chronic
kidney disease (CKD):

- `aki_staging()`: Classification of AKI staging (`aki_stages`) with
  automatic selection of:

  - `aki_bCr()`: AKI based on baseline creatinine
  - `aki_SCr()`: AKI based on changes in serum creatinine
  - `aki_UO()`: AKI based on urine output

- `anemia()`: Classification of anemia

- Classification of albuminuria (`Albuminuria_stages`)

  - `Albuminuria_staging_ACR()`: Albuminuria based on Albumin excretion
    rate
  - `Albuminuria_staging_AER()`: Albuminuria based on
    Albumin-to-creatinine ratio

- `eGFR()`: Estimation of glomerular filtration rate with automatic
  selection of:

  - `eGFR_adult_SCr()`: eGFR based on the 2009 CKD-EPI creatinine
    equation
  - `eGFR_adult_SCysC()`: eGFR based on the 2012 CKD-EPI cystatin C
    equation
  - `eGFR_adult_SCr_SCysC()`: eGFR based on the 2012 CKD-EPI
    creatinine-cystatin C equation
  - `eGFR_child_SCr()`: eGFR based on the pediatric creatinine-based
    equation
  - `eGFR_child_SCr_BUN()`: eGFR based on the pediatric creatinine-BUN
    equation
  - `eGFR_child_SCysC()`: eGFR based on the pediatric cystatin C-based
    equation

- `GFR_staging()`: Staging of GFR (`GFR_stages`)

- Multiple utility functions including:

  - `conversion_factors`: Conversion factors used throughout the KDIGO
    guidelines
  - `as_metric()`: Conversion of a measured value into metric units
  - `dob2age()`: Calculation of age from a date of birth
  - `binary2factor()`: Conversion of binary data into factors based on a
    column name
  - `combine_date_time_cols()`: Combining separate date and time columns
    into a single date and time column
  - `combn_changes`: Generating changes between measurements

## Installation

You can install the **released** version from
[CRAN](https://cran.r-project.org/package=epocakir) with:

``` r
install.packages("epocakir")
```

You can install the **development** version from
[GitHub](https://github.com/alwinw/epocakir) with:

``` r
# install.packages("remotes")
remotes::install_github("alwinw/epocakir")
```

## Getting Started

``` r
library(epocakir)
library(dplyr)
library(units)
```

Often clinical data must be cleansed and tidied before analysis can
begin. To assist in this, several utility functions have been included.
To explore these, consider a sample clinical dataset `clinical_obvs`:

``` r
glimpse(clinical_obvs)
#> Rows: 3
#> Columns: 9
#> $ `Patient Number` <chr> "p10001", "p10002", "p10003"
#> $ `Admission Date` <chr> "2020-03-05", "2020-03-06", "2020-03-17"
#> $ `Admission Time` <chr> "14:01:00", "09:10:00", "12:48:00"
#> $ Discharge_date   <chr> "2020-03-10", "2020-03-16", "2020-03-18"
#> $ Discharge_time   <chr> "16:34:00", "18:51:00", "09:12:00"
#> $ `Date of Birth`  <chr> "1956-01-09", "1997-12-04", "1973-05-28"
#> $ Male             <lgl> TRUE, FALSE, TRUE
#> $ Height           <dbl> 182, 161, 168
#> $ Surgery          <lgl> FALSE, FALSE, TRUE

tidy_obvs <- clinical_obvs %>%
  combine_date_time_cols() %>%
  mutate(
    Age = dob2age(`Date of Birth`),
    Height = as_metric(height = set_units(as.numeric(Height), "cm"))
  ) %>%
  binary2factor(Male, Surgery)

glimpse(tidy_obvs)
#> Rows: 3
#> Columns: 8
#> $ `Patient Number`     <chr> "p10001", "p10002", "p10003"
#> $ `Admission DateTime` <dttm> 2020-03-05 14:01:00, 2020-03-06 09:10:00, 2020-03…
#> $ Discharge_DateTime   <dttm> 2020-03-10 16:34:00, 2020-03-16 18:51:00, 2020-0…
#> $ `Date of Birth`      <chr> "1956-01-09", "1997-12-04", "1973-05-28"
#> $ Male                 <ord> Male, Not_Male, Male
#> $ Height               [m] 1.82 [m], 1.61 [m], 1.68 [m]
#> $ Surgery              <ord> Not_Surgery, Not_Surgery, Surgery
#> $ Age                  <Duration> 2202768000s (~69.8 years), 880416000s (~27.9 yea…
```

Make sure to use `set_units()` from the `units` package to convert all
measurements into unit objects for automatic unit conversion in
epocakir.

## Examples

It is possible to use `aki_staging()` to automatically classify the
presence and staging of AKI. If a particular method is required, it is
possible to classify AKI using `aki_bCr()`, `aki_SCr()` or `aki_UO().`

``` r
head(aki_pt_data)
#> # A tibble: 6 × 7
#>      SCr_    bCr_ pt_id_ dttm_      UO_ aki_staging_type aki_       
#>   [mg/dl] [mg/dl] <chr>  <dttm> [ml/kg] <chr>            <ord>      
#> 1     2       1.5 <NA>   NA          NA aki_bCr          No AKI     
#> 2     2.5     1.5 <NA>   NA          NA aki_bCr          AKI Stage 1
#> 3     3       1.5 <NA>   NA          NA aki_bCr          AKI Stage 2
#> 4     3.5     1.5 <NA>   NA          NA aki_bCr          AKI Stage 2
#> 5     4       1.5 <NA>   NA          NA aki_bCr          AKI Stage 3
#> 6     4.5     1.5 <NA>   NA          NA aki_bCr          AKI Stage 3

aki_staging(aki_pt_data,
  SCr = "SCr_", bCr = "bCr_", UO = "UO_",
  dttm = "dttm_", pt_id = "pt_id_"
)
#>  [1] No AKI      AKI Stage 1 AKI Stage 2 AKI Stage 2 AKI Stage 3 AKI Stage 3
#>  [7] No AKI      No AKI      AKI Stage 1 No AKI      No AKI      AKI Stage 1
#> [13] No AKI      No AKI      No AKI      AKI Stage 1 No AKI      AKI Stage 2
#> [19] AKI Stage 3 AKI Stage 1 AKI Stage 3 AKI Stage 2 No AKI      AKI Stage 1
#> [25] AKI Stage 3 AKI Stage 3 No AKI     
#> Levels: No AKI < AKI Stage 1 < AKI Stage 2 < AKI Stage 3

aki_pt_data %>%
  mutate(aki = aki_staging(
    SCr = SCr_, bCr = bCr_, UO = UO_,
    dttm = dttm_, pt_id = pt_id_
  )) %>%
  select(pt_id_, SCr_:dttm_, aki)
#> # A tibble: 27 × 5
#>    pt_id_    SCr_    bCr_ dttm_               aki        
#>    <chr>  [mg/dl] [mg/dl] <dttm>              <ord>      
#>  1 <NA>       2       1.5 NA                  No AKI     
#>  2 <NA>       2.5     1.5 NA                  AKI Stage 1
#>  3 <NA>       3       1.5 NA                  AKI Stage 2
#>  4 <NA>       3.5     1.5 NA                  AKI Stage 2
#>  5 <NA>       4       1.5 NA                  AKI Stage 3
#>  6 <NA>       4.5     1.5 NA                  AKI Stage 3
#>  7 pt1        3.4    NA   2020-10-23 09:00:00 No AKI     
#>  8 pt1        3.9    NA   2020-10-25 21:00:00 No AKI     
#>  9 pt1        3      NA   2020-10-20 09:00:00 AKI Stage 1
#> 10 pt2        3.4    NA   2020-10-18 22:00:00 No AKI     
#> # ℹ 17 more rows

aki_pt_data %>%
  mutate(aki = aki_SCr(
    SCr = SCr_, dttm = dttm_, pt_id = pt_id_
  )) %>%
  select(pt_id_, SCr_:dttm_, aki)
#> # A tibble: 27 × 5
#>    pt_id_    SCr_    bCr_ dttm_               aki        
#>    <chr>  [mg/dl] [mg/dl] <dttm>              <ord>      
#>  1 <NA>       2       1.5 NA                  No AKI     
#>  2 <NA>       2.5     1.5 NA                  No AKI     
#>  3 <NA>       3       1.5 NA                  No AKI     
#>  4 <NA>       3.5     1.5 NA                  No AKI     
#>  5 <NA>       4       1.5 NA                  No AKI     
#>  6 <NA>       4.5     1.5 NA                  No AKI     
#>  7 pt1        3.4    NA   2020-10-23 09:00:00 No AKI     
#>  8 pt1        3.9    NA   2020-10-25 21:00:00 No AKI     
#>  9 pt1        3      NA   2020-10-20 09:00:00 AKI Stage 1
#> 10 pt2        3.4    NA   2020-10-18 22:00:00 No AKI     
#> # ℹ 17 more rows
```

Similarly, `eGFR()` offers the ability to automatically select the
appropriate formula to estimate the glomerular filtration rate. If a
particular formula is required, then `eGFR_adult_SCr`,
`eGFR_adult_SCysC`, `eGFR_adult_SCr_SCysC`, `eGFR_child_SCr`,
`eGFR_child_SCr_BUN`, or `eGFR_child_SCysC` can be used.

``` r
head(eGFR_pt_data)
#> # A tibble: 6 × 10
#>    SCr_ SCysC_  Age_ male_ black_ height_  BUN_ eGFR_calc_type_ eGFR_ pediatric_
#>   [mg/… [mg/L] [yea… <lgl> <lgl>      [m] [mg/… <chr>           [mL/… <lgl>     
#> 1   0.5   NA      20 FALSE FALSE       NA    NA eGFR_adult_SCr   139. FALSE     
#> 2  NA      0.4    20 FALSE FALSE       NA    NA eGFR_adult_SCy…  162. FALSE     
#> 3   0.5    0.4    20 FALSE FALSE       NA    NA eGFR_adult_SCr…  167. FALSE     
#> 4   0.5   NA      30 FALSE TRUE        NA    NA eGFR_adult_SCr   150. FALSE     
#> 5  NA      0.4    30 FALSE TRUE        NA    NA eGFR_adult_SCy…  155. FALSE     
#> 6   0.5    0.4    30 FALSE TRUE        NA    NA eGFR_adult_SCr…  171. FALSE

eGFR(eGFR_pt_data,
  SCr = "SCr_", SCysC = "SCysC_",
  Age = "Age_", height = "height_", BUN = "BUN_",
  male = "male_", black = "black_", pediatric = "pediatric_"
)
#> Units: [mL/(min*1.73m^2)]
#>  [1] 139.32466 161.68446 166.81886 150.52336 155.33226 171.35616 139.32466
#>  [8]  66.77365  96.41798 150.52336  64.15027  99.04045  49.63420 161.68446
#> [15]  97.06854  53.62373 155.33226  99.70870  49.63420  66.77365  56.10368
#> [22]  53.62373  64.15027  57.62964 155.99874 173.48118 178.86404 168.53768
#> [29] 166.66552 183.72895 155.99874  71.64555 103.37985 168.53768  68.83077
#> [36] 106.19167  66.06766 173.48118 116.50660  71.37808 166.66552 119.67546
#> [43]  66.06766  71.64555  67.33849  71.37808  68.83077  69.17003  99.12000
#> [50] 148.21219 165.89761

eGFR_pt_data %>%
  dplyr::mutate(eGFR = eGFR(
    SCr = SCr_, SCysC = SCysC_,
    Age = Age_, height = height_, BUN = BUN_,
    male = male_, black = black_, pediatric = pediatric_
  )) %>%
  select(SCr_:pediatric_, eGFR)
#> # A tibble: 51 × 11
#>       SCr_ SCysC_    Age_ male_ black_ height_    BUN_ eGFR_calc_type_     eGFR_
#>    [mg/dl] [mg/L] [years] <lgl> <lgl>      [m] [mg/dl] <chr>               [mL/…
#>  1     0.5   NA        20 FALSE FALSE       NA      NA eGFR_adult_SCr      139. 
#>  2    NA      0.4      20 FALSE FALSE       NA      NA eGFR_adult_SCysC    162. 
#>  3     0.5    0.4      20 FALSE FALSE       NA      NA eGFR_adult_SCr_SCy… 167. 
#>  4     0.5   NA        30 FALSE TRUE        NA      NA eGFR_adult_SCr      150. 
#>  5    NA      0.4      30 FALSE TRUE        NA      NA eGFR_adult_SCysC    155. 
#>  6     0.5    0.4      30 FALSE TRUE        NA      NA eGFR_adult_SCr_SCy… 171. 
#>  7     0.5   NA        20 FALSE FALSE       NA      NA eGFR_adult_SCr      139. 
#>  8    NA      1.2      20 FALSE FALSE       NA      NA eGFR_adult_SCysC     66.8
#>  9     0.5    1.2      20 FALSE FALSE       NA      NA eGFR_adult_SCr_SCy…  96.4
#> 10     0.5   NA        30 FALSE TRUE        NA      NA eGFR_adult_SCr      150. 
#> # ℹ 41 more rows
#> # ℹ 2 more variables: pediatric_ <lgl>, eGFR [mL/(min*1.73m^2)]

eGFR_pt_data %>%
  dplyr::mutate(eGFR = eGFR_adult_SCr(
    SCr = SCr_, Age = Age_, male = male_, black = black_
  )) %>%
  select(SCr_:pediatric_, eGFR)
#> # A tibble: 51 × 11
#>       SCr_ SCysC_    Age_ male_ black_ height_    BUN_ eGFR_calc_type_     eGFR_
#>    [mg/dl] [mg/L] [years] <lgl> <lgl>      [m] [mg/dl] <chr>               [mL/…
#>  1     0.5   NA        20 FALSE FALSE       NA      NA eGFR_adult_SCr      139. 
#>  2    NA      0.4      20 FALSE FALSE       NA      NA eGFR_adult_SCysC    162. 
#>  3     0.5    0.4      20 FALSE FALSE       NA      NA eGFR_adult_SCr_SCy… 167. 
#>  4     0.5   NA        30 FALSE TRUE        NA      NA eGFR_adult_SCr      150. 
#>  5    NA      0.4      30 FALSE TRUE        NA      NA eGFR_adult_SCysC    155. 
#>  6     0.5    0.4      30 FALSE TRUE        NA      NA eGFR_adult_SCr_SCy… 171. 
#>  7     0.5   NA        20 FALSE FALSE       NA      NA eGFR_adult_SCr      139. 
#>  8    NA      1.2      20 FALSE FALSE       NA      NA eGFR_adult_SCysC     66.8
#>  9     0.5    1.2      20 FALSE FALSE       NA      NA eGFR_adult_SCr_SCy…  96.4
#> 10     0.5   NA        30 FALSE TRUE        NA      NA eGFR_adult_SCr      150. 
#> # ℹ 41 more rows
#> # ℹ 2 more variables: pediatric_ <lgl>, eGFR [mL/(min*1.73m^2)]
```

## References

KDIGO Guidelines - <https://kdigo.org/guidelines/>

## Activity

![GitHub commit
activity](https://img.shields.io/github/commit-activity/m/alwinw/epocakir)
![GitHub last
commit](https://img.shields.io/github/last-commit/alwinw/epocakir)
![Visits](https://badges.pufler.dev/visits/alwinw/epocakir?&label=visits)
![GitHub repo size in
bytes](https://img.shields.io/github/repo-size/alwinw/epocakir) ![Total
Lines](https://img.shields.io/tokei/lines/github/alwinw/epocakir)

------------------------------------------------------------------------

See <https://alwinw.github.io/epocakir/reference/index.html> for more
usage details and package reference.
