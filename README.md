
<!-- README.md is generated from README.Rmd. Please edit that file -->

# lsatTS - an R package to make sense of Landsat greeneness time-series

<!-- badges: start -->

<!-- badges: end -->

**Logan T. Berner, Jakob J. Assmann, Richard Massey (TBC), Signe Normand
and Scott Goetz**

Building upon the workflow developed for [Berner et
al. 2020](https://www.nature.com/articles/s41467-020-18479-5) the
**lsatTS** package facilitates:

  - The export of time-series for the whole Landsat record from point
    coordinates (“sites”).
  - The cross-calibration of VIs derived from the time-series to account
    for sparse observations.
  - The definition of growing season characteristics such as the maximum
    NDVI.

## Content

1.  [Installation](#1-installation)
2.  [Preparation and extraction](#2-preparation-and-extraction%5D)
3.  [Cross calibration of
    time-series](#3-cross-calibration-of-time-series)
4.  [Defining growing season
    characteristics](#4-defining-growing-season-characteristics)
5.  [Citation](#5-citation)
6.  [Contact](#6-contact)
7.  [Contributions](#7-contributions)
8.  [References](#8-references)
9.  [License](#9-license)

[To top](#Content)

## 1\. Installation

You can install the package using `devtools` as follows:

``` r
# install.packages("devtools")
devtools::install_github("jakobjassmann/lsatTS")
```

[To top](#Content)

## 2\. Preparation and extraction

![](man/figures/Landsat%20R%20Package%20-%20Preparation%20and%20Extraction.jpg)

[To top](#Content)

## 3\. Cross-calibration of time-series

![](man/figures/Landsat%20R%20Package%20-%20Cross-calibration%20of%20time-series.jpg)

[To top](#Content)

## 4\. Defining growing season characteristics

![](man/figures/Landsat%20R%20Package%20-%20Defining%20growing%20season%20characteristics.jpg)

[To top](#Content)

## 5\. Citation

When using this package please citing:

Berner, Logan T., Richard Massey, Patrick Jantz, Bruce C. Forbes, Marc
Macias-Fauria, Isla Myers-Smith, Timo Kumpula, et al. 2020. Summer
Warming Explains Widespread but Not Uniform Greening in the Arctic
Tundra Biome. Nature Communications 11, no. 1: 4621.
<https://doi.org/10.1038/s41467-020-18479-5>.

[To top](#Content)

## 6\. Contact

Logan T. Berner and Jakob J. Assmann Email Logan at:
<Logan.Berner@nau.edu> Email Jakob at: <j.assmann@bio.au.dk>

[To top](#Content)

## 7\. Contributions

Logatn T. Berner wrote the analysis functions (items 3 and 4). Jakob J.
Assmann wrote the extraction and preparation functions (item 1). Richard
Massey wrote the original Python code for the `lsat_export_ts()`
function, later refined and transferred to JavaScript and R by Jakob
Assmann. Singe Normand and Scoet Goetz provided funding (?) and
mentorship for this project. Jakob J. Assmann faciliated package
development. Logan T Berner and Jakob J. Assmann prepared the code for
publication and wrote the documnetation.

Thank you to all the testers: Tester A, Tester B and Tester C.

[To top](#Content)

## 8\. References

**rgee**

C Aybar, Q Wu, L Bautista, R Yali and A Barja (2020) rgee: An R package
for interacting with Google Earth Engine Journal of Open Source Software
URL <https://github.com/r-spatial/rgee/>.

## 9\. License

[MIT LICENSE](LICENSE)
