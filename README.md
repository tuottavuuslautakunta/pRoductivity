
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pRoductivity

<!-- badges: start -->
<!-- badges: end -->

pRoductivity is a packege for Finnish Productivity Board data management
and figure production.

## Installation

You can install pRoductivity from [GitHub](https://github.com/) with:

``` r
devtools::install_github("tuottavuuslautakunta/pRoductivity")
```

``` r

library(pRoductivity)
```

## Data

Datasets are combined with data-raw/main_data.R Eurostat data is updated
with data-raw/get_eurostat_data_10.R and data-raw/get_eurostat_data.R
OECD data is updated with data-raw/get_oecd_sna.R

### Main datasets

Detailed data:

- data_main, data from with main industry classification plus C26 (“C”,
  “C26”, “F”, “G”, “H”, “I”, “J”, “M”, “N”, “TOTAL”).

``` r

str(data_main)
#> tibble [8,044 × 12] (S3: tbl_df/tbl/data.frame)
#>  $ geo            : Factor w/ 28 levels "AT","BE","BG",..: 1 1 1 1 1 1 1 1 1 1 ...
#>  $ time           : num [1:8044] 1995 1995 1995 1995 1995 ...
#>  $ nace_r2        : Factor w/ 10 levels "C","C26","F",..: 1 2 3 4 5 6 7 8 9 10 ...
#>  $ B1G__CLV15_MNAC: num [1:8044] 35992 2393 21447 26612 13392 ...
#>  $ B1G__CP_MNAC   : num [1:8044] 31623 2333 12695 20557 8749 ...
#>  $ D1__CP_MNAC    : num [1:8044] 20214 1409 7844 12132 5850 ...
#>  $ B1G__PYP_MNAC  : num [1:8044] 30926 2287 12451 20545 8949 ...
#>  $ EMP_DC__THS_HW : num [1:8044] 1131998 61019 466732 929816 381411 ...
#>  $ SAL_DC__THS_HW : num [1:8044] 1090024 59874 431314 794767 359709 ...
#>  $ EMP_DC__THS_PER: num [1:8044] 673 36 281 550 200 ...
#>  $ SAL_DC__THS_PER: num [1:8044] 655 35.6 266.8 497.3 192.3 ...
#>  $ geo_name       : Factor w/ 28 levels "Itävalta","Belgia",..: 1 1 1 1 1 1 1 1 1 1 ...

range(data_main$time)
#> [1] 1995 2023

levels(data_main$nace_r2)
#>  [1] "C"     "C26"   "F"     "G"     "H"     "I"     "J"     "M"     "N"    
#> [10] "TOTAL"
```

- data_main_groups, data with main groupings private, private_ex26,
  manu, manu_ex26, service

``` r

str(data_main_groups)
#> tibble [4,060 × 18] (S3: tbl_df/tbl/data.frame)
#>  $ geo            : Factor w/ 28 levels "AT","BE","BG",..: 1 1 1 1 1 1 1 1 1 1 ...
#>  $ geo_name       : Factor w/ 28 levels "Euroalue-12",..: 28 28 28 28 28 28 28 28 28 28 ...
#>  $ nace0          : Factor w/ 5 levels "private","private_ex26",..: 1 1 1 1 1 1 1 1 1 1 ...
#>  $ time           : num [1:4060] 1995 1996 1997 1998 1999 ...
#>  $ B1G__CLV15_MNAC: num [1:4060] 128109 131183 133721 139358 144271 ...
#>  $ B1G__CP_MNAC   : num [1:4060] 94428 97061 100293 105233 109475 ...
#>  $ B1G__PYP_MNAC  : num [1:4060] 93092 96772 99109 104497 109111 ...
#>  $ D1__CP_MNAC    : num [1:4060] 56068 56519 57597 60029 62326 ...
#>  $ EMP_DC__THS_HW : num [1:4060] 3828240 3911107 3974389 3994668 4087364 ...
#>  $ EMP_DC__THS_PER: num [1:4060] 2197 2208 2234 2264 2311 ...
#>  $ SAL_DC__THS_HW : num [1:4060] 3357252 3373661 3416789 3429746 3500598 ...
#>  $ SAL_DC__THS_PER: num [1:4060] 2013 2003 2027 2049 2092 ...
#>  $ b1g__clv10_mnac: num [1:4060] 114642 117488 119967 124996 129603 ...
#>  $ lp_10          : num [1:4060] 0.0299 0.03 0.0302 0.0313 0.0317 ...
#>  $ lp_ind         : num [1:4060] 78.6 78.9 79.3 82.2 83.3 ...
#>  $ va_ind         : num [1:4060] 69.5 71.2 72.7 75.8 78.6 ...
#>  $ h_ind          : num [1:4060] 88.4 90.3 91.7 92.2 94.4 ...
#>  $ emp_ind        : num [1:4060] 87.6 88 89 90.2 92.1 ...

range(data_main_groups$time)
#> [1] 1995 2023

levels(data_main_groups$nace0)
#> [1] "private"      "private_ex26" "manu"         "manu_ex26"    "service"
```

Data with shorter publication lag:

- data_main10, data with aggregate industry classification (“C”, “F”,
  “G-I”, “J”, “M_N”, “TOTAL”).

``` r

str(data_main10)
#> tibble [4,832 × 12] (S3: tbl_df/tbl/data.frame)
#>  $ geo            : Factor w/ 28 levels "AT","BE","BG",..: 1 1 1 1 1 1 1 1 1 1 ...
#>  $ time           : num [1:4832] 1995 1995 1995 1995 1995 ...
#>  $ nace_r2        : Factor w/ 6 levels "C","F","G-I",..: 1 2 3 4 5 6 1 2 3 4 ...
#>  $ B1G__CLV15_MNAC: num [1:4832] 35992 21447 51718 5415 13122 ...
#>  $ B1G__CP_MNAC   : num [1:4832] 31623 12695 35559 5338 9213 ...
#>  $ D1__CP_MNAC    : num [1:4832] 20214 7844 21150 2614 4245 ...
#>  $ B1G__PYP_MNAC  : num [1:4832] 30926 12451 35393 5326 8996 ...
#>  $ EMP_DC__THS_HW : num [1:4832] 1131998 466732 1721343 120985 387182 ...
#>  $ SAL_DC__THS_HW : num [1:4832] 1090024 431314 1456680 105569 273664 ...
#>  $ EMP_DC__THS_PER: num [1:4832] 673.3 280.9 961.5 68.1 213.2 ...
#>  $ SAL_DC__THS_PER: num [1:4832] 655 266.8 863.2 61.6 166.5 ...
#>  $ geo_name       : Factor w/ 28 levels "Itävalta","Belgia",..: 1 1 1 1 1 1 1 1 1 1 ...

range(data_main10$time)
#> [1] 1995 2023

levels(data_main10$nace_r2)
#> [1] "C"     "F"     "G-I"   "J"     "M_N"   "TOTAL"
```

- data_main10_groups, data with main groupings private, manu, service

``` r

str(data_main10_groups)
#> tibble [2,436 × 17] (S3: tbl_df/tbl/data.frame)
#>  $ geo            : Factor w/ 28 levels "AT","BE","BG",..: 1 1 1 1 1 1 1 1 1 1 ...
#>  $ geo_name       : Factor w/ 28 levels "Euroalue-12",..: 28 28 28 28 28 28 28 28 28 28 ...
#>  $ nace0          : Factor w/ 3 levels "private","manu",..: 1 1 1 1 1 1 1 1 1 1 ...
#>  $ time           : num [1:2436] 1995 1996 1997 1998 1999 ...
#>  $ B1G__CP_MNAC   : num [1:2436] 94428 97061 100292 105233 109474 ...
#>  $ B1G__PYP_MNAC  : num [1:2436] 93092 96772 99109 104497 109111 ...
#>  $ D1__CP_MNAC    : num [1:2436] 56068 56519 57597 60029 62326 ...
#>  $ EMP_DC__THS_HW : num [1:2436] 3828240 3911107 3974389 3994667 4087364 ...
#>  $ EMP_DC__THS_PER: num [1:2436] 2197 2208 2234 2264 2311 ...
#>  $ SAL_DC__THS_HW : num [1:2436] 3357251 3373662 3416790 3429746 3500597 ...
#>  $ SAL_DC__THS_PER: num [1:2436] 2013 2003 2027 2049 2092 ...
#>  $ b1g__clv10_mnac: num [1:2436] 114642 117487 119966 124996 129602 ...
#>  $ lp_10          : num [1:2436] 0.0299 0.03 0.0302 0.0313 0.0317 ...
#>  $ lp_ind         : num [1:2436] 78.6 78.9 79.3 82.2 83.3 ...
#>  $ va_ind         : num [1:2436] 69.5 71.2 72.7 75.8 78.6 ...
#>  $ h_ind          : num [1:2436] 88.4 90.3 91.7 92.2 94.4 ...
#>  $ emp_ind        : num [1:2436] 87.6 88 89 90.2 92.1 ...

range(data_main10_groups$time)
#> [1] 1995 2023

levels(data_main10_groups$nace0)
#> [1] "private" "manu"    "service"
```

### Eurostat data

#### Annual national accounts

From: \* National accounts aggregates by industry (up to NACE A*64)
(nama_10_a64) and nama_10_a10  
* National accounts employment data by industry (up to NACE A\*64)
(nama_10_a64_e) and nama_10_a10e

##### Misssing dat_eurostat_nace10_imput;

- Estonia Hours for 1995-1999. Imputed based on persons and 2000.
- Belgian Hours for 2019. Imputed based on persons and 2018.

##### Missing dat_eurostat_nace_imput:

t-1 for many

Hours based on previous or following hours per persons.

UK: \* Industry data from t-2

Euroarea \* Only from 2000

### OECD data

#### Annual national accounts

Missin data: Japan: - EMP_DC\_\_THS_HW missing: based on SAL_DC ? -
D1\_\_CP_NAC M before 2004 - N and M is missing (in STAN M-N) -
B1G\_\_CLV_NAC and B1G\_\_CLV_NAC for 26

CH: - Hours data is missing from all industries - D1 all missing

CA: - Only from 2007

USA: - EMP_DC\_\_THS_HW missing: based on SAL_DC ? - from 1997,
industries from 1998 - persons from 2000, jobs and hours from 1998 (in
stan also for services from 1998)

New Zealand: - Constant prices series (and more) missing for industies.
(in STAN, but only with t-4 years)

Imputed values:

- Employment data is extrapolated using linear reggression for USA and
  Japan based on jobs data. For USA 1997-? and for Japan 2015-.
- USA industries 1997 based on total (TODO)
- EMP_DC\_\_THS_HW is approximated with EMP_DC\_\_THS_PER x
  SAL_DC\_\_THS_HW / SAL_DC\_\_THS_PER for USA and JAPAN.
- M and N have been dropped for Japan due to missing values

# Weighting data

Countries used in weighting (from weight_geos): “AT”, “BG”, “CZ”, “DE”,
“DK”, “EE”, “EL”, “ES”, “FI”, “BE”, “HU”, “LT”, “FR”, “IT”, “NL”, “NO”,
“PT”, “SE”, “CY”, “PL”, “SI”, “SK”

# Prodtivity levels

- data_main10_groups_level

Data from [The GGDC Productivity Level
Database](https://www.rug.nl/ggdc/productivity/pld/) 2005 benchmark is
used to set productivity levels on benchmark year.
