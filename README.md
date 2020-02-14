
# pRoductivity

<!-- badges: start -->
<!-- badges: end -->

The goal of pRoductivity is to ...

## Installation



## Data

Dataset are combined in data-raw/main_data.R

Datasets:
 * data_main, data with main industry classification plus C26.
 * data_main_groups, data with main groupings

### OECD data

#### Annual national accounts

Missin data:
 Japan:
- EMP_DC__THS_HW missing: based on SAL_DC ?
- D1__CP_NAC M before 2004
- N is missing (in STAN M-N)
- B1G__CLV_NAC and B1G__CLV_NAC for  26

CH:
- Hours data is missing from all industries
- D1 all missing

CA:
- Only from 2007

USA:
- EMP_DC__THS_HW missing: based on SAL_DC ?
- from 1997, industries from 1998
- persons from 2000, jobs and hours from 1998 (in stan also for services from 1998)

New Zealand:
- Constant prices series (and more) missing for industies. (in STAN, but only with t-4 years)

Imputed values:

 * Employment data is extrapolated using linear reggression for USA and Japan based on jobs data. For USA 1997-? and for Japan 2015-.
 * USA industries 1997 based on total (TODO)
 * EMP_DC__THS_HW is approximated with EMP_DC__THS_PER x SAL_DC__THS_HW / SAL_DC__THS_PER for USA and JAPAN.
