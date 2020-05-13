
# pRoductivity

<!-- badges: start -->
<!-- badges: end -->

The goal of pRoductivity is to ...

## Installation



## Data

Datasets are combined in data-raw/main_data.R

Datasets:
Detailed:
 * data_main, data from with main industry classification plus C26.
 * data_main_groups, data with main groupings private, private_ex26, manu, manu_ex26, service
Shorter publication lag:
 * data_main10, data with aggregate industry classification.
 * data_main10_groups, data with main groupings private, private_ex26, manu, manu_ex26, service
 



### Eurostat data

#### Annual national accounts

From:
* National accounts aggregates by industry (up to NACE A*64) (nama_10_a64) and nama_10_a10	 
* National accounts employment data by industry (up to NACE A*64) (nama_10_a64_e) and nama_10_a10e

##### Misssing dat_eurostat_nace10_imput;

Estonia
* Hours for 1995-1999. Imputed based on persons and 2000.

##### Missing dat_eurostat_nace_imput:

t-1 for many

Sweden:
* J 2015 employment data. An error as series is different form nama_10_a10_e. Imputed by 2014 and 2016 mean.

UK:
* Industry data from t-2

Euroarea
* Only from 2000

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
 * M and N have been dropped for Japan due to missing values


## Simple Syntethic Control Analysis


