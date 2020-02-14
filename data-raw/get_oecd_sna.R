# OECD annual national account data

library(OECD)
library(tidyverse)
library(countrycode)

devtools::load_all()

# dataset_list <- get_datasets()

# dataset_list %>%
#   filter(id %in% c("SNA_TABLE6A", "SNA_TABLE7A")) %>%
#   knitr::kable()
#
# sna6a_str <- get_data_structure("SNA_TABLE6A")
# sna7a_str <- get_data_structure("SNA_TABLE7A")
#
# sna6a_str$ACTIVITY


# Countries
sna_geo <- countrycode(c("US", "JP"), "eurostat", "iso3c")

# Industries
sna_activity <- setNames(names(main_nace_sna), main_nace_sna)

# Transactions
sna6a_transact <- c(
  B1G = "B1GA", #"Gross Value added",
  D1 = "D1A" #"Compensation of employees, total",
)

sna7a_transact <- c(
  EMP_DC = "ETOA", # "Total empoyment",
  SAL_DC = "EEMA" #"Employees",
)


# Measeures
sna_measures <-   c(
  CP_MNAC = "C",   # current prices
  CLV15_MNAC = "V",  # Constant prices
  PYP_MNAC = "VP" # previous year prices
)

sna7a_measures <-   c(
  THS_PER = "PER",   # Persons
  THS_HW = "HRS",   # Hours
  THS_JOBS = "JOB"  # JObs
)


dat_oecd_sna6a_0 <- get_dataset(dataset = "SNA_TABLE6A",
                                filter = list(sna_geo, sna6a_transact, sna_activity, sna_measures))

dat_oecd_sna7a_0 <- get_dataset(dataset = "SNA_TABLE7A",
                                filter = list(sna_geo, sna7a_transact, sna_activity, sna7a_measures))




dat_oecd_sna6a <- dat_oecd_sna6a_0 %>%
  transmute(
    time = as.numeric(obsTime),
    geo = as_factor(countrycode(LOCATION, "iso3c", "eurostat", nomatch = NULL)),
    nace_r2 = fct_recode(ACTIVITY, !!!sna_activity),
    na_item = fct_recode(TRANSACT, !!!sna6a_transact),
    unit = fct_recode(MEASURE, !!!sna_measures),
    currency = as_factor(UNIT),
    values = obsValue)  %>%
  unite(vars, na_item, unit, sep = "__") %>%
  mutate(vars = as_factor(vars)) %>%
  spread(vars, values)

dat_oecd_sna7a <- dat_oecd_sna7a_0 %>%
  transmute(
    time = as.numeric(obsTime),
    geo = as_factor(countrycode(LOCATION, "iso3c", "eurostat", nomatch = NULL)),
    nace_r2 = fct_recode(ACTIVITY, !!!sna_activity),
    na_item = fct_recode(TRANSACT, !!!sna7a_transact),
    unit = fct_recode(MEASURE, !!!sna7a_measures),
    values = obsValue) %>%
  unite(vars, na_item, unit, sep = "__") %>%
  mutate(vars = as_factor(vars)) %>%
  spread(vars, values)

dat_oecd_sna_nace <-
  dat_oecd_sna6a %>%
  left_join(dat_oecd_sna7a, by = c("time", "geo", "nace_r2")) %>%
  filter(time >= start_year) %>%
  complete(time, geo, nace_r2)


dat_oecd_sna_nace_imput <-
  dat_oecd_sna_nace %>%
  # Extrapolate emp based on jobs
  nest(data = c(-geo, -nace_r2)) %>%
  mutate(emp_mod = map(data, possibly(~lm(EMP_DC__THS_PER ~ EMP_DC__THS_JOBS, data = .x), otherwise = NA_real_)),
         emp_approx = map2(emp_mod, data, possibly(~predict(.x, .y), otherwise = NA_real_)),
         sal_mod = map(data, possibly(~lm(SAL_DC__THS_PER ~ SAL_DC__THS_JOBS, data = .x), otherwise = NA_real_)),
         sal_approx = map2(sal_mod, data, possibly(~predict(.x, .y), otherwise = NA_real_))) %>%
  select(-emp_mod, -sal_mod) %>%
  unnest(cols = c(data, emp_approx, sal_approx)) %>%
  mutate(EMP_DC__THS_PER = coalesce(EMP_DC__THS_PER, emp_approx),
         SAL_DC__THS_PER = coalesce(SAL_DC__THS_PER, sal_approx)) %>%
  select(-emp_approx, -sal_approx) %>%
  # Approx EMP_DC__THS_HW base on EMP_DC__THS_PER and SAL_DC__THS_HW and SAL_DC__THS_PER
  mutate(EMP_DC__THS_HW = if_else(is.na(EMP_DC__THS_HW), EMP_DC__THS_PER * SAL_DC__THS_HW / SAL_DC__THS_PER, EMP_DC__THS_HW)) %>%
  # drop M and N for Japan for missing
  filter(!(geo == "JP" & nace_r2 %in% c("M", "N")))

# visdat::vis_dat(dat_oecd_sna_nace_imput)

# filter(dat_oecd_sna_nace_imput, geo == "JP") %>%
#   filter(time != "2018") %>%
#   filter(nace_r2 != "C26") %>%
#   filter(nace_r2 != "N") %>%
#   filter(!(nace_r2 == "M" & time < 2004))  %>%
#   visdat::vis_dat()
#
# filter(dat_oecd_sna_nace_imput, geo == "US") %>%
#   filter(time != "2018", time != 1997) %>%
#   # filter(nace_r2 != "C26") %>%
#   # filter(nace_r2 != "N") %>%
#   # filter(!(nace_r2 == "M" & time < 2004)) %>%
#   visdat::vis_dat()


usethis::use_data(dat_oecd_sna_nace, dat_oecd_sna_nace_imput, overwrite = TRUE)
