# OECD annual national account data

library(OECD)
library(tidyverse)
library(countrycode)

devtools::load_all()

dataset_list <- get_datasets()

dataset_list %>%
  filter(id %in% c("SNA_TABLE1", "SNA_TABLE6A", "SNA_TABLE7A")) %>%
  knitr::kable()
sna1_str <- get_data_structure("SNA_TABLE1")
sna6a_str <- get_data_structure("SNA_TABLE6A")
# sna7a_str <- get_data_structure("SNA_TABLE7A")
#
# sna6a_str$ACTIVITY


# Countries
sna_geo <- countrycode(c("US", "JP"), "eurostat", "iso3c")

# Industries
sna_activity <- setNames(names(main_nace_sna_new), main_nace_sna_new)

# Transactions

sna1_transact <- c(
  B1GQ = "B1_GA", #"Gross domestic product",
  D1 = "D11", #"Compensation of employees, total",
  P6 = "P6", #"Exports of goods and services",
  P61 = "P61", #"Exports of goods",
  P62 = "P62", #"Exports of services",
  B11 = "B11") #"External balance of goods and services"

sna6a_transact <- c(
  B1G = "B1G", #"Gross Value added",
  D1 = "D1" #"Compensation of employees, total",
)

sna7a_transact <- c(
  EMP_DC = "ETOA", # "Total empoyment",
  SAL_DC = "EEMA" #"Employees",
)


# Measeures
sna_measures <-   c(
  CP_MNAC = "V",   # current prices
  CLV15_MNAC = "LR",  # Constant prices
  PYP_MNAC = "Y" # previous year prices
)

sna7a_measures <-   c(
  THS_PER = "PER",   # Persons
  THS_HW = "HRS",   # Hours
  THS_JOBS = "JOB"  # JObs
)



dat_oecd_sna1_0 <- get_dataset(dataset = "SNA_TABLE1")
                               ,
                               filter = list(sna_geo, sna1_transact, sna_measures))

filter_sna6 <- list("A", sna_geo, "", "", sna6a_transact, "", sna_activity, "", "", sna_measures, "", "") |>
  map(~paste(.x, collapse = "+")) |>
  paste(collapse = ".")

# dat_oecd_sna6a_0 <- get_dataset(dataset = "OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE6,1.0",
#                                 filter = "A.FIN+DNK+AUS...D1+B1G..C26+C16T18+_T+B+C...V+LR+Y..")

dat_oecd_sna6a_0 <- get_dataset(dataset = "OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE6,1.0",
                                filter = filter_sna6)

dat_oecd_sna7a_0 <- get_dataset(dataset = "SNA_TABLE7A",
                                filter = list(sna_geo, sna7a_transact, sna_activity, sna7a_measures))


dat_oecd_sna <- dat_oecd_sna1_0 %>%
  transmute(
    time = as.numeric(Time),
    geo = as_factor(countrycode(LOCATION, "iso3c", "eurostat", nomatch = NULL)),
    na_item = fct_recode(TRANSACTION, !!!sna1_transact),
    unit = fct_recode(PRICE_BASE, !!!sna_measures),
    currency = as_factor(CURRENCY),
    values = as.numeric(ObsValue)) %>%
  mutate(nace_r2 = "TOTAL")  %>%
  unite(vars, na_item, unit, sep = "__") %>%
  mutate(vars = as_factor(vars)) %>%
  spread(vars, values) %>%
  filter(time >= start_year) %>%
  droplevels() %>%
  complete(time, geo)


dat_oecd_sna6a <- dat_oecd_sna6a_0 %>%
  transmute(
    time = as.numeric(TIME_PERIOD),
    geo = as_factor(countrycode(REF_AREA, "iso3c", "eurostat", nomatch = NULL)),
    nace_r2 = fct_recode(ACTIVITY, !!!sna_activity),
    na_item = fct_recode(TRANSACTION, !!!sna6a_transact),
    unit = fct_recode(PRICE_BASE, !!!sna_measures),
    currency = as_factor(CURRENCY),
    values = as.numeric(ObsValue))  %>%
  unite(vars, na_item, unit, sep = "__") %>%
  mutate(vars = as_factor(vars)) %>%
  spread(vars, values)

dat_oecd_sna7a <- dat_oecd_sna7a_0 %>%
  transmute(
    time = as.numeric(Time),
    geo = as_factor(countrycode(LOCATION, "iso3c", "eurostat", nomatch = NULL)),
    nace_r2 = fct_recode(ACTIVITY, !!!sna_activity),
    na_item = fct_recode(TRANSACT, !!!sna7a_transact),
    unit = fct_recode(MEASURE, !!!sna7a_measures),
    values = as.numeric(ObsValue)) %>%
  unite(vars, na_item, unit, sep = "__") %>%
  mutate(vars = as_factor(vars)) %>%
  spread(vars, values) |>
  mutate(SAL_DC__THS_HW = 1000 * SAL_DC__THS_HW,
         EMP_DC__THS_HW = 1000 * EMP_DC__THS_HW)

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

data_oecd_total <- dat_oecd_sna %>%
  filter(time >= start_year) %>%
  droplevels() %>%
  complete(time, geo, nace_r2)

usethis::use_data(dat_oecd_sna_nace, dat_oecd_sna_nace_imput, data_oecd_total, overwrite = TRUE)

# dat_oecd_sna_nace_old <- dat_oecd_sna_nace
# dat_oecd_sna_nace_imput_old <- dat_oecd_sna_nace_imput
# data_oecd_total_old <- data_oecd_total
#
# usethis::use_data(dat_oecd_sna_nace_old, dat_oecd_sna_nace_imput_old, data_oecd_total_old, overwrite = TRUE)
