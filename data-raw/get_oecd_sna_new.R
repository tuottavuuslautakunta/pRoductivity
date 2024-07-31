# OECD annual national account data

library(OECD)
library(tidyverse)
library(countrycode)

devtools::load_all()


# sna1_str <- get_data_structure("OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE1_EXPENDITURE,1.0")
# str(sna1_str, max.level = 1)
# sna6a_str <- get_data_structure("OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE6,1.0")
# str(sna6a_str, max.level = 1)
# sna7a_str <- get_data_structure("SNA_TABLE7A")
#
# sna6a_str$ACTIVITY


# Countries
sna_geo <- countrycode(c("US", "JP"), "eurostat", "iso3c")

# Industries
sna_activity <- setNames(names(main_nace_sna_new), main_nace_sna_new)

# Transactions

sna1_transact <- c(
  B1GQ = "B1GQ", #"Gross domestic product",
  # D1 = "D1", #"Compensation of employees, total",
  P6 = "P6", #"Exports of goods and services",
  P61 = "P61", #"Exports of goods",
  P62 = "P62", #"Exports of services",
  B11 = "B11") #"External balance of goods and services"

sna1_2_transact <- c(
  D1 = "D1") #"Compensation of employees, total",


sna6a_transact <- c(
  B1G = "B1G", #"Gross Value added",
  D1 = "D1" #"Compensation of employees, total",
)

sna7a_transact <- c(
  EMP_DC = "EMP", # "Total empoyment",
  SAL_DC = "SAL" #"Employees",
)


# Measeures
sna_measures <-   c(
  CP_MNAC = "V",   # current prices
  CLV15_MNAC = "LR",  # Constant prices
  PYP_MNAC = "Y" # previous year prices
)

sna7a_measures <-   c(
  THS_PER = "PS",   # Persons
  THS_HW = "H",   # Hours
  THS_JOBS = "JB"  # JObs
)


# Annual GDP and components - expenditure approach
filter_sna1 <- list("A", sna_geo, "", "", sna1_transact, "", "", "", "XDC", sna_measures, "", "")
dat_oecd_sna1_0 <- get_dataset(dataset = "OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE1_EXPENDITURE,1.0",
                               filter = make_oecd_filter(filter_sna1))

# Annual GDP and components - income approach
filter_sna1_2 <- list("A", sna_geo, "", "", sna1_2_transact, "", "_T", "", "XDC", sna_measures["CP_MNAC"], "", "")
dat_oecd_sna1_2_0 <- get_dataset(dataset = "OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE1_INCOME,1.0",
                               filter = make_oecd_filter(filter_sna1_2))

# Annual value added and its components by economic activity
filter_sna6 <- list("A", sna_geo, "", "", sna6a_transact, "", sna_activity, "", "", sna_measures, "", "")
dat_oecd_sna6a_0 <- get_dataset(dataset = "OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE6,1.0",
                                filter = make_oecd_filter(filter_sna6))

# Annual employment by detailed economic activity, domestic concept
filter_sna7a <- list("A", sna_geo, "", "", sna7a_transact, "", sna_activity, "", sna7a_measures, "", "", "")
dat_oecd_sna7a_0 <- get_dataset(dataset = "OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE7,1.0",
                                filter = make_oecd_filter(filter_sna7a))


dat_oecd_sna1_1 <- dat_oecd_sna1_0 %>%
  transmute(
    time = as.numeric(TIME_PERIOD),
    geo = as_factor(countrycode(REF_AREA, "iso3c", "eurostat", nomatch = NULL)),
    na_item = fct_recode(TRANSACTION, !!!sna1_transact),
    unit = fct_recode(PRICE_BASE, !!!sna_measures),
    currency = as_factor(CURRENCY),
    values = as.numeric(ObsValue))  %>%
  mutate(nace_r2 = "TOTAL")  %>%
  unite(vars, na_item, unit, sep = "__") %>%
  mutate(vars = as_factor(vars)) %>%
  spread(vars, values) %>%
  filter(time >= start_year) %>%
  droplevels() %>%
  complete(time, geo)

dat_oecd_sna1_2 <- dat_oecd_sna1_2_0 %>%
  transmute(
    time = as.numeric(TIME_PERIOD),
    geo = as_factor(countrycode(REF_AREA, "iso3c", "eurostat", nomatch = NULL)),
    na_item = fct_recode(TRANSACTION, !!!sna1_2_transact),
    unit = fct_recode(PRICE_BASE, !!!sna_measures["CP_MNAC"]),
    currency = as_factor(CURRENCY),
    values = as.numeric(ObsValue))  %>%
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
    time = as.numeric(TIME_PERIOD),
    geo = as_factor(countrycode(REF_AREA, "iso3c", "eurostat", nomatch = NULL)),
    nace_r2 = fct_recode(ACTIVITY, !!!sna_activity),
    na_item = fct_recode(TRANSACTION, !!!sna7a_transact),
    unit = fct_recode(UNIT_MEASURE, !!!sna7a_measures),
    values = as.numeric(ObsValue))  %>%
  unite(vars, na_item, unit, sep = "__") %>%
  mutate(vars = as_factor(vars)) %>%
  spread(vars, values) |>
  mutate(SAL_DC__THS_HW = 1000 * SAL_DC__THS_HW,
         EMP_DC__THS_HW = 1000 * EMP_DC__THS_HW)

dat_oecd_sna_nace <-
  dat_oecd_sna6a %>%
  left_join(dat_oecd_sna7a, by = c("time", "geo", "nace_r2")) %>%
  filter(time >= start_year) %>%
  complete(time, geo, nace_r2)|>
  # Korjataan OECD:n USA:n kiinetähintaisten sarjojen puute BEA datalla, mutta käytetään vain hintaindeksi,
  left_join(select(dat_bea, geo, time, nace_r2, B1G__P), by = join_by(geo, time, nace_r2)) |>
  mutate(B1G__CLV15_MNAC = coalesce(B1G__CLV15_MNAC, B1G__CP_MNAC / B1G__P)) |>
  group_by(geo, nace_r2) |>
  mutate(B1G__PYP_MNAC = coalesce(B1G__PYP_MNAC, statfitools::pp(B1G__CP_MNAC, B1G__CLV15_MNAC, time))) |>
  select(-B1G__P)


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
  filter(!(geo == "JP" & nace_r2 %in% c("M", "N"))) %>%
  # drop attributes from columns
  mutate(across(where(is.numeric), ~{ attributes(.) <- NULL; . }))


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

data_oecd_total <-
  dat_oecd_sna1_1 %>%
  left_join(dat_oecd_sna1_2, by = c("time", "geo", "currency", "nace_r2")) %>%
  filter(time >= start_year) %>%
  droplevels() %>%
  complete(time, geo, nace_r2)

usethis::use_data(dat_oecd_sna_nace, dat_oecd_sna_nace_imput, data_oecd_total, overwrite = TRUE)

# dat_oecd_sna_nace_old <- dat_oecd_sna_nace
# dat_oecd_sna_nace_imput_old <- dat_oecd_sna_nace_imput
# data_oecd_total_old <- data_oecd_total
#
# usethis::use_data(dat_oecd_sna_nace_old, dat_oecd_sna_nace_imput_old, data_oecd_total_old, overwrite = TRUE)
