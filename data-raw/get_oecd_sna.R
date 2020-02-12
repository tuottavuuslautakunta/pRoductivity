# OECD annual national account data

library(OECD)
library(tidyverse)
library(countrycode)

dataset_list <- get_datasets()

start_year <- 1997

dataset_list %>%
  filter(id %in% c("SNA_TABLE6A", "SNA_TABLE7A")) %>%
  knitr::kable()

sna6a_str <- get_data_structure("SNA_TABLE6A")
sna7a_str <- get_data_structure("SNA_TABLE7A")

sna7a_str$POWERCODE


# Needed countries that are in SNA
sna_geo <- countrycode(c("US", "JP", "CH"), "eurostat", "iso3c")

main_nace_sna <- c(VC = "C", V26 = "C26",  VF = "F", VG = "G", VH = "H",
              VI = "I", VJ = "J", VM = "M", VN = "N")

sna6a_transact <- c(
  B1G = "B1GA", #"Gross Value added",
  D1 = "D1A" #"Compensation of employees, total",
)

sna7a_transact <- c(
  EMP_DC = "ETOA", # "Total empoyment",
  SAL_DC = "EEMA" #"Employees",
)


sna_activity <- setNames(names(main_nace_sna), main_nace_sna)

sna_measures <-   c(
  CP_NAC = "C",   # current prices
  CLV_NAC = "V",  # Constant prices
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

# kk <- get_dataset(dataset = "SNA_TABLE7A",
#                filter = list("FIN", "ETOA", "VC", "PER"))


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
  complete(time, geo, nace_r2) %>%
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
  select(-emp_approx, -sal_approx)

visdat::vis_dat(dat_oecd_sna_nace)

filter(dat_oecd_sna_nace, geo == "JP") %>%
  filter(time != "2018") %>%
  filter(nace_r2 != "C26") %>%
  filter(nace_r2 != "N") %>%
  filter(!(nace_r2 == "M" & time < 2004)) %>%
  visdat::vis_dat()

filter(dat_oecd_sna_nace, geo == "US") %>%
  filter(time != "2018") %>%
  # filter(nace_r2 != "C26") %>%
  # filter(nace_r2 != "N") %>%
  # filter(!(nace_r2 == "M" & time < 2004)) %>%
  visdat::vis_dat()

sum(is.na(dat_oecd_sna_nace))

dat_oecd_sna_nace %>%
  filter(is.na(EMP_DC__THS_PER), geo != "USA") %>%
  distinct(geo, time, nace_r2) %>% View()

dat_oecd_sna_nace %>%
  filter(!(geo == "JP" & nace_r2 == "N")) %>%
  filter(time < 2018) %>%  #visdat::vis_dat()
  filter(is.na(EMP_DC__THS_PER)) %>%
  distinct(geo, time, nace_r2) %>%
  knitr::kable()

extrapol <- function(x, y, data){
  mod <- lm(x ~ y)
  # ret <- predict(mod, newdata = data)
  # ret
  mod
}

kk <- dat_oecd_sna_nace %>%
  select(geo, time ,nace_r2, EMP_DC__THS_PER, EMP_DC__THS_JOBS) %>%
  filter(time > 1997, geo == "US") %>%
  nest(data = c(time, EMP_DC__THS_PER, EMP_DC__THS_JOBS)) %>%
  mutate(emp_app = map(data, extrapol(EMP_DC__THS_PER, EMP_DC__THS_JOBS, data = .x))) %>%
  unnest(cols = c(data, emp_app))

kk %>% knitr::kable()

kk <- dat_oecd_sna_nace %>%
  select(geo, time ,nace_r2, EMP_DC__THS_PER, EMP_DC__THS_JOBS) %>%
  filter(time > 1997, geo == "US") %>%
  nest(data = c(-geo, nace_r2)) %>%
  mutate(emp_app = map(data, ~predict(lm(EMP_DC__THS_PER ~ EMP_DC__THS_JOBS, data = .), .))) %>%
  unnest(cols = c(data, emp_app))

kk <- dat_oecd_sna_nace %>%
  select(geo, time ,nace_r2, EMP_DC__THS_PER, EMP_DC__THS_JOBS) %>%
  nest(data = c(time, EMP_DC__THS_PER, EMP_DC__THS_JOBS)) %>%
  mutate(emp_mod = map(data, possibly(~lm(EMP_DC__THS_PER ~ EMP_DC__THS_JOBS, data = .x), otherwise = NA_real_)),
         emp_approx = map2(emp_mod, data, possibly(~predict(.x, .y), otherwise = NA_real_))) %>%
  select(-emp_mod) %>%
  unnest(cols = c(data, emp_approx)) %>%
  mutate(EMP_DC__THS_PER = coalesce(EMP_DC__THS_PER, emp_approx)) %>%
  select(-emp_approx)


kk %>%
  gather(vars, values, EMP_DC__THS_PER:emp_app) %>%
  ggplot(aes(time, values, colour = vars)) +
  facet_wrap(~ nace_r2) +
  geom_line()

  mutate(emp_app = Hmisc::approxExtrap(EMP_DC__THS_PER, EMP_DC__THS_JOBS, xout = EMP_DC__THS_JOBS)) %>% knitr::kable()



  filter(geo == "US", nace_r2 == "C") %>% knitr::kable()
  filter(time < 2018) %>%   #visdat::vis_dat()
  filter(is.na(EMP_DC__THS_PER)) %>%
