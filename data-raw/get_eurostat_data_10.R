
devtools::load_all()

library(dplyr)
library(tidyr)
library(forcats)
library(eurostat)

dat_nama_10_gdp_0 <- eurostat::get_eurostat("nama_10_gdp", time_format = "num", cache = FALSE)%>%
  rename(time = TIME_PERIOD) %>% select(-freq)
dat_nama_10_a10_0 <- eurostat::get_eurostat("nama_10_a10", time_format = "num", cache = FALSE)%>%
  rename(time = TIME_PERIOD) %>% select(-freq)
dat_nama_10_a10_e_0 <- eurostat::get_eurostat("nama_10_a10_e", time_format = "num", cache = FALSE)%>%
  rename(time = TIME_PERIOD) %>% select(-freq)

dat_nama_10_gdp <- dat_nama_10_gdp_0 %>%
  filter(unit %in% c("CLV15_MEUR", "CLV15_MNAC", "CP_MNAC"),
         na_item %in% c("B1GQ", "P6", "P61", "P62", "B11")) %>%
  unite(vars, na_item, unit, sep = "__") %>%
  mutate(vars = as_factor(vars)) %>%
  spread(vars, values)

dat_nama_10_a10 <- dat_nama_10_a10_0 %>%
  filter(unit %in% c("CLV15_MEUR", "CLV15_MNAC", "CP_MNAC", "PYP_MNAC"),
         na_item %in% c("B1G", "D1")) %>%
  unite(vars, na_item, unit, sep = "__") %>%
  mutate(vars = as_factor(vars)) %>%
  spread(vars, values)

dat_nama_10_a10_e <- dat_nama_10_a10_e_0 %>%
  filter(unit %in% c("THS_HW", "THS_PER"),
         na_item %in% c("EMP_DC", "SAL_DC")) %>%
  unite(vars, na_item, unit, sep = "__") %>%
  mutate(vars = as_factor(vars)) %>%
  spread(vars, values)


# Main
dat_eurostat_nace10 <-
  dat_nama_10_a10 %>%
  left_join(dat_nama_10_a10_e, by = c("nace_r2", "geo", "time")) %>%
  mutate_at(c("geo", "nace_r2"), as_factor) %>%
  filter(nace_r2 %in% main_nace10_sna,
         geo %in% countries,
         time >= start_year) %>%
  droplevels() %>%
  complete(geo, time, nace_r2)


dat_eurostat_nace10_imput <-
  dat_eurostat_nace10 %>%
  # Imput hours from persons. For EE 1995-1999 and BE 2019
  mutate(emp_hw_per = EMP_DC__THS_HW / EMP_DC__THS_PER,
         sal_hw_per = SAL_DC__THS_HW / SAL_DC__THS_PER) %>%
  group_by(geo, nace_r2) %>%
  fill(emp_hw_per, sal_hw_per, .direction = "updown") %>%
  ungroup() %>%
  mutate(EMP_DC__THS_HW = coalesce(EMP_DC__THS_HW, emp_hw_per * EMP_DC__THS_PER),
         SAL_DC__THS_HW = coalesce(SAL_DC__THS_HW, sal_hw_per * SAL_DC__THS_PER)) %>%
  select(-emp_hw_per, -sal_hw_per)



# visdat::vis_dat(filter(dat_eurostat_nace10, geo == "BE"))
#


# data_eurostat_total <-
#   dat_nama_10_gdp %>%
#   filter(time >= start_year) %>%
#   complete(time, geo)

usethis::use_data(dat_eurostat_nace10, dat_eurostat_nace10_imput, overwrite = TRUE)
# usethis::use_data(data_eurostat_total, overwrite = TRUE)



