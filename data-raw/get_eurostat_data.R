
devtools::load_all()

library(dplyr)
library(tidyr)
library(forcats)
library(eurostat)

options(timeout = 300)

dat_nama_10_gdp_0 <- eurostat::get_eurostat("nama_10_gdp", time_format = "num", cache = FALSE)
dat_nama_10_a64_0 <- eurostat::get_eurostat("nama_10_a64", time_format = "num", cache = FALSE)
dat_nama_10_a64_e_0 <- eurostat::get_eurostat("nama_10_a64_e", time_format = "num", cache = FALSE)

dat_nama_10_gdp <- dat_nama_10_gdp_0 %>%
  filter(unit %in% c("CLV15_MEUR", "CLV15_MNAC", "CP_MNAC"),
         na_item %in% c("B1GQ", "P6", "P61", "P62", "B11")) %>%
  unite(vars, na_item, unit, sep = "__") %>%
  mutate(vars = as_factor(vars)) %>%
  spread(vars, values)

dat_nama_10_a64 <- dat_nama_10_a64_0 %>%
  filter(unit %in% c("CLV15_MEUR", "CLV15_MNAC", "CP_MNAC", "PYP_MNAC"),
         na_item %in% c("B1G", "D1")) %>%
  unite(vars, na_item, unit, sep = "__") %>%
  mutate(vars = as_factor(vars)) %>%
  spread(vars, values) %>%
  select(- D1__CLV15_MEUR, -D1__CLV15_MNAC, -D1__PYP_MNAC)

dat_nama_10_a64_e <- dat_nama_10_a64_e_0 %>%
  filter(unit %in% c("THS_HW", "THS_PER"),
         na_item %in% c("EMP_DC", "SAL_DC")) %>%
  unite(vars, na_item, unit, sep = "__") %>%
  mutate(vars = as_factor(vars)) %>%
  spread(vars, values)


# Main
dat_eurostat_nace <-
  dat_nama_10_a64 %>%
  left_join(dat_nama_10_a64_e, by = c("nace_r2", "geo", "time")) %>%
  mutate_at(c("geo", "nace_r2"), as_factor) %>%
  filter(nace_r2 %in% main_nace_sna,
         geo %in% countries,
         time >= start_year) %>%
  droplevels() %>%
  complete(geo, time, nace_r2)


dat_eurostat_nace_imput <-
  dat_eurostat_nace %>%
  # Imput hours from persons. For EE 1995-1999 and BE 2019
  mutate(emp_hw_per = EMP_DC__THS_HW / EMP_DC__THS_PER,
         sal_hw_per = SAL_DC__THS_HW / SAL_DC__THS_PER) %>%
  group_by(geo, nace_r2) %>%
  fill(emp_hw_per, sal_hw_per, .direction = "updown") %>%
  ungroup() %>%
  mutate(EMP_DC__THS_HW = coalesce(EMP_DC__THS_HW, emp_hw_per * EMP_DC__THS_PER),
         SAL_DC__THS_HW = coalesce(SAL_DC__THS_HW, sal_hw_per * SAL_DC__THS_PER)) %>%
  select(-emp_hw_per, -sal_hw_per)


  # Impute confidental 2015 for J in SE, not neede anymore
  # mutate_at(c("EMP_DC__THS_HW", "SAL_DC__THS_HW", "EMP_DC__THS_PER", "SAL_DC__THS_PER"),
  #           ~if_else(geo == "SE" & nace_r2 == "J" & time == 2015 & is.na(.),
  #                    mean(c(.[geo == "SE" & nace_r2 == "J" & time == 2014], .[geo == "SE" & nace_r2 == "J" & time == 2016])),
  #                    .))


# visdat::vis_dat(dat_eurostat_nace_imput)
#


# Detailed

dat_eurostat_nace_23 <-
  dat_nama_10_a64 %>%
  left_join(dat_nama_10_a64_e, by = c("nace_r2", "geo", "time")) %>%
  mutate_at(c("geo", "nace_r2"), as_factor) %>%
  filter(nace_r2 %in% names(nace_stan), geo %in% countries, time >= start_year) %>%
  droplevels() %>%
  complete(geo, time, nace_r2)

dat_eurostat_nace_23_imput <-
  dat_eurostat_nace_23 %>%
  mutate(nace_r2 = fct_recode(nace_r2, C20_C21 = "C20", C20_C21 = "C21")) %>%
  group_by(geo, time, nace_r2) %>%
  summarise_all(sum) %>%
  group_by(geo, time) %>%
  mutate(across(where(is.numeric), ~if_else((nace_r2 == "C20_C21" & is.na(.)),
                                 .[nace_r2 == "C"] - sum(.[nace_r2 %in% c("C10-C12", "C13-C15", "C16-C18","C19",
                                                          "C22_C23", "C24_C25", "C26", "C27", "C28", "C29_C30",
                                                         "C31-C33")]), .))) %>%
  ungroup()



# visdat::vis_dat(dat_eurostat_nace_23)

# filter(dat_eurostat_nace_23_imput) %>%
#   filter(time != "2018") %>%
#   filter(!(geo == "BG" & nace_r2 %in% c("C20", "C21"))) %>%
#   filter(!(geo == "EE" & nace_r2 %in% c("C20", "C21"))) %>%
#   filter((!geo == "SE" & nace_r2 %in% c("C20", "C21"))) %>%
#   filter((!geo == "NO" & nace_r2 %in% c("C20", "C21"))) %>%
#   # # filter(nace_r2 != "C26") %>%
#   filter(!(geo == "EA12" & time < 2000)) %>%
#   filter(!(geo == "UK" & time == 2017))  %>%
#   # filter(is.na(EMP_DC__THS_HW)) %>%
#   # distinct(time, nace_r2, geo)
#   visdat::vis_dat()

data_eurostat_total <-
  dat_nama_10_gdp %>%
  filter(time >= start_year) %>%
  complete(time, geo)

usethis::use_data(dat_eurostat_nace, dat_eurostat_nace_imput, overwrite = TRUE)
usethis::use_data(dat_eurostat_nace_23, data_eurostat_total, overwrite = TRUE)


## OECD-style digital classification eurostat data

dat_eurostat_digi <-
  dat_nama_10_a64 %>%
  left_join(dat_nama_10_a64_e, by = c("nace_r2", "geo", "time")) %>%
  mutate_at(c("geo", "nace_r2"), as_factor) %>%
  filter(nace_r2 %in% d_class$nace_r2, geo %in% countries, time >= start_year) %>%
  droplevels() %>%
  complete(geo, time, nace_r2)

usethis::use_data(dat_eurostat_digi, overwrite = TRUE)

