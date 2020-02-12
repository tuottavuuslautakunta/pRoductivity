# Main data


library(tidyverse)


devtools::load_all()

data("dat_stan_main", "dat_nama_main")

countries0 <- c("AT", "BG", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI",
               "FR", "IT", "LT", "NL", "NO", "PT", "RO", "SE", "SI", "SK", "UK",
               "US")

countries <- setNames(countries0, countrycode::countrycode(countries0, "eurostat", "cldr.name.fi"))

start_time_main <- 1995
base_year <- 2008

data_main <-
  dat_nama_main %>%
  select(- B1G__CLV10_MEUR) %>%
  bind_rows(filter(dat_stan_main, time >= min(dat_nama_main$time))) %>%
  filter(time >= start_time_main,
         geo %in% countries) %>%
  complete(geo, time, nace_r2) %>%
  mutate(geo_name = fct_recode(geo, !!!countries))


visdat::vis_dat(data_main)

data_main %>%
  filter(is.na(EMP_DC__THS_PER)) %>%
  distinct(geo, time)


data_main_groups <- data_main %>%
  gather(vars, values, -time, - geo, -geo_name, - nace_r2) %>%
  group_by(geo, geo_name, time, vars) %>%
  summarise(private = sum(values),
            manu = sum(values[nace_r2 == "C"]),
            service = sum(values[nace_r2 %in% c(c("G", "H", "I", "J", "M", "N"))])) %>%
  ungroup() %>%
  gather(nace0, values, private, manu, service) %>%
  spread(vars, values) %>%
  group_by(geo, geo_name, nace0) %>%
  # volyymia ei voi laskea yhteen, täytyy laske cp ja pp sarjoista
  mutate(b1g__clv10_mnac = statfitools::fp(cp = B1G__CP_MNAC, pp = B1G__PYP_MNAC, time = time, year = 2010),
         lp_10 = b1g__clv10_mnac / EMP_DC__THS_HW,
         lp_ind = 100 * lp_10/lp_10[time == base_year]) %>%
  ungroup() %>%
  mutate(geo_name = fct_relevel(geo_name, rev(names(countries))))


usethis::use_data(data_main, overwrite = TRUE)
usethis::use_data(data_main_groups, overwrite = TRUE)
