
load_all()
library(eurostat)

dat_nama_10_a64_0 <- eurostat::get_eurostat("nama_10_a64", time_format = "num", cache = FALSE)
dat_nama_10_a64_e_0 <- eurostat::get_eurostat("nama_10_a64_e", time_format = "num", cache = FALSE)


dat_nama_10_a64 <- dat_nama_10_a64_0 %>%
  filter(unit %in% c("CLV15_MEUR", "CLV15_MNAC", "CP_MNAC", "PYP_MNAC"),
         na_item == "B1G") %>%
  unite(vars, na_item, unit, sep = "__") %>%
  mutate(vars = as_factor(vars)) %>%
  spread(vars, values)

dat_nama_10_a64_e <- dat_nama_10_a64_e_0 %>%
  filter(unit %in% c("THS_HW", "THS_PER"),
         na_item %in% c("EMP_DC", "SAL_DC")) %>%
  unite(vars, na_item, unit, sep = "__") %>%
  mutate(vars = as_factor(vars)) %>%
  spread(vars, values)

dat_eurostat_nace <-
  dat_nama_10_a64 %>%
  left_join(dat_nama_10_a64_e, by = c("nace_r2", "geo", "time")) %>%
  mutate_at(c("geo", "nace_r2"), as_factor) %>%
  filter(nace_r2 %in% main_nace_sna, geo %in% countries, time >= start_year) %>%
  droplevels() %>%
  complete(geo, time, nace_r2)

dat_eurostat_nace_imput <-
  dat_eurostat_nace %>%
  mutate_at(c("EMP_DC__THS_HW", "SAL_DC__THS_HW", "EMP_DC__THS_PER", "SAL_DC__THS_PER"),
            ~if_else(geo == "SE" & nace_r2 == "J" & time == 2015 & is.na(.),
                     mean(c(.[geo == "SE" & nace_r2 == "J" & time == 2014], .[geo == "SE" & nace_r2 == "J" & time == 2016])),
                     .))


# visdat::vis_dat(dat_eurostat_nace_imput)
#
# nrow(dat_eurostat_nace)
#
filter(dat_eurostat_nace_imput) %>%
  filter(time != "2018") %>%
  # filter(nace_r2 != "C26") %>%
  filter(!(geo == "EA12" & time < 2000)) %>%
  filter(!(geo == "UK" & time == 2017))  %>%
  visdat::vis_dat()
# #
# #
# dat_eurostat_nace_imput %>%
#   filter(is.na(EMP_DC__THS_HW)) %>%
#   distinct(time, nace_r2, geo)


use_data(dat_eurostat_nace, dat_eurostat_nace_imput, overwrite = TRUE)



