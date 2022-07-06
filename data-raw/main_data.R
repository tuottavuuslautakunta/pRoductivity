# Main data


library(tidyverse)

devtools::load_all()

start_year <- 1995
base_year <- 2007

## Countries and classifications

countries0 <- c("AT", "BG", "CZ", "DE", "DK", "EE", "EL", "ES", "FI", "BE", "HU", "LT", "LV",
                "FR", "IT", "NL", "NO", "PT", "SE", "UK", "CY", "IE", "PL", "SI", "SK",
                "US", "JP", "EA12")

countries <- setNames(countries0, countrycode::countrycode(countries0, "eurostat", "cldr.name.fi",
                                                           custom_match = c(EA12 = "Euroalue-12")))

countries_synth <- c("FI", "SE", "NO", "DK", "BE", "NL", "AT", "PT", "DE", "IT", "FR", "ES")

# weight_geos <- setdiff(countries, c("US", "JP", "EA12", "IE", "LV", "UK"))
weight_geos <- setdiff(countries, c("US", "JP", "EA12", "LV", "IE"))


main_nace_sna <- c(VTOT = "TOTAL", VC = "C", V26 = "C26",  VF = "F", VG = "G", VH = "H",
                   VI = "I", VJ = "J", VM = "M", VN = "N")

main_nace10_sna <- c(TOTAL = "TOTAL", C = "C", F = "F", G = "G-I", H = "G-I",
                   I = "G-I", J = "J", M = "M_N", N = "M_N")

nace_stan <- c(
  TOTAL = "D01T99",
  C = "D10T33",
  "C10-C12" = "D10T12",
  "C13-C15" = "D13T15",
  "C16-C18" = "D16T18",
  "C19-C21" =  "D19T23", # Have to be aggregated in eurostat data
  C19 = "D19T23",
  C20 = "D19T23",
  C21 = "D19T23",
  C22_C23 = "D19T23",
  C24_C25 = "D24T25",
  C26 = "D26",
  C27 = "D27",
  C28 = "D28",
  C29_C30 = "D29T30",
  "C31-C33" = "D31T33",
  F = "D41T43",
  G = "D45T47",
  H = "D49T53",
  I = "D55T56",
  "J58-J60" = "D58T60",
  J61 = "D61",
  J62_J63 = "D62T63",
  M_N = "D69T82"
)

# OECD non-digital (nd_manu, nd_serv) / digital (d_manu, d_serv) classification
d_class <- read.csv2("data-raw/d_class.csv")

usethis::use_data(start_year, base_year, countries, countries_synth, main_nace_sna, main_nace10_sna, nace_stan, d_class, overwrite = TRUE)

geo_digi_oecd = c("SE", "NO", "FR", "BE", "PT")
geo_digi_1 = c("FR", "BE","AT", "IT", "NL")
geo_digi_2 = c("SE", "NO", "FR", "BE", "AT", "IT", "NL", "DE", "ES", "PT")
geo_digi_3 = c("SE", "NO", "FR", "BE", "AT", "IT", "NL", "DE", "ES", "PT", "DK")


usethis::use_data(geo_digi_oecd, geo_digi_1, geo_digi_2, geo_digi_3, overwrite = TRUE)


## Data used
# update:
source("data-raw/get_eurostat_data_10.R")
source("data-raw/get_eurostat_data.R")
source("data-raw/get_oecd_sna.R")

data("dat_eurostat_nace_imput", "dat_oecd_sna_nace_imput")



# Main from a64

data_main <-
  dat_eurostat_nace_imput %>%
  select(- B1G__CLV15_MEUR) %>%
  bind_rows(select(dat_oecd_sna_nace_imput, all_of(names(.)))) %>%
  # filter(time >= start_time_main,
  #        geo %in% countries) %>%
  mutate(geo_name = fct_recode(geo, !!!countries),
         geo = as_factor(geo))


# visdat::vis_dat(data_main)


data_main_groups <- data_main %>%
  gather(vars, values, -time, - geo, -geo_name, - nace_r2) %>%
  group_by(geo, geo_name, time, vars) %>%
  summarise(private = sum(values[!(nace_r2 %in% c("TOTAL", "C26"))]),
            private_ex26 = private - values[nace_r2 == "C26"],
            manu = sum(values[nace_r2 == "C"]),
            manu_ex26 = manu - values[nace_r2 == "C26"],
            service = sum(values[nace_r2 %in% c(c("G", "H", "I", "J", "M", "N"))])) %>%
  ungroup() %>%
  gather(nace0, values, private, private_ex26, manu, manu_ex26, service) %>%
  mutate(nace0 = as_factor(nace0)) %>%
  spread(vars, values) %>%
  group_by(geo, geo_name, nace0) %>%
  # volyymia ei voi laskea yhteen, täytyy laske cp ja pp sarjoista
  mutate(b1g__clv10_mnac = statfitools::fp(cp = B1G__CP_MNAC, pp = B1G__PYP_MNAC, time = time, year = 2010),
         lp_10 = b1g__clv10_mnac / EMP_DC__THS_HW,
         lp_ind = 100 * lp_10/lp_10[time == base_year]) %>%
  ungroup() %>%
  complete(nesting(geo, geo_name), nace0, time) %>%
  group_by(geo, nace0) %>%
  mutate(lp_ind =  rebase(lp_ind, time, base_year),
         va_ind =  rebase(b1g__clv10_mnac, time, base_year),
         h_ind = rebase(EMP_DC__THS_HW, time, base_year),
         emp_ind = rebase(EMP_DC__THS_PER, time, base_year)) %>%
  ungroup() %>%
  mutate(geo_name = fct_relevel(geo_name, rev(names(countries))))

usethis::use_data(data_main, overwrite = TRUE)
usethis::use_data(data_main_groups, overwrite = TRUE)


data_main_g_weighted <-
  data_main_groups %>%
  select(geo, geo_name, time, nace0, lp_ind, va_ind, h_ind) %>% # filter(nace0 == "private") %>% select(geo, time, lp_ind)  %>% spread(geo, lp_ind) %>% View()
  group_by(nace0, time) %>%
  mutate_at(vars(lp_ind, va_ind, h_ind), .funs = list(rel_ea = ~(100 * .x / .x[geo == "EA12"]))) %>%
  # filter(geo %in% weight_geos) %>%
  group_by(nace0, time) %>%
  mutate(across(c(lp_ind, va_ind, h_ind),
                ~ficomp::weight_index2(.x, geo, time, geos = weight_geos, weight_df = ficomp::weights_ecfin37),
                .names = paste0("{col}_rel_weight"))) %>%
  ungroup()

usethis::use_data(data_main_g_weighted, overwrite = TRUE)



data_total <-
  data_eurostat_total %>%
  select(-contains("MEUR")) %>%
  bind_rows(select(data_oecd_total, all_of(names(.)))) %>%
  filter(geo %in% countries) %>%
  mutate(geo_name = fct_recode(geo, !!!countries),
         geo_name = fct_relevel(geo_name, rev(names(countries))))





usethis::use_data(data_main, data_total, overwrite = TRUE)



# Main form a10

dat_oecd_sna_nace10_imput <-
  dat_oecd_sna_nace_imput %>%
  select(-currency) %>%
  filter(nace_r2 != "C26") %>%
  mutate(nace_r2 = fct_recode(nace_r2, !!!set_names(names(main_nace10_sna), main_nace10_sna))) %>%
  group_by(geo, time, nace_r2) %>%
  summarise_all(sum) %>%
  group_by(geo, nace_r2) %>%
  mutate(B1G__CLV15_MNAC = statfitools::fp(cp = B1G__CP_MNAC, pp = B1G__PYP_MNAC, time = time, year = 2015)) %>%
  ungroup()


data_main10 <-
  dat_eurostat_nace10_imput %>%
  select(- B1G__CLV15_MEUR) %>%
  bind_rows(select(dat_oecd_sna_nace10_imput, all_of(names(.)))) %>%
  # filter(time >= start_time_main,
  #        geo %in% countries) %>%
  mutate(geo_name = fct_recode(geo, !!!countries),
         geo = as_factor(geo))


data_main10_groups <- data_main10 %>%
  gather(vars, values, -time, - geo, -geo_name, - nace_r2) %>%
  group_by(geo, geo_name, time, vars) %>%
  summarise(private = sum(values[!(nace_r2 %in% c("TOTAL"))]),
            manu = sum(values[nace_r2 == "C"]),
            service = sum(values[nace_r2 %in% c(c("G-I", "J", "M_N"))]),
            .groups = "drop_last") %>%
  ungroup() %>%
  gather(nace0, values, private, manu, service) %>%
  mutate(nace0 = as_factor(nace0)) %>%
  spread(vars, values) %>%
  group_by(geo, geo_name, nace0) %>%
  # volyymia ei voi laskea yhteen, täytyy laske cp ja pp sarjoista
  mutate(b1g__clv10_mnac = statfitools::fp(cp = B1G__CP_MNAC, pp = B1G__PYP_MNAC, time = time, year = 2010),
         lp_10 = b1g__clv10_mnac / EMP_DC__THS_HW,
         lp_ind = 100 * lp_10/lp_10[time == base_year]) %>%
  ungroup() %>%
  complete(nesting(geo, geo_name), nace0, time) %>%
  group_by(geo, nace0) %>%
  mutate(lp_ind =  rebase(lp_ind, time, base_year),
         va_ind =  rebase(b1g__clv10_mnac, time, base_year),
         h_ind = rebase(EMP_DC__THS_HW, time, base_year),
         emp_ind = rebase(EMP_DC__THS_PER, time, base_year)) %>%
  ungroup() %>%
  mutate(geo_name = fct_relevel(geo_name, rev(names(countries))))

data_main10_g_weighted <-
  data_main10_groups %>%
  select(geo, geo_name, time, nace0, lp_ind, va_ind, h_ind) %>% #  filter(nace0 == "manu") %>% select(geo, time, lp_ind)  %>% spread(geo, lp_ind) %>% View()
  group_by(nace0, time) %>%
  mutate_at(vars(lp_ind, va_ind, h_ind), .funs = list(rel_ea = ~(100 * .x / .x[geo == "EA12"]))) %>%
  group_by(nace0, time) %>%
  mutate(across(c(lp_ind, va_ind, h_ind),
                ~ficomp::weight_index2(.x, geo, time, geos = weight_geos, weight_df = ficomp::weights_ecfin37),
                .names = paste0("{col}_rel_weight"))) %>%
  ungroup()

usethis::use_data(data_main10, overwrite = TRUE)
usethis::use_data(data_main10_groups, overwrite = TRUE)
usethis::use_data(data_main10_g_weighted, overwrite = TRUE)


## OECD-style digital classification data

data_digi <-
  dat_eurostat_digi |>
  left_join(d_class, by = "nace_r2") |>
  # filter(time<2020) |>
  group_by(geo, nace_r2) %>%
  mutate(lp_ind =  rebase(B1G__CLV15_MNAC / EMP_DC__THS_PER, time, base_year),
         va_ind =  rebase(B1G__CLV15_MNAC, time, base_year),
         h_ind = rebase(EMP_DC__THS_HW, time, base_year),
         emp_ind = rebase(EMP_DC__THS_PER, time, base_year)) %>%
  ungroup() %>%
  group_by(geo, time) |>
  mutate(emp_weight = EMP_DC__THS_PER / sum(EMP_DC__THS_PER, na.rm = TRUE)) |>
  group_by(nace_r2) |>
  mutate(emp_weight_fi = emp_weight[geo == "FI" & time == 2007]) |>
  group_by(d_class, geo, time) |>
  summarise(lp_ind = ficomp::weighted_gmean(lp_ind, w = emp_weight_fi, na.rm = TRUE)) |>
  ungroup() |>
  mutate(w_oecd = geo %in% geo_digi_oecd,
         w_1 = geo %in% geo_digi_1,
         w_2 = geo %in% geo_digi_2,
         w_3 = geo %in% geo_digi_3) |>
  group_by(d_class, time) |>
  summarise(FI = lp_ind[geo == "FI"],
            Bench_OECD = weighted.mean(lp_ind, w_oecd),
            Bench_1 = weighted.mean(lp_ind, w_1),
            Bench_2 = weighted.mean(lp_ind, w_2),
            Bench_3 = weighted.mean(lp_ind, w_3)) |>
  ungroup() |>
  gather(geo, lp_ind, -d_class, -time)

usethis::use_data(data_digi, overwrite = TRUE)


## Extended level data

# read extension data for history
lp_private_history <-
  readxl::read_xlsx(here::here("data-raw/LP history.xlsx"), skip = 4) |>
  mutate(nace0 = "private") |>
  gather(geo, values, -time, -nace0)

# join to data and level


data_main10_groups_level <-
  data_main10_groups |>
  filter(!is.na(lp_ind)) |>
  mutate(time_org = time) |>
  # join history and 2005 levels
  full_join(lp_private_history, by = c("geo", "nace0", "time"))|>
  left_join(rename(data_ggdc_level05, levels05 = values), by = c("geo", "nace0")) |>
  left_join(select(exh_eur_a, geo, time, exh_eur = values), by = c("geo", "time")) |>
  arrange(time, geo) |>
  # extrapolate lp_ind based on lp_hist
  group_by(geo, nace0) |>
  mutate(lp_ind = rebase(lp_ind, time, first(na.omit(time_org))),
         lp_hist = rebase(values, time, first(na.omit(time_org))),
         lp_ind = coalesce(lp_ind, lp_hist)) |>
  # USA täytyy fiksata myöhemmin, nyt historiasarjassa uudempi privatelle, joten otetaan siitä kokonaan
  mutate(lp_ind = if_else(geo == "US" & nace0 == "private", lp_hist, lp_ind)) |>
  # vuoden 20005 indeksiin ja markkinahinnoin
  mutate(lp_ind = rebase(lp_ind, time, 2005),
         lp_fp05 = (B1G__CP_MNAC / EMP_DC__THS_HW)[time == 2005] * lp_ind / 100,
         lp_fp05_eur = lp_fp05 / exh_eur[time == 2005]) |>
  ungroup() |>
  # level lp
  mutate(lp_level05 = levels05 * lp_ind) |>
  mutate(geo_name = fct_recode(geo, !!!countries)) |>
  select(geo, geo_name, time, lp_ind, lp_level05, lp_fp05, lp_fp05_eur, levels05, nace0) |>
  droplevels()

usethis::use_data(data_main10_groups_level, overwrite = TRUE)
