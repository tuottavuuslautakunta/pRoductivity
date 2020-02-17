# Main data


library(tidyverse)

devtools::load_all()

start_year <- 1995
base_year <- 2008

countries0 <- c("AT", "BG", "CZ", "DE", "DK", "EE", "EL", "ES", "FI",
                "FR", "IT", "NL", "NO", "PT", "SE", "UK",
                "US", "JP", "EA12")

countries <- setNames(countries0, countrycode::countrycode(countries0, "eurostat", "cldr.name.fi",
                                                           custom_match = c(EA12 = "Euroalue-12")))

main_nace_sna <- c(VTOT = "TOTAL", VC = "C", V26 = "C26",  VF = "F", VG = "G", VH = "H",
                   VI = "I", VJ = "J", VM = "M", VN = "N")

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

usethis::use_data(start_year, base_year, countries, main_nace_sna, nace_stan, overwrite = TRUE)


data("dat_eurostat_nace_imput", "dat_oecd_sna_nace_imput")



data_main <-
  dat_eurostat_nace_imput %>%
  select(- B1G__CLV15_MEUR) %>%
  bind_rows(select(dat_oecd_sna_nace_imput, all_of(names(.)))) %>%
  # filter(time >= start_time_main,
  #        geo %in% countries) %>%
  mutate(geo_name = fct_recode(geo, !!!countries))


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
