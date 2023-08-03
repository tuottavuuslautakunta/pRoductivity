
library(readr)
library(tidyverse)

devtools::load_all()


# Labels

# labels_klems_0 <- scan("http://www.euklems.net/TCB/2018/ALL_output_Readme_17ii.txt",
#                        what =  character(),
#                        sep = "\n")




# data

# Data is from https://euklems-intanprod-llee.luiss.it/download/
# links change in releases
# Variable list: https://www.dropbox.com/s/ziu7wpl8pgqhq51/Variable%20List.xlsx?dl=1

tmp <- tempfile(fileext = ".rds")

## 	Growth Accounts Basic


download.file("https://www.dropbox.com/s/ri1ezv4tq4o9mmm/growth%20accounts.rds?dl=1", destfile = tmp, mode = "wb")

dat_klems_luiss_0 <- readRDS(tmp)



dat_klems_luiss <-
  dat_klems_luiss_0 |>
  mutate(year = as.numeric(year)) |>
  mutate(across(where(is.character), as_factor)) |>
  rename(geo = geo_code, time = year, vars = var, values = value, nace_r2 = nace_r2_code)

data_luiss_groups <- dat_klems_luiss |>
  mutate(nace_r2 = forcats::fct_recode(nace_r2, TOTAL = "TOT")) |>
  filter(nace_r2 %in% main_nace_sna) |>
  # complete(geo, time, vars, nace_r2_code) |>
  group_by(geo, geo_name, time, vars) %>%
  summarise(private = sum(values[!(nace_r2 %in% c("TOTAL", "C26"))]),
            private_ex26 = private - values[nace_r2 == "C26"],
            manu = sum(values[nace_r2 == "C"]),
            manu_ex26 = manu - values[nace_r2 == "C26"],
            service = sum(values[nace_r2 %in% c(c("G", "H", "I", "J", "M", "N"))])) %>%
  ungroup() %>%
  gather(nace0, values, private, private_ex26, manu, manu_ex26, service) %>%
  mutate(nace0 = as_factor(nace0))

## 	Growth Accounts Extended


download.file("https://www.dropbox.com/s/d1cegmlamt0biy7/growth%20accounts.rds?dl=1", destfile = tmp, mode = "wb")

dat_klems_luiss_ext0 <- readRDS(tmp)



dat_klems_luiss_ext <-
  dat_klems_luiss_ext0 |>
  mutate(year = as.numeric(year)) |>
  mutate(across(where(is.character), as_factor)) |>
  rename(geo = geo_code, time = year, vars = var, values = value, nace_r2 = nace_r2_code)

# data_luiss_groups_ext <- dat_klems_luiss_ext |>
#   mutate(nace_r2 = forcats::fct_recode(nace_r2, TOTAL = "TOT")) |>
#   filter(nace_r2 %in% main_nace_sna) |>
#   # complete(geo, time, vars, nace_r2_code) |>
#   group_by(geo, geo_name, time, vars) %>%
#   summarise(private = sum(values[!(nace_r2 %in% c("TOTAL", "C26"))]),
#             private_ex26 = private - values[nace_r2 == "C26"],
#             manu = sum(values[nace_r2 == "C"]),
#             manu_ex26 = manu - values[nace_r2 == "C26"],
#             service = sum(values[nace_r2 %in% c(c("G", "H", "I", "J", "M", "N"))])) %>%
#   ungroup() %>%
#   gather(nace0, values, private, private_ex26, manu, manu_ex26, service) %>%
#   mutate(nace0 = as_factor(nace0))


data_luiss_groups_tfp <-
  bind_rows(
    filter_recode(dat_klems_luiss,
                  vars = c(VA_CP = "VA_CP")),
    filter_recode(dat_klems_luiss_ext,
                  vars = c(VATFP_I = "LP1TFP_I")))|>
  mutate(nace_r2 = forcats::fct_recode(nace_r2, TOTAL = "TOT")) |>
  filter(nace_r2 %in% main_nace_sna) |>
  # filter(nace_r2 %in% c("B", "C", "D-E", "F", "G", "H", "I", "J", "K", "M", "N", "R", "S", "C26", "TOTAL")) |>
  # complete(geo, time, vars, nace_r2_code) |>
  spread(vars, values) |>
  group_by(geo, geo_name, time) |>
  summarise(vacp_private = sum(VA_CP[!(nace_r2 %in% c("TOTAL", "C26"))]),
            vacp_private_ex26 = vacp_private - VA_CP[nace_r2 == "C26"],
            vacp_manu = sum(VA_CP[nace_r2 == "C"]),
            vacp_manu_ex26 = vacp_manu - VA_CP[nace_r2 == "C26"],
            vacp_service = sum(VA_CP[nace_r2 %in% c(c("G", "H", "I", "J", "M", "N"))]),
            # Geometrinen keskiarvo painoina arvonlisä
            tfp_private = ficomp::weighted_gmean(VATFP_I[!(nace_r2 %in% c("TOTAL", "C26"))], VA_CP[!(nace_r2 %in% c("TOTAL", "C26"))]),
            # Geometrinen keskiarvo painoina arvonlisä, laskettu koko private ja c26 perustella. Private paino 1
            tfp_private_ex26 = (tfp_private ^ 1 / VATFP_I[nace_r2 == "C26"] ^ (VA_CP[nace_r2 == "C26"]/vacp_private)) ^ (1 / (1 - VA_CP[nace_r2 == "C26"] / vacp_private)),
            tfp_manu = VATFP_I[nace_r2 == "C"],
            tfp_manu_ex26 = (tfp_manu ^ 1 / VATFP_I[nace_r2 == "C26"] ^ (VA_CP[nace_r2 == "C26"]/vacp_manu)) ^ (1 / (1 - VA_CP[nace_r2 == "C26"] / vacp_manu)),
            tfp_service = ficomp::weighted_gmean(VATFP_I[nace_r2 %in% c(c("G", "H", "I", "J", "M", "N"))], VATFP_I[nace_r2 %in% c(c("G", "H", "I", "J", "M", "N"))])) %>%
  ungroup() %>%
  gather(vars, values, -geo, - geo_name, -time) %>%
  separate(vars, c("vars", "nace0"), sep = "_", extra = "merge") |>
  mutate(nace0 = as_factor(nace0),
         vars = as_factor(vars))

## Intangibles

download.file("https://www.dropbox.com/s/um2yu23712lx1hi/intangibles%20analytical.rds?dl=1", destfile = tmp, mode = "wb")

dat_klems_luiss_intan_0 <- readRDS(tmp)

attr(attributes(dat_klems_luiss_intan_0), "label")

# Muuttujien nimet
labels_intan <-
  sapply(dat_klems_luiss_intan_0, attr, "label") |>
  as_tibble(rownames = "label")

# View(labels_intan)

dat_klems_luiss_intan <-
  dat_klems_luiss_intan_0 |>
  mutate(year = as.numeric(year)) |>
  mutate(across(where(is.character), as_factor)) |>
  # to drop labels
  mutate(across(where(is.numeric), as.numeric)) |>
  rename(geo = geo_code, time = year, nace_r2 = nace_r2_code) |>
  pivot_longer(cols = !c(nace_r2:time), names_to = "vars", values_to = "values") |>
  mutate(vars = as_factor(vars)) |>
  # fix vars names so that VA names are equal with investment names in same indicator group
  mutate(vars = fct_recode(vars,
                           I_VA = "VA_CP",
                           I_VAadj = "VAadj",
                           Ipyp_VA = "VA_PYP",
                           Ipyp_VAadj = "VAadj_pyp",
                           Iq_VA = "VA_Q",
                           Iq_VAadj = "VAadj_q",
                           Ip_VA = "VA_PI",
                           Ip_VAadj = "VAadj_pi")) |>
  separate(vars, c("ind" ,"vars"), sep = "_", extra = "merge")

# US do not have Ipyp series that are used for aggragation. They are calcultated for cp ja q series.

dat_klems_luiss_intan_fix_us <-
  dat_klems_luiss_intan |>
  filter(geo == "US") |>
  pivot_wider(names_from = ind, values_from = values, names_prefix = "tmp_") |>
  group_by(geo, nace_r2, vars) |>
  mutate(tmp_Ipyp = statfitools::pp(tmp_I, tmp_Iq, time)) |>
  ungroup() |>
  pivot_longer(cols = starts_with("tmp_"), names_prefix = "tmp_", names_to = "ind", values_to = "values")

dat_klems_luiss_intan_us_fixed <-
  dat_klems_luiss_intan |>
  filter(geo != "US") |>
  bind_rows(dat_klems_luiss_intan_fix_us)

data_luiss_intan_groups_detail <-
  dat_klems_luiss_intan_us_fixed |>
  # only cp and pyp series that can be summarised
  filter(ind %in% c("HK", "HKpyp", "I", "Ipyp", "K", "Kpyp")) |>
  mutate(nace_r2 = forcats::fct_recode(nace_r2, TOTAL = "TOT")) |>
  filter(nace_r2 %in% main_nace_sna,
         !(vars %in% c("EconComp", "Innovprop"))) |>
  # complete(geo, time, vars, nace_r2_code) |>
  group_by(geo, geo_name, time, ind, vars) %>%
  summarise(
    total = values[nace_r2 == "TOTAL"],
    private = sum(values[!(nace_r2 %in% c("TOTAL", "C26"))]),
    private_ex26 = private - values[nace_r2 == "C26"],
    manu = sum(values[nace_r2 == "C"]),
    manu_ex26 = manu - values[nace_r2 == "C26"],
    service = sum(values[nace_r2 %in% c(c("G", "H", "I", "J", "M", "N"))]),
    inform = values[nace_r2 == "J"]) %>%
  ungroup() %>%
  gather(nace0, values, total, private, private_ex26, manu, manu_ex26, service, inform) %>%
  mutate(nace0 = as_factor(nace0)) |>
  pivot_wider(names_from = ind, values_from = values, names_prefix = "ind_") |>
  group_by(geo, geo_name, nace0, vars) |>
  mutate(ind_Iq = fp(ind_I, ind_Ipyp, time, 2000)) %>%
  ungroup() %>%
  pivot_longer(cols = starts_with("ind_"), names_to = "ind", values_to = "values", names_prefix = "ind_")

data_luiss_intan_groups_main <-
  dat_klems_luiss_intan_us_fixed |>
  # only cp and pyp series that can be summarised
  filter(ind %in% c("HK", "HKpyp", "I", "Ipyp", "K", "Kpyp")) |>
  mutate(nace_r2 = forcats::fct_recode(nace_r2, TOTAL = "TOT")) |>
  filter(nace_r2 %in% main_nace_sna) |>
  # complete(geo, time, vars, nace_r2_code) |>
  # To fix error in Innovprop main series are calculated
  filter(!(vars %in% c("Innovprop", "EconComp"))) %>%
  mutate(vars = fct_recode(vars,
                           Innovprop = "RD", Innovprop = "Design", Innovprop = "NFP", Innovprop = "OIPP",
                           EconComp = "OrgCap", EconComp = "Brand", EconComp = "Train")) %>%
  filter(!(vars %in% c("Brand", "Design", "NFP", "OIPP", "OrgCap", "RD", "Train"))) %>%
  group_by(geo, geo_name, time, ind, vars, nace_r2) %>%
  summarise(values = sum(values)) %>%
  group_by(geo, geo_name, time, ind, vars) %>%
  summarise(
    total = values[nace_r2 == "TOTAL"],
    private = sum(values[!(nace_r2 %in% c("TOTAL", "C26"))]),
    private_ex26 = private - values[nace_r2 == "C26"],
    manu = sum(values[nace_r2 == "C"]),
    manu_ex26 = manu - values[nace_r2 == "C26"],
    service = sum(values[nace_r2 %in% c(c("G", "H", "I", "J", "M", "N"))]),
    inform = values[nace_r2 == "J"]) %>%
  ungroup() %>%
  gather(nace0, values, total, private, private_ex26, manu, manu_ex26, service, inform) %>%
  mutate(nace0 = as_factor(nace0)) |>
  pivot_wider(names_from = ind, values_from = values, names_prefix = "ind_") |>
  group_by(geo, geo_name, nace0, vars) |>
  mutate(ind_Iq = fp(ind_I, ind_Ipyp, time, 2000)) %>%
  ungroup() %>%
  pivot_longer(cols = starts_with("ind_"), names_to = "ind", values_to = "values", names_prefix = "ind_")

usethis::use_data(data_luiss_intan_groups_detail, data_luiss_groups, data_luiss_intan_groups_main, data_luiss_groups_tfp,  overwrite = TRUE)

data_luiss_intan_groups_main %>% write.csv2(file = "data_luiss_intan_groups_main.csv")
