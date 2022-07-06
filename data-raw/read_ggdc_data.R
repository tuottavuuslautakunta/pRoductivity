
# map

library(tidyverse)

ggdc_file <- here::here("data-raw", "benchmark_2005.xlsx")

data_ggdc_level05 <- readxl::read_xlsx(ggdc_file, sheet = "LP_GDP_MajorSector", skip = 1) |>
  select(-...4) |>
  statfitools::clean_names(to_lower = TRUE) |>
  mutate(geo = countrycode::countrycode(iso_code, "iso3c", destination = "eurostat"),
         geo_nimi = countrycode::countrycode(iso_code, "iso3c", destination = "cldr.short.fi")) |>
  select(geo, geo_nimi, private = market_economy, manu = manufacturing, service = market_services)|>
  mutate_at(vars(geo, geo_nimi), as_factor) |>
  mutate(across(!where(is.factor), as.numeric)) |>
  gather(nace0, values, -geo, -geo_nimi) |>
  mutate(nace0 = as_factor(nace0)) |>
  filter(geo %in% countries)

usethis::use_data(data_ggdc_level05, overwrite = TRUE)


# readxl::excel_sheets(ggdc_file)

ggdc_read_fun <- function(sheet, skip){
  readxl::read_xlsx(ggdc_file, sheet = sheet, skip = skip) |>
    # select(-...4) |>
    statfitools::clean_names(to_lower = TRUE) |>
    mutate(geo = countrycode::countrycode(iso_code, "iso3c", destination = "eurostat"),
           geo_nimi = countrycode::countrycode(iso_code, "iso3c", destination = "cldr.short.fi")) |>
    select(-country, -iso_code)|>
    mutate_at(vars(geo, geo_nimi), as_factor) |>
    mutate(across(!where(is.factor), as.numeric)) |>
    gather(nace0, values, -geo, -geo_nimi) |>
    mutate(nace0 = as_factor(nace0)) |>
    filter(geo %in% countries)
}

## All major tables
data_ggdc_main_level05 <-
  map(set_names((c("VA_GDP_MajorSector", "LP_GDP_MajorSector", "GO_GDP_MajorSector"))), ggdc_read_fun, skip = 1) |>
  bind_rows(.id = "vars") |>
  mutate(vars = as_factor(vars))

## 35 industries
data_ggdc_35_level05 <-
  map(set_names((c("GO_35Industry"))), ggdc_read_fun, skip = 2) |>
  bind_rows(.id = "vars") |>
  mutate(vars = as_factor(vars))

## 10 Industries
# Mapping to nace_r2, jtk includes M and N, i includes H and J. J is taken form 35 industries
nace_ggdc_key <-
  c(atb = "A", c = "B", d = "C", e = "E", f = "F", g = "G", i = "H", h = "I", jtk = "M")

data_ggdc_10_go_level05 <-
  map(set_names((c("GO_10Sector"))), ggdc_read_fun, skip = 2) |>
  bind_rows(.id = "vars") |>
  mutate(vars = as_factor(vars)) |>
  rename(nace_ggdc = nace0) |>
  mutate(nace_r2 = recode(nace_ggdc, !!!nace_ggdc_key)) |>
  select(-nace_ggdc) |>
  spread(nace_r2, values) |>
  mutate(N = M) |>
  left_join(data_ggdc_35_level05 |>
              select(geo, nace0, values) |>
              filter(nace0 == "x64") |>
              spread(nace0, values)) |>
  rename(J = x64) |>
  gather(nace_r2, go_level05, -geo, -geo_nimi, -vars)

usethis::use_data(data_ggdc_10_go_level05, overwrite = TRUE)



# data_ggdc_main_level05 |>
#   filter(geo %in% geo_level,
#          nace0 == "market_services") |>
#   ggplot(aes(vars, values, fill = geo)) +
#   geom_col(position = "dodge")
#
# data_ggdc_main_level05 |>
#   filter(geo %in% geo_level,
#          nace0 == "market_services",
#          vars != "LP_GDP_MajorSector") |>
#   ggplot(aes(geo, values, fill = vars)) +
#   geom_col(position = "dodge")
