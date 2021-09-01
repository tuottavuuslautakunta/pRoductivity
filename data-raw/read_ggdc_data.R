
# https://www.rug.nl/ggdc/productivity/pld/

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
