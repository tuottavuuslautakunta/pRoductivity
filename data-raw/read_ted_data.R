

ted_file <- here::here("data-raw", "TED_1_AUGUST20211.xlsx")

data_ted <- readxl::read_xlsx(ted_file, sheet = "DATA", skip = 4) |>
  rename_with(tolower) |>
  mutate(geo = countrycode::countrycode(iso, "iso3c", destination = "eurostat"),
         geo_nimi = countrycode::countrycode(iso, "iso3c", destination = "cldr.short.fi")) |>
  mutate_at(vars(region, iso, country, indicator, measure, geo), as_factor) |>
  gather(time, value, -region, -iso, -country, -indicator, -measure, -geo, -geo_nimi) |>
  mutate(time = as.numeric(time)) |>
  filter(geo %in% countries)

usethis::use_data(data_ted, overwrite = TRUE)


