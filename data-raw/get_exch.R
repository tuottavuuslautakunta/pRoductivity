
# Exchange rates from eurostat

currencies <- c(AU = "AUD", CA = "CAD", US = "USD", JP = "JPY", NZ = "NZD", CH = "CHF", DK = "DKK", SE = "SEK")

exh_eur_euro <-
  list(time = 1971:lubridate::year(Sys.time()),
         geo = eurostat::ea_countries$code,
         currency = "EUR",
         values = 1) |>
  expand.grid()

exh_eur_a <- get_eurostat("ert_bil_eur_a", filters = list(statinfo = "AVG", currency = currencies), time_format = "num") %>%
  select(time, currency, values) %>%
  mutate(geo = recode(currency, !!!purrr::set_names(names(currencies), currencies))) |>
  bind_rows(exh_eur_euro)



usethis::use_data(exh_eur_a, overwrite = TRUE)
