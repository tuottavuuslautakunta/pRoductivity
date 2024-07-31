

library(tidyverse)
devtools::load_all()

# bea_datasets()

# bea_parameters("GDPbyIndustry")
# bea_parameter_values("GDPbyIndustry", "Industry")
# bea_parameter_values("GDPbyIndustry", "TableID")
# bea_parameters("NIPA")
# bea_parameter_values("NIPA", "TableID")
# bea_parameter_values("NIPA", "Industry")

bea_activity <- c(TOTAL = "GDP", C = "31G", C26 = "334", F = "23", G = "42", G = "44RT", H = "48TW",
  I = "72", J = "51", M = "54", N = "561")

# Value Added by Industry
params1 <- list(
  datasetname = "GDPbyIndustry",
  Frequency = "A",
  Industry = bea_activity,
  TableID = "1",
  Year = "ALL"
)

bea_data1 <-
  bea_get_data(params1) |>
  mutate(
    currency = "USD",
    vars = "B1G__CP_MNAC"
  )

# Real Value Added by Industry

params2 <- list(
  datasetname = "GDPbyIndustry",
  Frequency = "A",
  Industry = bea_activity,
  TableID = "10",
  Year = "ALL"
)

bea_data2 <-
  bea_get_data(params2) |>
  mutate(
    currency = "USD",
    vars = "B1G__CLV15_MNAC"
  )

# Compensation of employees

params3 <- list(
  datasetname = "GDPbyIndustry",
  Frequency = "A",
  Industry = bea_activity,
  TableID = "6",
  Year = "ALL"
)

bea_data3 <-
  bea_get_data(params3) |>
  filter(IndustrYDescription == "Compensation of employees") |>
  mutate(
    currency = "USD",
    vars = "D1__CP_MNAC"
  )

dat_bea <-
  bind_rows(
    bea_data1,
    bea_data2,
    bea_data3
  ) |>
  transmute(
    geo = factor("US"),
    industry = as_factor(Industry),
    time = as.numeric(Year),
    currency = as_factor(currency),
    vars = as_factor(vars),
    values = 1000 * as.numeric(DataValue)
  ) |>
  spread(vars, values) |>
  # lasketaaan edellisen vuoden hinnoin ja aggregoidaan whole ja retail trade yhteen
  group_by(industry) |>
  mutate(B1G__PYP_MNAC = statfitools::pp(B1G__CP_MNAC, B1G__CLV15_MNAC, time)) |>
  ungroup() |>
  mutate(nace_r2 = fct_recode(industry, !!!bea_activity)) |>
  select(-industry) |>
  group_by(geo, nace_r2, time, currency) |>
  summarise(across(everything(), sum)) |>
  ungroup() |>
  # lasketaan kiinteähintainen uudelleen
  group_by(geo, nace_r2, currency) |>
  mutate(B1G__CLV15_MNAC = statfitools::fp(B1G__CP_MNAC, B1G__PYP_MNAC, time, 2015)) |>
  ungroup() |>
  # VA hintaindeksi
  mutate(B1G__P = B1G__CP_MNAC / B1G__CLV15_MNAC)

usethis::use_data(dat_bea, overwrite = TRUE)


# # NIPA 6.4 Full-Time and Part-Time Employees by Industry
#
# params4 <- list(
#   datasetname = "NIPA",
#   Frequency = "A",
#   Year = "ALL"
# )
#
# bea_data4 <- bind_rows(
#   # A ja B sisältää vanhempia tietoja
#   # bea_get_data(c(params4, TableName = c("T60400A"))),
#   # bea_get_data(c(params4, TableName = c("T60400B"))),
#   bea_get_data(c(params4, TableName = c("T60400C"))),
#   bea_get_data(c(params4, TableName = c("T60400D")))
# )
#
# bea_data4_2 <-
#   bea_data4 |>
#   filter(LineDescription %in% unique(bea_data2$IndustrYDescription),
#          TimePeriod %in% unique(bea_data2$Year))
#
#
#   bea_get_data(params4) |>
#   filter(IndustrYDescription == "Compensation of employees") |>
#   mutate(
#     currency = "USD",
#     vars = "B1G_CLV15_MNAC"
#   )



