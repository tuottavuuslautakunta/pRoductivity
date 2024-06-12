

library(tidyverse)
devtools::load_all()

bea_datasets()

params <- list(
  datasetname = "GDPbyIndustry",
  Frequency = "A",
  Industry = "ALL",
  TableID = "1",
  Year = "ALL"
)
bea_data <- bea_get_data(params)


bea_parameters("UnderlyingGDPbyIndustry")
bea_parameter_values("UnderlyingGDPbyIndustry", "TableID")

params_u <- list(
  datasetname = "UnderlyingGDPbyIndustry",
  Frequency = "A",
  Industry = "31G",
  TableID = "210",
  Year = "ALL"
)
bea_data_u <- bea_get_data(params_u)
