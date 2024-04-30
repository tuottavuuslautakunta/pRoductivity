
library(tidyverse)
library(blsR)
library(bea.R)

# key set in .Rprofile
beakey <- Sys.getenv("BEA_API_KEY")

k <- query_popular_series()
get_series_tables(
  list(uer.men ='LNS14000001', uer.women = 'LNS14000002'), api_key = getOption("bls.key"))

lp_us_manu_q <- get_series_tables(
  list(lp_ind ="PRS30006093"),
  start_year = 2002, end_year = 2021, api_key = getOption("bls.key"))

us_business_q <- get_series_tables(
  list(
    lp_ind ="PRS85006093",
    va_ind = 'PRS85006043',
    h_ind = "PRS85006033"),
  api_key = getOption("bls.key"),
  start_year = 2002,
  end_year = 2021
)



business_bls <-
  us_business_q |>
  merge_tables() |>
  select(-period) |>
  group_by(year) |>
  summarise(across(c(lp_ind, va_ind, h_ind), mean)) |>
  mutate(across(c(lp_ind, va_ind, h_ind), ~rebase(.x, year, 2007))) |>
  ungroup() |>
  mutate(id = "bls") |>
  rename(time = year)

  # statfitools::clean_times(year_col = "year", sub_year_col = "period")

data_main10_groups |>
  filter(geo == "US",
         nace0 == "private") |>
  select(time, lp_ind, va_ind, h_ind) |>
  mutate(id = "oma") |>
  bind_rows(business_bls) |>
  gather(vars, value, lp_ind, va_ind, h_ind) |>
  ggplot(aes(time, value, colour = id)) +
  geom_line() +
    facet_wrap(~vars)

# Datasets

bea_datasets <- beaSets(beaKey = beakey)

bea_datasets[[1]] |> View()

beaParams("NIPA", beaKey = beakey)
beaParamVals("NIPA", "TableID", beaKey = beakey)

beaSpecs <- list(
  'UserID' = beakey ,
  'Method' = 'GetData',
  'datasetname' = 'NIPA',
  'TableName' = 'T60200A',
  'Frequency' = 'A',
  'Year' = 'X',
  'ResultFormat' = 'json'
)

#
dat_bea0 <- beaGet(beaSpecs)

dat_bea <- dat_bea0 |>
  pivot_longer(starts_with("DataValue_"), names_to = "year", values_to = "values") |>
  mutate(year = as.numeric(str_remove(year, "DataValue_"))) |>
  mutate(across(where(is.character), as_factor))

dat_bea |> pull(LineDescription) |> unique()

beaParams("GDPbyIndustry", beaKey = beakey)
beaParamVals("GDPbyIndustry", "TableID", beaKey = beakey)
beaParamVals("GDPbyIndustry", "Industry", beaKey = beakey)

beaSpecs <- list(
  'UserID' = beakey ,
  'Method' = 'GetData',
  'datasetname' = 'GDPbyIndustry',
  'TableName' = "T10101",
  "Industry" = 11,
  'Frequency' = 'A',
  'Year' = 'X'
)

bea_va_comp <- beaGet(beaSpec = beaSpecs, asTable = TRUE)

userSpecList <- list('UserID' = beakey ,
                     'Method' = 'GetData',
                     'datasetname' = 'NIPA',
                     'Frequency' = 'A',
                     'TableID' = '68',
                     'Year' = 'X')
BDT <- beaGet(beaSpec = userSpecList, asTable = TRUE)


userSpecList <- list('UserID' = beakey ,
                     'Method' = 'GetData',
                     'datasetname' = 'NIPA',
                     'Frequency' = 'A',
                     'TableID' = '68',
                     'Year' = 'X')
resp <- beaGet(userSpecList)
BDT <- bea2Tab(resp)
