# OECD annual national account data

library(OECD)
library(tidyverse)
library(countrycode)

devtools::load_all()

# dataset_list <- get_datasets()

# dataset_list %>%
#   filter(id %in% c("SNA_TABLE6A", "SNA_TABLE7A")) %>%
#   knitr::kable()

# dataset_list %>%
#   filter(grepl("LFS", id)) %>%
#   knitr::kable()
#
pdb_lv_str <- get_data_structure("PDB_LV")

#
pdb_lv_str$SUBJECT
pdb_lv_str$MEASURE


# Countries
geo_levels <- countrycode(c("US", "CH", "DE", "DK", "ES", "FI", "FR", "IT", "NL", "SE"), "eurostat", "iso3c")



# Measeures
pdb_measures <-   c(
  cp_ppp = "VPVOB"                # USD, constant prices, 2015 PPPs
  )

# Subjects
pdb_subjects <-   c(
  gdp_per_h = "T_GDPHRS"             # GDP per hour worked
  )

dat_oecd_levels0 <- get_dataset(dataset = "PDB_LV",
                               filter = list(geo_levels, pdb_subjects, pdb_measures))




dat_oecd_levels <- dat_oecd_levels0 %>%
  transmute(
    time = as.numeric(obsTime),
    geo = as_factor(countrycode(LOCATION, "iso3c", "eurostat", nomatch = NULL)),
    vars = fct_recode(SUBJECT, !!!pdb_subjects),
    unit = fct_recode(MEASURE, !!!pdb_measures),
    currency = as_factor(UNIT),
    values = obsValue) %>%
  mutate(vars = as_factor(vars)) %>%
  spread(vars, values) %>%
  filter(time >= 1970) %>%
  droplevels() %>%
  complete(time, geo) %>%
  mutate(geo_name = as_factor(countrycode(geo, "eurostat", "cldr.name.fi", nomatch = NULL)))




usethis::use_data(dat_oecd_levels, geo_levels, overwrite = TRUE)
