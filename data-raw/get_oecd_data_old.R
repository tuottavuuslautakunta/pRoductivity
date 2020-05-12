
# OECD data

library(OECD)
library(tidyverse)
library(countrycode)

dataset_list <- get_datasets()
search_dataset("STAN", data = dataset_list) # %>% View()
search_dataset("National account", data = dataset_list) # %>% View()

stan_str <- get_data_structure("STANI4_2016")
NAAG_str <- get_data_structure("NAAG")


stan_str$VAR %>% knitr::kable()
stan_str$IND %>% knitr::kable()
stan_str$LOCATION %>% knitr::kable()
# ind_list <- stan_str$IND %>%
#   filter(label %in% c(" Business sector services excluding Real estate", " Manufacturing [C]",
#                       " Non-agriculture business sector excluding Real estate", " High R&D intensive activities (2-digit definition)")) %>%
#   pull(id)

# ind_list <- unique(dat_nama_10_a64_market$industry)
ind_list_main <- nace_stan

# location_list <- c("USA", "FIN", "SWE", "NOR", "GBR")
location_list_main <- c("USA", "JPN")

# Huom!
var_list <- c(B1G__CP_MNAC = "VALU", B1G__CLV10_MNAC = "VALK", B1G_PYP_MNAC = "VKPY",
              EMP_DC__THS_PER = "EMPN", EMP_DC__MIL_HW = "HRSN",
              SAL_DC__THS_PER = "EMPE", SAL_DC__MIL_HW = "HRSE")

# dat_stan_0 <- get_dataset("STANI4_2016", filter = list(location_list, c("VALU", "VALK", "VKPY", "EMPN", "HRSN"), ind_list))

dat_stan_0 <- get_dataset("STANI4_2016", filter = list(location_list_main, var_list, ind_list_main))



dat_stan <-
  dat_stan_0 %>%
  transmute(
    time = as.numeric(obsTime),
    geo = as_factor(countrycode(LOCATION, "iso3c", "eurostat", nomatch = NULL)),
    nace_r2 = fct_recode(IND, !!!ind_list_main),
    na_item = fct_recode(VAR, !!!var_list),
    values = obsValue) %>%
  filter(time >= start_year) %>%
  spread(na_item, values) %>%
  mutate(EMP_DC__THS_HW = EMP_DC__MIL_HW * 1000) %>%
  select(- EMP_DC__MIL_HW) %>%
  complete(time, geo, nace_r2)

dat_stan %>%
  filter(time < 2017) %>%
  # filter(nace_r2 != "C26") %>%
  # filter(nace_r2 != "N") %>%
  # filter(!(nace_r2 == "M" & time < 2004)) %>%
  visdat::vis_dat()

use_data(dat_stan, overwrite = TRUE)
## Population

# search_dataset("population", data = dataset_list)
#
# pop_str <- get_data_structure("ALFS_POP_LABOUR")
