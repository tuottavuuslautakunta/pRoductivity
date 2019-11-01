
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

ind_list <- unique(dat_nama_10_a64_market$industry)
ind_list_main <- setNames(names(main_nace), main_nace)

location_list <- c("USA", "FIN", "SWE", "NOR", "GBR")
location_list_main <- c("USA")

# Huom!
var_list <- c(B1G__CP_MNAC = "VALU", B1G__CLV10_MNAC = "VALK", B1G_PYP_MNAC = "VKPY",
              EMP_DC__THS_PER = "EMPN", EMP_DC__MIL_HW = "HRSN")

dat_stan_0 <- get_dataset("STANI4_2016", filter = list(location_list, c("VALU", "VALK", "VKPY", "EMPN", "HRSN"), ind_list))

dat_stan_main_0 <- get_dataset("STANI4_2016", filter = list(location_list_main, var_list, ind_list_main))

dat_stan <-
  dat_stan_0 %>%
  mutate(geo = as_factor(LOCATION),
         vars = as_factor(VAR),
         ind = as_factor(IND),
         time = as.numeric(obsTime),
         values = obsValue) %>%
  select(time, geo, vars, ind, values) %>%
  mutate(industry = plyr::mapvalues(ind, stan_str$IND$id, stan_str$IND$label, warn_missing = FALSE)) %>%
  spread(vars, values) %>%
  complete(time, geo, nesting(ind, industry)) %>%
  group_by(geo, ind) %>%
  mutate(lp = VALK / HRSN,
         lp_ind05 = 100 * lp/lp[time == 2005]) %>%
  ungroup()

dat_stan_main <-
  dat_stan_main_0 %>%
  transmute(geo = countrycode(LOCATION, "iso3c", "eurostat"),
         vars = fct_recode(VAR, !!!var_list),
         nace_r2 = fct_recode(IND, !!!ind_list_main),
         time = as.numeric(obsTime),
         values = obsValue) %>%
  spread(vars, values) %>%
  mutate(EMP_DC__THS_HW = EMP_DC__MIL_HW * 1000) %>%
  select(- EMP_DC__MIL_HW) %>%
  group_by(geo, nace_r2) %>%
  mutate(B1G__PYP_MNAC = statfitools::pp(cp = B1G__CP_MNAC, fp = B1G__CLV10_MNAC, time = time)) %>%
  ungroup()


use_data(dat_stan, overwrite = TRUE)
use_data(dat_stan_main, overwrite = TRUE)
## Population

# search_dataset("population", data = dataset_list)
#
# pop_str <- get_data_structure("ALFS_POP_LABOUR")
