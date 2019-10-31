
# OECD data

library(OECD)
library(tidyverse)

dataset_list <- get_datasets()
search_dataset("STAN", data = dataset_list) # %>% View()
search_dataset("National account", data = dataset_list) # %>% View()

stan_str <- get_data_structure("STANI4_2016")
NAAG_str <- get_data_structure("NAAG")

ind_list <- stan_str$IND %>%
  filter(label %in% c(" Business sector services excluding Real estate", " Manufacturing [C]",
                      " Non-agriculture business sector excluding Real estate", " High R&D intensive activities (2-digit definition)")) %>%
  pull(id)

dat_stan_0 <- get_dataset("STANI4_2016", filter = list(c("USA", "FIN", "SWE"), c("HRSN", "VALK"), ind_list))

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


use_data(dat_stan, overwrite = TRUE)

## Population

search_dataset("population", data = dataset_list)

pop_str <- get_data_structure("ALFS_POP_LABOUR")
