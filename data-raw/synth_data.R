
## Estimates synth series


library(here)
library(dplyr)
library(tidyr)
library(tidysynth)

devtools::load_all()



## estimation data

data(data_main_groups, data_main10_groups, data_total, base_year)



# longer data, Irland is excluded
synth_dat10 <- data_main10_groups |>
  filter(geo %in% countries_synth)


# more detailed data
synth_dat <- data_main_groups |>
  filter(geo %in% countries_synth)

# Data for export analysis
# synth_exp_data <- data_total %>%
#   filter(geo %in% countries_synth) |>
#   group_by(geo) %>%
#   mutate(p6_ind = rebase(P6__CLV15_MNAC, time, base_year),
#          p61_ind = rebase(P61__CLV15_MNAC, time, base_year),
#          p62_ind = rebase(P62__CLV15_MNAC, time, base_year))

# Estimation periods



## Estimation

# Estimatio for longer data
synth10_est_results <-
  map(set_names(c("lp_ind", "va_ind", "h_ind")),
      synth_est_loop,
      .data = synth_dat10,
      i_time = 2008)

# Estimation for more detailed data, including ex26
synth_est_results <-
  map(set_names(c("lp_ind", "va_ind", "h_ind")),
      synth_est_loop,
      .data = synth_dat,
      i_time = 2008)




usethis::use_data(synth10_est_results, synth_est_results, overwrite = TRUE)


