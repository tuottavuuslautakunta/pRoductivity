
## Estimates SSCA series


library(here)
library(dplyr)
library(tidyr)

devtools::load_all()



## estimation data

data(data_main_groups, data_main10_groups, data_total, base_year)



# longer data, Irland is excluded
ssca_dat10 <- data_main10_groups %>%
  filter(geo != "IE")

# more detailed data
ssca_dat <- data_main_groups%>%
  filter(geo != "IE")

# Data for export analysis
scca_exp_data <- data_total %>%
  group_by(geo) %>%
  mutate(p6_ind = rebase(P6__CLV15_MNAC, time, base_year),
         p61_ind = rebase(P61__CLV15_MNAC, time, base_year),
         p62_ind = rebase(P62__CLV15_MNAC, time, base_year))

# Estimation periods
est_periods <- 1996:(base_year - 1)
eval_periods <- base_year:2018
eval_periods10 <- base_year:2019


## Estimation

# Estimatio for longer data
ssca10_est_results <-
  map(set_names(c("lp_ind", "va_ind", "h_ind")),
      ssca_est_loop,
      .data = ssca_dat10,
      unit_var = "geo",
      target_unit = "FI",
      time_var = "time",
      est_periods = est_periods,
      eval_periods = eval_periods10)

# Estimation for more detailed data, including ex26
ssca_est_results <-
  map(set_names(c("lp_ind", "va_ind", "h_ind")),
      ssca_est_loop,
      .data = ssca_dat,
      unit_var = "geo",
      target_unit = "FI",
      time_var = "time",
      est_periods = est_periods,
      eval_periods = eval_periods)




usethis::use_data(ssca10_est_results, ssca_est_results, overwrite = TRUE)




ssca_exp_est_results <- map(set_names(c("p6_ind", "p61_ind", "p62_ind")),
                            ~ssca_est(scca_exp_data, unit_var = "geo",
                                      target_unit = "FI",
                                      value_var = .x,
                                      time_var = "time",
                                      est_periods = est_periods,
                                      eval_periods = eval_periods))


usethis::use_data(ssca_exp_est_results, overwrite = TRUE)
