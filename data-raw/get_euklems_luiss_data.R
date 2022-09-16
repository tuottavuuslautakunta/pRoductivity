
library(readr)
library(tidyverse)

# Labels

labels_klems_0 <- scan("http://www.euklems.net/TCB/2018/ALL_output_Readme_17ii.txt",
                       what =  character(),
                       sep = "\n")




# data

tmp <- tempfile(fileext = ".rds")
download.file("https://www.dropbox.com/s/8osl84xzo6952pt/growth%20accounts.rds?dl=1", destfile = tmp, mode = "wb")

dat_klems_luiss_0 <- readRDS(tmp)

dat_klems_luiss <-
  dat_klems_luiss_0 |>
  mutate(year = as.numeric(year)) |>
  mutate(across(where(is.character), as_factor)) |>
  rename(geo = geo_code, time = year)



use_data(dat_klems_luiss,  overwrite = TRUE)
