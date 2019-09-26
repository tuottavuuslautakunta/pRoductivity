
library(readr)
library(tidyverse)

# Labels

labels_klems_0 <- scan("http://www.euklems.net/TCB/2018/ALL_output_Readme_17ii.txt",
                       what =  character(),
                       sep = "\n")

labels_klems <-
  list(org = labels_klems_0) %>%
  as_tibble() %>%
  filter(!str_detect(org, "Source:")) %>%    # remove last line
  # label blocks starts after Description and there are 3 of them
  mutate(ind = str_detect(org, "Description"),
         type = cumsum(ind)) %>%
  filter(type > 0, !ind) %>%
  mutate(type = recode_factor(type,
                              `1` = "country",
                              `2` = "variable",
                              `3` = "industry")) %>%
  separate(org, c("code", "label"), sep = "\t") %>%
  mutate(label = if_else(str_detect(code, "^EU[0-9]"), code, label))



# data

dat_klems_0 <- read_csv("http://www.euklems.net/TCB/2018/ALL_output_17ii.txt",
                        col_types = cols(.default = col_double(),
                                         country = col_character(),
                                         var = col_character(),
                                         code = col_character()))

dat_klems <-
  dat_klems_0 %>%
  gather(time, values, - country, -var, -code) %>%
  mutate(time = parse_number(time),
         geo = as_factor(country),
         country = as_factor(plyr::mapvalues(geo, labels_klems$code,
                                             labels_klems$label, warn_missing = FALSE)),
         variable = as_factor(plyr::mapvalues(var, labels_klems$code,
                                             labels_klems$label, warn_missing = FALSE)),
         industry = as_factor(plyr::mapvalues(code, labels_klems$code,
                                             labels_klems$label, warn_missing = FALSE)))




use_data(dat_klems, labels_klems, overwrite = TRUE)
