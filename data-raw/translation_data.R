
library(stringr)

# 2021
#
# trans_source <-
#   scan("data-raw/trans_source.csv", what = "character", sep = "\n") %>%
#   str_split( "/") %>% unlist() %>%
#   str_replace("\\\\n" ,"\\\n") |>
#   # / in key are replaced with § to avoid split
#   str_replace("§", "/")
#
#
# trans_target <-
#   scan("data-raw/trans_target.csv", what = "character", sep = "\n") %>%
#   str_split( "/") %>% unlist()%>%
#   str_replace("\\\\n" ,"\\\n")|>
#   str_replace("§", "/")
#
# fig_translation_2021 <- purrr::set_names(trans_target, trans_source)

fig_translation_0 <- read.csv2("data-raw/Avain_FI_EN.csv", encoding = "latin1")|>
  distinct() |>
  mutate(across(everything(), .fns = ~str_replace(.x, "\\\\n" ,"\\\n")))

fig_translation <- purrr::set_names(fig_translation_0$Englanti, fig_translation_0$Suomi)

usethis::use_data(fig_translation, overwrite = TRUE)
# grep("Cz", fig_translation, value = TRUE)
