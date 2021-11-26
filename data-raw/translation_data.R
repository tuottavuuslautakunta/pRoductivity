
library(stringr)


trans_source <-
  scan("data-raw/trans_source.csv", what = "character", sep = "\n") %>%
  str_split( "/") %>% unlist() %>%
  str_replace("\\\\n" ,"\\\n") |>
  # / in key are replaced with § to avoid split
  str_replace("§", "/")


trans_target <-
  scan("data-raw/trans_target.csv", what = "character", sep = "\n") %>%
  str_split( "/") %>% unlist()%>%
  str_replace("\\\\n" ,"\\\n")|>
  str_replace("§", "/")

fig_translation <- purrr::set_names(trans_target, trans_source)

use_data(fig_translation, overwrite = TRUE)

# grep("Cz", fig_translation, value = TRUE)
