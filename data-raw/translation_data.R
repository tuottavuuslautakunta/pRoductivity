



trans_source <-
  scan("data-raw/trans_source.csv", what = "character", sep = "\n") %>%
  str_split( "/") %>% unlist() %>%
  str_replace("\\\\n" ,"\\\n")


trans_target <-
  scan("data-raw/trans_target.csv", what = "character", sep = "\n") %>%
  str_split( "/") %>% unlist()%>%
  str_replace("\\\\n" ,"\\\n")

fig_translation <- set_names(trans_target, trans_source)

use_data(fig_translation, overwrite = TRUE)

# grep("Cz", fig_translation, value = TRUE)
