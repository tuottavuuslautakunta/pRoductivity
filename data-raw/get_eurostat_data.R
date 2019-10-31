
load_all()
library(eurostat)

nama_10_a64_0 <- eurostat::get_eurostat("nama_10_a64", time_format = "num", cache = FALSE)
nama_10_a64_e_0 <- eurostat::get_eurostat("nama_10_a64_e", time_format = "num", cache = FALSE)


nama_10_a64 <- nama_10_a64_0 %>%
  filter(unit %in% c("CLV10_MEUR", "CP_MNAC", "PYP_MNAC"),
         na_item == "B1G") %>%
  unite(vars, na_item, unit, sep = "__") %>%
  mutate(vars = as_factor(vars)) %>%
  spread(vars, values)

nama_10_a64_e <- nama_10_a64_e_0 %>%
  filter(unit %in% c("THS_HW", "THS_PER"),
         na_item == "EMP_DC") %>%
  unite(vars, na_item, unit, sep = "__") %>%
  mutate(vars = as_factor(vars)) %>%
  spread(vars, values)

dat_nama_10_a64 <-
  nama_10_a64 %>%
  left_join(nama_10_a64_e, by = c("nace_r2", "geo", "time")) %>%
  mutate_at(c("geo", "nace_r2"), as_factor) %>%
  mutate(sektori = as_factor(substr(nace_r2, 1, 1)),
         sektori = recode(sektori, M = "M_N", N = "M_N")) %>%
  # lisää sektoritason keskimääräiset tunnit ?
  #
  # Valitut toimialat
  mutate(industry = case_when(
           nace_r2 == "B" ~ "D05T09",
           nace_r2 == "C10-C12" ~ "D10T12",
           nace_r2 == "C13-C15" ~ "D13T15",
           nace_r2 == "C16" ~ "D16",
           nace_r2 == "C17" ~ "D17",
           nace_r2 == "C18" ~ "D18",
           nace_r2 == "C19" ~ "D19T23",
           nace_r2 == "C20"  ~ "D19T23",
           nace_r2 == "C21" ~ "D19T23",
           nace_r2 == "C22_C23" ~ "D19T23",
           nace_r2 == "C24_C25" ~ "D24T25",
           nace_r2 == "C24_C25" ~ "D24T25",
           nace_r2 == "C26"  ~ "D26",
           nace_r2 == "C27" ~ "D27",
           nace_r2 == "C28" ~ "D28",
           nace_r2 == "C29_C30" ~ "D29T30",
           nace_r2 == "C31-C33" ~ "D31T33",
           nace_r2 == "D" ~ "D35T39",
           nace_r2 == "E" ~ "D35T39",
           nace_r2 == "F" ~ "D41T43",
           nace_r2 == "G" ~ "D45T47",
           nace_r2 == "H" ~ "D49T53",
           nace_r2 == "I" ~ "D55T56",
           nace_r2 == "J58-J60" ~ "D58T60",
           nace_r2 == "J61" ~ "D61",
           nace_r2 == "J62_J63" ~ "D62T63",
           nace_r2 == "M_N" ~ "D69T82",
           nace_r2 == "R" ~ "D90T93",
           nace_r2 == "S" ~ "D94T96",
           TRUE ~ NA_character_)) %>%
  mutate(industry = as_factor(industry))

dat_nama_10_a64_keep_I <- dat_nama_10_a64 %>%
  filter(!is.na(industry)) %>%
  droplevels()

dropped_I <- c("A", "K", "L", "O", "P", "Q", "T", "U")


keep_II <- c("D10T12", "D13T15", "D16", "D17", "D18", "D19T23", "D24T25",
             "D26", "D27" , "D28", "D29T30", "D31T33", "D35T39", "D41T43",
             "D45T47", "D49T53", "D55T56", "D58T60", "D61", "D62T63", "D69T82")

dat_nama_10_a64_keep_I %>%
  filter(!(industry %in% keep_II)) %>%
  droplevels() %>%
  pull(nace_r2) %>%
  levels()

# Also E ja F

dropped_II <- c("B", "R", "S")

dat_nama_market <- dat_nama_10_a64_keep_I %>%
  filter(industry %in% keep_II) %>%
  droplevels() %>%
  mutate(nace = eurostat::label_eurostat(nace_r2, dic = "nace_r2"))

# dat_nama_10_a64_market %>%
#   distinct(nace_r2, industry)


use_data(dat_nama_10_a64, dat_nama_10_a64_keep_I, dat_nama_market, dropped_I, dropped_II, overwrite = TRUE)
