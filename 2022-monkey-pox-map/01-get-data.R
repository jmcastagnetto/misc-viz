library(tidyverse)

monkeypox_cases <- read_csv(
  file = "https://github.com/globaldothealth/monkeypox/raw/main/latest.csv",
  col_types = cols(
    .default = col_character(),
    ID = col_character(),
    Date_onset = col_date(format = ""),
    Date_confirmation = col_date(format = ""),
    Date_hospitalisation = col_date(format = ""),
    Date_isolation = col_date(format = ""),
    Date_entry = col_date(format = ""),
    Date_last_modified = col_date(format = "")
  )
)

saveRDS(
  monkeypox_cases,
  file = glue::glue("2022-monkey-pox-map/{Sys.Date()}-monkey-pox-cases.rds")
)
