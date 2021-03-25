library(tidyverse)
library(vroom)

estimated_r_url <- "https://github.com/crondonm/TrackingR/blob/main/Estimates-Database/database.csv?raw=true"
raw_df <- vroom(estimated_r_url)

est_r <- raw_df %>%
  janitor::clean_names() %>%
  mutate(
    iso3c = countrycode::countrycode(
      country_region,
      origin = "country.name.en",
      destination = "iso3c"
    ),
    iso3c = case_when(
      country_region == "Kosovo" ~ "KSV",
      country_region == "World" ~ "WORLD",
      TRUE ~ iso3c
    )
  ) %>%
  mutate_at(
    vars(country_region, iso3c),
    factor
  )

saveRDS(est_r, file = "estimated-r-pone-0244474/data/est_r.rds")
