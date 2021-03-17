library(tidyverse)
library(gt)

owid_url <- "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/locations.csv"

vaccines <- vroom::vroom(owid_url) %>%
  separate_rows(
    vaccines,
    sep = ","
  ) %>%
  mutate(
    vaccines = str_trim(vaccines)
  ) %>%
  group_by(vaccines) %>%
  summarise(
    Countries = paste(location, collapse = ", ")
  ) %>%
  rename(
    Vaccine = vaccines
  )

tab <- vaccines %>%
  gt() %>%
  tab_header(
    title = md("**Which countries are using each COVID-19 Vaccine**"),
    subtitle = "From OWID vaccine data (https://github.com/owid/covid-19-data/)"
  ) %>%
  tab_source_note(
    md("**`@jmcastagnetto, Jesus M. Castagnetto, 2021-03-07`**")
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "gray70"),
      cell_text(style = "italic", weight = "bold")
    ),
    locations = cells_column_labels(columns = c("Vaccine", "Countries"))
  )

gtsave(
  tab,
  filename = "covid19-vaccines-countries.html"
)

gtsave(
  tab,
  filename = "covid19-vaccines-countries.png"
)
