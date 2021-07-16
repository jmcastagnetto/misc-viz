library(tidyverse)

prices <- read_csv("covid19-vaccines/20210716-unicef-dashboard-vaccines-prices.csv") %>%
  mutate(
     `Price per dose` = str_remove(`Price per dose`, fixed("$")) %>%
       as.numeric(),
     `Vaccine developer` = str_replace_all(
       `Vaccine developer`,
       c(
         "^Janssen$" = "Janssen Pharmaceuticals",
         "^Sinopharm \\(Beijing\\)$" = "Beijing Institute of Biological Products (CNBG)",
         "Pfizer BioNTech" = "Pfizer/BioNTech",
         "Serum Institute of India" = "AstraZeneca"
       )
     )
  )

write_csv(
  prices,
  "covid19-vaccines/20210716-unicef-dashboard-vaccines-prices-cleaned.csv"
)

avg_prices <- prices %>%
  group_by(`Vaccine developer`, Manufacturer, `Vaccine name`) %>%
  summarise(
    n = n(),
    avg_price = round(mean(`Price per dose`, na.rm = TRUE), 2),
    min_price = min(`Price per dose`, na.rm = TRUE),
    max_price = max(`Price per dose`, na.rm = TRUE)
  )

