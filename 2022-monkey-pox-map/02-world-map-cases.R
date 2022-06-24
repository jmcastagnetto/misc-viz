library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

monkey_pox_cases <- readRDS("2022-monkey-pox-map/2022-06-24-monkey-pox-cases.rds")

count_by_country <- monkey_pox_cases %>%
  group_by(Country_ISO3) %>%
  tally(name = "n_cases")

world <- ne_countries(scale = "small", returnclass = "sf") %>%
  st_transform("+proj=moll")

world_cases <- world %>%
  left_join(
    count_by_country,
    by = c("iso_a3" = "Country_ISO3")
  )

p1 <- ggplot(world_cases) +
  geom_sf(aes(fill = n_cases)) +
  scale_fill_viridis_b(
    option = "plasma",
    direction = -1,
    breaks = seq(0, 800, by = 100),
    limits = c(0, 800),
    na.value = "grey90",
    show.limits = TRUE
  ) +
  labs(
    fill = "Cases",
    title = "Reported cases of \"Monkey Pox\"",
    subtitle = "Data Source: https://github.com/globaldothealth/monkeypox",
    caption = "@jmcastagnetto, Jesus M. Castagnetto (2022-06-24)"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = "white"),
    plot.margin = unit(rep(0.5, 4), "cm"),
    plot.title = element_text(size = 28, face = "bold"),
    plot.subtitle = element_text(size = 18, color = "grey50"),
    plot.caption = element_text(family = "Inconsolata", size = 14),
    legend.key.height = unit(1.5, "cm"),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14, face = "bold")
  )

ggsave(
  plot = p1,
  filename = glue::glue("2022-monkey-pox-map/{Sys.Date()}-monkey-pox-cases-map.png"),
  width = 12,
  height = 6
)
