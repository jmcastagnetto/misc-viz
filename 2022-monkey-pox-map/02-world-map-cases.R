library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

monkeypox_cases <- readRDS("2022-monkey-pox-map/2022-07-19-monkey-pox-cases.rds") %>%
  filter(Status == "confirmed")
last_mod <- max(monkeypox_cases$Date_last_modified)

count_by_country <- monkeypox_cases %>%
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
    breaks = seq(0, 3000, by = 250),
    limits = c(0, 3000),
    na.value = "white",
    show.limits = TRUE
  ) +
  labs(
    fill = "Cases",
    title = "Confirmed cases of \"Monkeypox\" around the world",
    subtitle = glue::glue("Data Source: https://github.com/globaldothealth/monkeypox,  Last modified: {last_mod}"),
    caption = glue::glue("@jmcastagnetto, Jesus M. Castagnetto ({Sys.Date()})")
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = "white"),
    plot.margin = unit(rep(0.5, 4), "cm"),
    plot.title = element_text(size = 22, face = "bold"),
    plot.subtitle = element_text(size = 14, color = "grey50"),
    plot.caption = element_text(family = "Inconsolata", size = 10),
    legend.key.height = unit(1.5, "cm"),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12, face = "bold")
  )

ggsave(
  plot = p1,
  filename = glue::glue("2022-monkey-pox-map/{Sys.Date()}-monkey-pox-cases-map.png"),
  width = 10,
  height = 6
)
