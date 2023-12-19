library(tidyverse)
library(rvest)
library(ggbump)


rds_2019 <- "2019-2023_quality_living_cities/ranking_2019.rds"
rds_2023 <- "2019-2023_quality_living_cities/ranking_2023.rds"

if (!file.exists(rds_2019)) {

  mercer_2019 <- "https://mobilityexchange.mercer.com/Insights/quality-of-living-rankings"

  tbls_2019 <- read_html(mercer_2019) %>%
    html_table()

  ranking_2019 <- bind_rows(
    tbls_2019[[1]] %>%
      janitor::clean_names(),
    tbls_2019[[2]] %>%
      janitor::clean_names()
  ) %>%
    rename(
      rank_2019 = rank,
      location  = country_region
    ) %>%
    mutate(
      city = iconv(city, to = "ASCII//TRANSLIT")
    )
  saveRDS(ranking_2019, "2019-2023_quality_living_cities/ranking_2019.rds")
}

ranking_2019 <- readRDS(rds_2019)


if (!file.exists(rds_2023)) {

mercer_2023 <- "https://www.mercer.com/insights/total-rewards/talent-mobility-insights/quality-of-living-city-ranking/"

tbls_2023 <- read_html(mercer_2023) %>%
  html_table()

ranking_2023 <- tbls_2023[[1]] %>%
  janitor::clean_names() %>%
  mutate(
    city = str_to_title(city),
    location = str_to_title(location),
    ranking_2023 = str_remove(ranking_2023, fixed("*")) %>%
      as.integer()
  ) %>%
  rename(
    rank_2023 = ranking_2023
  )
  saveRDS(ranking_2023, rds_2023)
}

ranking_2023 <- readRDS(rds_2023)

rankings_2019_2023 <- ranking_2019 %>%
  full_join(
    ranking_2023,
    by = c("city", "location")
  )

sam_complete <- rankings_2019_2023 %>%
  na.omit() %>%
  filter(
    location %in% c(
      "Argentina",
      "Bolivia",
      "Brazil",
      "Chile",
      "Colombia",
      "Ecuador",
      "Paraguay",
      "Peru",
      "Uruguay",
      "Venezuela"
    )
  ) %>%
  pivot_longer(
    cols = c("rank_2019", "rank_2023"),
    names_to = "year",
    names_prefix = "rank_",
    values_to = "rank"
  ) %>%
  mutate(
    year = as.integer(year)
  )

qol_plot <- ggplot(
  sam_complete,
  aes(x = year, y = rank, color = city)
) +
  geom_point(size = 4) +
  ggrepel::geom_text_repel(
    data = sam_complete %>% filter(year == min(year)),
    aes(x = year - .1, label = city),
    size = 5,
    hjust = 1,
    nudge_x = -.5,
    direction = "y"
  ) +
  ggrepel::geom_text_repel(
    data = sam_complete %>% filter(year == max(year)),
    aes(x = year - .1, label = city),
    size = 5,
    hjust = 0,
    nudge_x = .5,
    direction = "y"
  ) +
  geom_bump(linewidth = 2) +
  scale_y_reverse(n.breaks = 10) +
  scale_x_continuous(
    breaks = c(2019, 2023),
    expand = expansion(add = .6)
  ) +
  theme_minimal(
    base_size = 16,
    base_family = "Atkinson Hyperlegible"
  ) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    plot.margin = unit(rep(1, 4), "cm"),
    plot.title.position = "plot",
    plot.title = element_text(size = 26, face = "bold"),
    plot.subtitle = element_text(color = "gray70", size = 18),
    plot.caption = element_text(family = "Inconsolata", size = 16),
    plot.background = element_rect(fill = "white", color = "white")
  ) +
  labs(
    y = "Ranking",
    x = "Year",
    title = "Change in 'Quality of Living Ranking'\nfor cities in South America",
    subtitle = "Data source: Mercer LLC (https://www.mercer.com/)",
    caption = "@jmcastagnetto@mastodon.social, Jesus M. Castagnetto (2023)"
  )

ggsave(
  plot = qol_plot,
  filename = "2019-2023_quality_living_cities/quality-living-southamerican-cities.png",
  height = 14,
  width = 11
)

