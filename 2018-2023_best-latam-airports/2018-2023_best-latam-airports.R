# Sources:
# https://www.worldairportawards.com/worlds-top-100-airports-2023/
# https://www.worldairportawards.com/worlds-top-100-airports-2022/
# https://www.worldairportawards.com/worlds-top-100-airports-2021/
# https://www.worldairportawards.com/worlds-top-100-airports-2020/
# https://www.worldairportawards.com/worlds-top-100-airports-2019/
# https://www.worldairportawards.com/worlds-top-100-airports-2018/

library(tidyverse)
library(ggbump)
library(ggrepel)

airports_latam <- tribble(
  ~airport, ~year, ~rank,
  "Bogotá", 2018, 46,
  "Bogotá", 2019, 53,
  "Bogotá", 2020, 53,
  "Bogotá", 2021, 43,
  "Bogotá", 2022, 35,
  "Bogotá", 2023, 37,
  "Quito", 2018, 47,
  "Quito", 2019, 49,
  "Quito", 2020, 46,
  "Quito", 2021, 40,
  "Quito", 2022, 39,
  "Quito", 2023, 40,
  "Lima", 2018, 49,
  "Lima", 2019, 47,
  "Lima", 2020, 44,
  "Lima", 2021, 62,
  "Lima", 2022, 73,
  "Lima", 2023, 89
)

latam_airports_plot <- ggplot(
  airports_latam,
  aes(x = year, y = rank, color = airport)
) +
  geom_bump(linewidth = 3) +
  geom_point(size = 6) +
  geom_label_repel(
    aes(label = rank),
    size = 7,
    seed = 13579
  ) +
  geom_text_repel(
    data = airports_latam %>% filter(year == min(year)),
    aes(x = year - .2, label = airport),
    size = 9,
    hjust = 1,
    direction = "y",
    seed = 13579
  ) +
  geom_text_repel(
    data = airports_latam %>% filter(year == max(year)),
    aes(x = year + .2, label = airport),
    size = 9,
    hjust = 0,
    direction = "y",
    seed = 13579
  ) +
  scale_y_reverse() +
  scale_x_continuous(breaks = 2018:2023, expand = expansion(add = .5)) +
  scale_color_brewer(type = "qual", palette = "Dark2") +
  labs(
    x = "",
    y = "",
    title = "Airports in Latin America in the \"World's Top 100 airports\"",
    subtitle = "Data source: SKYTRAX World Airport Awards 2018-2023",
    caption = "@jmcastagnetto@mastodon.social, Jesus M. Castagnetto"
  ) +
  theme_minimal(base_size = 24) +
  theme(
    plot.title = element_text(size = 32),
    plot.subtitle = element_text(color = "grey50"),
    plot.caption = element_text(family = "Incosolata"),
    plot.background = element_rect(color = "white", fill = "white"),
    legend.position = "none",
    axis.text.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "grey80")
  )

ggsave(
  plot = latam_airports_plot,
  filename = "2018-2023_best-latam-airports/2018-2023_best-latam-airports.png",
  width = 16,
  height = 16
)
