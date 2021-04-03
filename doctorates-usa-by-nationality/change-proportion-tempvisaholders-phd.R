# Data source: https://ncsesdata.nsf.gov/home

library(tidyverse)
library(hrbrthemes)
library(ggfx)

import_goldman_sans()

phd <- read_csv(
  "data/SED_export_table_2021-04-03T20 08 55.911Z.csv",
  skip = 9,
  n_max = 3,
  col_names = c(
    "citizenship",
    as.character(seq(2019, 1980, -1)),
    "dummy"
  )
) %>%
  select(-dummy) %>%
  pivot_longer(
    cols = 2:41,
    names_to = "year",
    values_to = "graduated"
  ) %>%
  mutate(
    year = as.integer(year),
    citizenship = factor(
      citizenship,
      levels = c(
        "Temporary visa holder",
        "Unknown citizenship",
        "U.S. citizen or permanent resident"
      ),
      order = TRUE
    )
  ) %>%
  arrange(year, citizenship)

pct <- phd %>%
  group_by(year) %>%
  mutate(
    pct = graduated / sum(graduated)
  )

y_1980 = sum((pct %>% filter(year == 1980))$pct[2:3])
temp_1980 = round(100*(pct %>% filter(year == 1980))$pct[1], 1)
y_2000 = sum((pct %>% filter(year == 2000))$pct[2:3])
temp_2000 = round(100*(pct %>% filter(year == 2000))$pct[1], 1)
y_2019 = sum((pct %>% filter(year == 2019))$pct[2:3])
temp_2019 = round(100*(pct %>% filter(year == 2019))$pct[1], 1)


p1 <- ggplot() +
  geom_area(
    data = phd,
    mapping = aes(x = year, y = graduated, fill = citizenship),
    position = "fill"
  ) +
  with_outer_glow(
    annotate(
      geom = "segment",
      x = 1981,
      y = .6,
      xend = 1980,
      yend = y_1980,
      color = "white"
    )
  ) +
  with_outer_glow(
    annotate(
      geom = "text",
      x = 1981,
      y = .595,
      hjust = 0,
      vjust = 1,
      label = glue::glue("Temporary visa holders\nwere about {temp_1980}% of Ph.D.\ngraduates in 1980."),
      size = 8,
      color = "white"
    )
  ) +
  with_outer_glow(
    annotate(
      geom = "segment",
      x = 2000,
      y = .5,
      xend = 2000,
      yend = y_2000,
      color = "white"
    )
  ) +
  with_outer_glow(
    annotate(
      geom = "text",
      x = 2000,
      y = .495,
      hjust = 0.5,
      vjust = 1,
      label = glue::glue("By 2000, they were {temp_2000}%\nof Ph.D. graduates"),
      size = 8,
      color = "white"
    )
  ) +
  with_outer_glow(
    annotate(
      geom = "segment",
      x = 2018,
      y = .30,
      xend = 2019,
      yend = y_2019,
      color = "white"
    )
  ) +
  with_outer_glow(
    annotate(
      geom = "text",
      x = 2018,
      y = .295,
      hjust = 1,
      vjust = 1,
      label = glue::glue("And in 2019 they comprised\n{temp_2019}% of Ph.D. graduates"),
      size = 8,
      color = "white"
    )
  ) +
  with_outer_glow(
    geom_point(
      data = data.frame(
        x = c(1980, 2000, 2019),
        y = c(y_1980, y_2000, y_2019)
      ),
      mapping = aes(x = x, y = y),
      shape = 10,
      size = 9,
      color = "black"
    )
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Dark2", type = "qual") +
  theme_ipsum_gs(
    base_size = 16,
    plot_title_size = 26,
    caption_family = "Inconsolata",
    caption_size = 16
  ) +
  theme(
    plot.title.position = "plot",
    plot.subtitle = element_text(size = 20, color = "grey50"),
    legend.position = "top",
    legend.text = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16)
  ) +
  labs(
    title = "Increase in proportion of temporary visa holders among USA Ph.D. graduates",
    subtitle = "SOURCE: National Center for Science and Engineering Statistics, Survey of Earned Doctorates.",
    caption = "@jmcastagnetto, Jesus M. Castagnetto, 2021-04-03",
    x = "",
    y = "",
    fill = ""
  )

ggsave(
  plot = p1,
  filename = "doctorates-usa-by-nationality/proportion_tempvisaholders_phd_usa.png",
  width = 17,
  height = 10
)
