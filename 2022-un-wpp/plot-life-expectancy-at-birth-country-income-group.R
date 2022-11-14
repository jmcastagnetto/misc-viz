library(tidyverse)
library(readxl)

un_wpp_raw <- read_excel(
  "2022-un-wpp/WPP2022_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT_REV1.xlsx",
  range = "A17:BM20613",
  sheet = 1,
  col_types = c("text")
) %>% janitor::clean_names() %>%
  mutate(
    across(11:65, as.numeric)
  )

income_grps <- c(
  "High-income countries",
  "Upper-middle-income countries",
  "Middle-income countries",
  "Lower-middle-income countries",
  "Low-income countries"
)

byincome_life_expect_yr <- un_wpp_raw %>%
  filter(region_subregion_country_or_area %in% income_grps |
           region_subregion_country_or_area == "WORLD") %>%
  filter(year > 1970) %>%
  select(
    region = region_subregion_country_or_area,
    year,
    leab = life_expectancy_at_birth_both_sexes_years
  ) %>%
  mutate(
    region = str_to_sentence(region)
  )

latest_df <- byincome_life_expect_yr %>%
  filter(year == max(year))

plot_byincome_life_expect_yr <- ggplot(
  byincome_life_expect_yr,
  aes(x = year, y = leab,
      group = region,
      color = region)
) +
  geom_rect(
    xmin = 2019, xmax = 2021,
    ymin = 40, ymax = 90,
    fill = "grey85",
    color = "grey85",
    alpha = 0.6
  ) +
  geom_line(aes(linetype = region), linewidth = 1) +
  geom_point(data = latest_df, size = 4) +
  ggrepel::geom_text_repel(
    data = latest_df, aes(label = region),
    size = 6,
    fontface = "bold",
    hjust = 0, nudge_x = 2,
    arrow = arrow(length = unit(2, "mm"), type = "closed")
  ) +
  scale_x_continuous(
    n.breaks = 7,
    expand = expansion(0, c(0, 20))
  ) +
  annotate(
    geom = "text",
    x = 2022,
    y = 50,
    label = "The effect of the\nCOVID-19 pandemic",
    size = 9,
    hjust = 0
  ) +
  annotate(
    geom = "curve",
    curvature = -0.5,
    x = 2027,
    xend = 2022,
    y = 47,
    yend = 43,
    arrow = arrow(type = "closed"),
    linewidth = 1
  ) +
  labs(
    x = "",
    y = "Life expentancy at birth (both sexes, in years)",
    title = "Change in life expectancy at birth in countries by income group",
    subtitle = "Data Source: World Population Prospects 2022 (UN): https://population.un.org/wpp/",
    caption = "@jmcastagnetto@mastodon.social, Jesus M. Castagnetto (2022)"
  ) +
  theme_classic(base_size = 18, base_family = "Roboto") +
  theme(
    plot.margin = unit(rep(1, 4), "cm"),
    plot.title = element_text(size = 26, face = "bold"),
    plot.subtitle = element_text(color = "gray50", size = 20),
    plot.caption = element_text(family = "Inconsolata", size = 16),
    legend.position = "none"
  )

ggsave(
  plot = plot_byincome_life_expect_yr,
  filename = "2022-un-wpp/life-expentancy-at-birth-by-income-group.png",
  width = 18,
  height = 10
)
