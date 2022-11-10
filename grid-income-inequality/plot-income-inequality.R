library(tidyverse)

grid_df <- read_csv(
  "grid-income-inequality/Stats_20221109181556.csv",
  col_types = cols(
    country = col_character(),
    year = col_integer(),
    gender = col_character(),
    age = col_character(),
    share_bottom50_inc = col_double(),
    share_top10_pct_inc = col_double()
  )
) %>%
  select(
    country,
    year,
    share_bottom50_inc,
    share_top10_pct_inc
  ) %>%
  pivot_longer(
    cols = c(share_bottom50_inc, share_top10_pct_inc),
    names_to = "segment",
    names_prefix = "share_",
    values_to = "share_of_income"
  ) %>%
  mutate(
    segment = str_replace_all(
      segment,
      c(
        "bottom50_inc" = "Bottom 50%",
        "top10_pct_inc" = "Top 10%"
      )
    ),
    segment = factor(
      segment,
      levels = c("Top 10%", "Bottom 50%"),
      ordered = TRUE
    )
  )

ggplot(
  grid_df,
  aes(x = year, y = share_of_income/100, group = segment, color = segment)
) +
  geom_line(linewidth = 2) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_brewer(palette = "Accent") +
  facet_wrap(~country) +
  labs(
    x = "",
    y = "Share of country's income",
    title = "Income inequality in some countries of America over the years",
    subtitle = "The gaps in the share of the country's income are extreme in Brazil, USA and Mexico, compared to Argentina (which has decreased) and Canada (the lowest)",
    caption = "Data source: https://www.grid-database.org/\n@jmcastagnetto@mastodon.social, Jesus M. Castagnetto (2022)",
    color = "Segment"
  ) +
  theme_linedraw(base_size = 18, base_family = "Roboto") +
  theme(
    plot.margin = unit(rep(.5, 4), "cm"),
    plot.title = element_text(size = 26, face = "bold"),
    plot.title.position = "plot",
    plot.subtitle = element_text(color = "gray40", size = 16),
    plot.caption = element_text(family = "Incosolata", size = 14),
    legend.position = c(.8, .2),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    strip.text = element_text(size = 16, face = "bold.italic"),
    axis.title.y = element_text(size = 18, hjust = 1)
  )

ggsave(
  filename = "grid-income-inequality/income-inequality-arg-bra-can-mex-usa.png",
  width = 16,
  height = 9
)
