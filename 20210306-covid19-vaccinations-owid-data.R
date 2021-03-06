library(tidyverse)

# data from owid
vaccinations <- vroom::vroom("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv")

last_vaccination_data <- vaccinations %>%
  group_by(location, iso_code) %>%
  summarise(
    initial_date = min(date, na.rm = TRUE),
    last_date = max(date, na.rm = TRUE)
  ) %>%
  mutate(
    days = lubridate::time_length(last_date - initial_date, "day")
  ) %>%
  ungroup() %>%
  left_join(
    vaccinations %>%
      select(c(1:3, 9)),
    by = c("location", "iso_code", "initial_date" = "date")
  ) %>%
  rename(
    initial_total_per_100 = total_vaccinations_per_hundred
  ) %>%
  left_join(
    vaccinations %>%
      select(c(1:3, 9)),
    by = c("location", "iso_code", "last_date" = "date")
  ) %>%
  rename(
    last_total_per_100 = total_vaccinations_per_hundred
  ) %>%
  mutate(
    continent = countrycode::countryname(location, destination = "continent"),
    continent = if_else(
      location %in% c("Wales", "Scotland", "Northern Ireland"),
      "Europe",
      continent
    ),
    region = countrycode::countryname(location, destination = "region23"),
    region = case_when(
      location %in% c("Wales", "Scotland", "Northern Ireland") ~ "Northern Europe",
      TRUE ~ region
    )
  ) %>%
  filter(!is.na(continent)) %>%
  mutate(
    location2 = tidytext::reorder_within(location, initial_date, continent) %>%
      fct_rev()
  ) %>%
  mutate(
    initial_total_per_100 = replace_na(initial_total_per_100, 0),
    initial_rank = min_rank(initial_total_per_100),
    last_rank = min_rank(last_total_per_100)
  )

p1 <- ggplot(
  last_vaccination_data,
  aes(y = location2, x = initial_date, xend = last_date)
) +
  ggalt::geom_dumbbell(size_x = 5, size_xend = 5, size = 2, dot_guide = TRUE) +
  tidytext::scale_y_reordered() +
  facet_wrap(~continent, scales = "free_y", ncol = 5) +
  labs(
    x = "",
    y = "",
    title = "Comparing COVID-19 vaccination campaing durations as of 2021-03-06",
    subtitle = "Source: https://ourworldindata.org/covid-vaccinations",
    caption = "@jmcastagnetto, Jesus M. Castagnetto, 2021-03-06"
  ) +
  ggthemes::theme_few(24) +
  theme(
    plot.caption = element_text(family = "Inconsolata"),
    axis.text.y.left = element_text(size = 12)
  )

ggsave(
  plot = p1,
  filename = "plots/20210306-covid19-vaccination-periods.png",
  width = 30,
  height = 14
)

log1p10 <- function(x) {
  log1p(x) / log(10)
}

invlog1p10 <- function(x) {
  log(10) * exp(x)
}

scales::trans_new(
  "log1p10_trans",
  "log1p10",
  "invlog1p10"
)


df1 <- last_vaccination_data %>%
  select(
    continent,
    region,
    location,
    initial_date,
    last_date
  ) %>%
  rename(
    initial = initial_date,
    last = last_date
  ) %>%
  pivot_longer(
    cols = c(initial, last),
    names_to = "period",
    values_to = "date"
  )


df2 <- last_vaccination_data %>%
  select(
    continent,
    region,
    location,
    initial_total_per_100,
    last_total_per_100
  ) %>%
  rename(
    initial = initial_total_per_100,
    last = last_total_per_100
  ) %>%
  pivot_longer(
    cols = c(initial, last),
    names_to = "period",
    values_to = "total_per_100"
  )

slope_df <- df1 %>%
  left_join(
    df2,
    by = c("continent", "region", "location", "period")
  ) %>%
  mutate(
    label = if_else(
      period == "initial",
      glue::glue("{period}:({date}, {total_per_100}) {location}"),
      glue::glue("{period}:{location} ({date}, {total_per_100})")
    )
  )

mk_slope_plot <- function(df) {
  ggplot(
    df,
    aes(x = period, y = total_per_100, group = location)
  ) +
    geom_point(aes(color = location), show.legend = FALSE, size = 3) +
    geom_line(aes(color = location), show.legend = FALSE, size = 2, alpha = .6) +
    scale_y_continuous(trans = scales::pseudo_log_trans(base = 10)) +
    ggrepel::geom_label_repel(
      data = df %>% filter(period == "last"),
      aes(x = period, y = total_per_100,
          label = str_remove(label, "last:"), color = location),
      max.overlaps = 100,
      label.size = 0,
      hjust = 0,
      point.padding = .5,
      nudge_x = 0.15,
      arrow = arrow(length = unit(.2, "cm"), type = "closed"),
      direction = "y",
      show.legend = FALSE,
      size = 5,
      max.time = 30,
      seed = 13579
    ) +
    ggrepel::geom_label_repel(
      data = df %>% filter(period == "initial"),
      aes(x = period, y = total_per_100,
          label = str_remove(label, "initial:"), color = location),
      max.overlaps = 50,
      label.size = 0,
      hjust = 1,
      point.padding = .5,
      nudge_x = -0.15,
      arrow = arrow(length = unit(.2, "cm"), type = "closed"),
      direction = "y",
      show.legend = FALSE,
      size = 5,
      max.time = 30,
      seed = 13579
    ) +
    ggthemes::theme_tufte(base_family = "Roboto", base_size = 38) +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      plot.caption = element_text(family = "Inconsolata")
    ) +
    facet_wrap(~region) +
    labs(
      title = glue::glue("COVID-19: Changes in total vaccinated by 100 habitants for {unique(df$continent)}"),
      subtitle = "by World Bank subregion and country, from first to latest recorded vaccination data",
      caption = "Source: https://ourworldindata.org/covid-vaccinations (downloaded: 2021-03-06)\n@jmcastagnetto, Jesus M. Castagnetto, 2021-03-06"
    )
}

plot_americas <- mk_slope_plot(slope_df %>% filter(continent == "Americas"))
ggsave(
  plot = plot_americas,
  filename = "plots/20210306-total-vaccinated-per100-americas.png",
  width = 24,
  height = 16
)

plot_europe <- mk_slope_plot(slope_df %>% filter(continent == "Europe"))
ggsave(
  plot = plot_europe,
  filename = "plots/20210306-total-vaccinated-per100-europe.png",
  width = 24,
  height = 24
)

plot_asia <- mk_slope_plot(slope_df %>% filter(continent == "Asia"))
ggsave(
  plot = plot_asia,
  filename = "plots/20210306-total-vaccinated-per100-asia.png",
  width = 28,
  height = 20
)

plot_africa <- mk_slope_plot(slope_df %>% filter(continent == "Africa"))
ggsave(
  plot = plot_africa,
  filename = "plots/20210306-total-vaccinated-per100-africa.png",
  width = 24,
  height = 16
)

plot_oceania <- mk_slope_plot(slope_df %>% filter(continent == "Oceania"))
ggsave(
  plot = plot_oceania,
  filename = "plots/20210306-total-vaccinated-per100-oceania.png",
  width = 24,
  height = 26
)
