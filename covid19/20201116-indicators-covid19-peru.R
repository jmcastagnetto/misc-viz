library(tidyverse)
library(latex2exp)
library(patchwork)

# Mandatory lockdown: 2020-03-16 - 2020-06-30
ini_lkdn <- lubridate::ymd("2020-03-16")
end_lkdn <- lubridate::ymd("2020-06-30")

peru <- read_csv("data/covid-19-peru-data-augmented.csv.xz") %>%
  filter(is.na(region)) %>%
  select(
    country,
    iso3c,
    date,
    confirmed,
    deaths,
    recovered
  ) %>%
  mutate(
    confirmed = replace_na(confirmed, 0),
    deaths = replace_na(deaths, 0),
    recovered = replace_na(recovered, 0),
    epiweek = lubridate::epiweek(date),
    wed_of_wk = lubridate::floor_date(date, unit = "week") + 3 # Wed of each week
  ) %>%
  group_by(epiweek, wed_of_wk) %>%
  summarise(
    confirmed = max(confirmed),
    deaths = max(deaths),
    recovered = max(recovered)
  ) %>%
  ungroup() %>%
  mutate(
    new_confirmed = confirmed - lag(confirmed),
    new_deaths = deaths - lag(deaths),
    new_recovered = recovered - lag(recovered)
  )

# epiweek range
first_week <- 14
last_week <- max(peru$epiweek)

# Positive cases and recovered --------------------------------------------

peru_epiweek1 <- peru %>%
  filter(epiweek >= first_week & epiweek < last_week) %>%
  rename(
    Positive = new_confirmed,
    Recovered = new_recovered
  ) %>%
  select(epiweek, wed_of_wk, Positive, Recovered) %>%
  pivot_longer(
    cols = c(Positive, Recovered),
    names_to = "item",
    values_to = "count"
  )

p1 <- ggplot(
  peru_epiweek1 %>% filter(epiweek >= 14),
  aes(x = wed_of_wk, y = count, color = item)
) +
  geom_line(size = 1) +
  annotate(
    geom = "rect",
    xmin = ini_lkdn,
    xmax = end_lkdn,
    ymin = 0,
    ymax = 60000,
    color = "gray",
    alpha = .3
  ) +
  annotate(
    geom = "text",
    x = ini_lkdn + (end_lkdn - ini_lkdn) / 2,
    y = .9 * 60000,
    label = "Mandatory\nlockdown",
    family = "Inconsolata",
    size = 6,
    color = "yellow"
  ) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(
    date_breaks = "8 weeks",
    date_labels = "Wk: %V\n%b %d",
    date_minor_breaks = "1 week"
  ) +
  labs(
    x = "",
    y = "",
    color = "",
    subtitle = "Weekly Positive (PCR + Serological) and Recovered cases"
  ) +
  scale_color_brewer(type = "qual", palette = "Paired") +
  ggdark::dark_theme_bw(18) +
  theme(
    plot.title.position = "plot",
    legend.background = element_blank(),
    legend.position = c(.7, .2),
    legend.text = element_text(size = 18),
    legend.key.width = unit(2, "cm"),
    plot.margin = unit(c(1,2,1,1), "cm")
  )
# p1


# CFR estimates -----------------------------------------------------------

peru_epiweek2 <- peru %>%
  filter(epiweek >= first_week & epiweek < last_week) %>%
  mutate(
    total = new_recovered + new_deaths,
    cfr = new_deaths / total
  ) %>%
  ungroup()

dt_eq <- "$CFR = \\left( \\frac{Deaths}{Deaths + Recovered} \\right)$"

p2 <- ggplot(
  peru_epiweek2,
  aes(x = wed_of_wk, y = cfr)
) +
  geom_line(size = 1, color = "red") +
  # geom_point(size = 3, shape = "circle", color = "white") +
  # geom_point(size = 5, shape = "✝", color = "black") +
  annotate(
    geom = "rect",
    xmin = ini_lkdn,
    xmax = end_lkdn,
    ymin = 0,
    ymax = .3,
    color = "gray",
    alpha = .3
  ) +
  annotate(
    geom = "text",
    x = ini_lkdn + (end_lkdn - ini_lkdn) / 2,
    y = .9 * .3,
    label = "Mandatory\nlockdown",
    family = "Inconsolata",
    size = 6,
    color = "yellow"
  ) +
  annotate(geom = "text", x = as.Date("2020-07-15"),
           size = 4, hjust = 0,
           y = .2, label = TeX(dt_eq)) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_date(
    date_breaks = "8 weeks",
    date_labels = "Wk: %V\n%b %d",
    date_minor_breaks = "1 week"
  ) +
  labs(
    x = "",
    y = "",
    subtitle = "Weekly CFR estimates"
  ) +
  ggdark::dark_theme_bw(18) +
  theme(
    plot.title.position = "plot",
  )
# p2


# Compose plots -----------------------------------------------------------

comb_caption <-  "CFR definition from: https://www.who.int/news-room/commentaries/detail/estimating-mortality-from-covid-19\nData source: https://github.com/jmcastagnetto/covid-19-peru-data/ (commit: 3e95f31)\n@jmcastagnetto, Jesus M. Castagnetto, 2020-11-16"

p12 <- p1 + p2 +
  plot_annotation(
    title = "Perú: Evolution of indicators during the COVID-19 pandemic",
    subtitle = "Range: From epiweek 14 to 46, 2020",
    caption = comb_caption,
    theme = ggdark::dark_theme_bw(18) +
      theme(
        plot.caption = element_text(family = "Inconsolata"),
        plot.margin = unit(c(1,2,1,1), "cm")
      )
  )
# p12
ggsave(
  plot = p12,
  filename = "plots/20201116-indicators-covid19-peru.png",
  dpi = 300,
  width = 15,
  height = 9
)
