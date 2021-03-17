library(tidyverse)
library(ggbump)

if(!file.exists("data/covid-19_cases_raw.rds")) {
  rds_url <- "https://github.com/jmcastagnetto/covid-19-data-cleanup/blob/main/data/covid-19_cases_raw.rds?raw=true"
  download.file(
    rds_url,
    "data/covid-19_cases_raw.rds"
  )
}

if(!file.exists("data/wb_popluation.Rdata")) {
  rdata_url <- "https://github.com/jmcastagnetto/covid-19-data-cleanup/blob/main/data/wb_population.Rdata?raw=true"
  download.file(
    rdata_url,
    "data/wb_popluation.Rdata"
  )
}

load("data/wb_popluation.Rdata")

df <- readRDS("data/covid-19_cases_raw.rds") %>%
  # consider only the most recent data
  filter(data_update == max(data_update)) %>%
  # acumulate by country
  group_by(data_update,
           continent, country_region, iso3c,
           who_region,
           world_bank_income_group) %>%
  summarise(
    confirmed = sum(confirmed, na.rm = TRUE),
    dead = sum(dead, na.rm = TRUE),
    recovered = sum(dead, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  # add World Bank 2020 population estimates
  left_join(
    wb_pop %>% select(country_code, population_2020),
    by = c("iso3c" = "country_code")
  ) %>%
  # rankings
  mutate(
    rank_confirmed = row_number(desc(confirmed))
  )

prepare_df <- function(df) {
  df %>%
    mutate(
      rank_confirmed = row_number(desc(confirmed))
    ) %>%
    filter(rank_confirmed <= 10) %>%
    arrange(rank_confirmed) %>%
    select(
      data_update,
      country_region,
      population_2020,
      confirmed,
      dead,
      recovered
    ) %>%
    pivot_longer(
      cols = c(population_2020, confirmed, dead, recovered),
      names_to = "metric",
      names_prefix = "rank_"
    ) %>%
    group_by(data_update, metric) %>%
    mutate(
      rank = row_number(desc(value)),
      map_val = case_when(
        metric == "confirmed" ~ 1,
        metric == "dead" ~ 2,
        metric == "recovered" ~ 3,
        metric == "population_2020" ~ 4
      ),
      metric = factor(
        metric,
        levels = c("confirmed", "dead", "recovered", "population_2020"),
        labels = c("Confirmed", "Dead", "Recovered", "Population"),
        ordered = TRUE
      )
    )
}

mk_bumpchart <- function(df, title = "Worldwide") {
  n_ranks = length(unique(df$rank))
  last_date <- max(df$data_update)
  ggplot(
    df,
    aes(x = map_val, y = rank,
        group = country_region,
        color = country_region)
  ) +
    #  geom_vline(xintercept = 1:4, linetype = "dashed", color = "grey") +
    geom_point(size = 8) +
    geom_text(data = df %>% filter(map_val == 1),
              aes(x = map_val - .1, label = country_region),
              size = 7, hjust = 1) +
    geom_text(data = df %>% filter(map_val == 4),
              aes(x = map_val + .1, label = country_region),
              size = 7, hjust = 0) +
    geom_bump(size = 2) +
    scale_y_reverse(
      breaks = 1:10,
      labels = LETTERS[2:11] # trick to use numberpile font
    ) +
    scale_x_continuous(
      breaks = 1:4,
      labels = c("Confirmed", "Dead", "Recovered", "Population"),
      expand = c(.15, .15),
      sec.axis = dup_axis()
    ) +
    coord_cartesian(expand = TRUE) +
    labs(
      x = "",
      y = "Rank",
      title = glue::glue("{title}: top {n_ranks} countries by confirmed COVID-19 cases"),
      subtitle = glue::glue("Source: JHU COVID-19 ({last_date}) and World Bank 2020 Population Data"),
      caption = glue::glue("@jmcastagnetto, Jesus M. Castagnetto, {Sys.Date()}")
    ) +
    theme(
      plot.title = element_text(size = 32),
      plot.subtitle = element_text(size = 22, color = "brown"),
      plot.caption = element_text(family = "Inconsolata", size = 20),
      plot.title.position = "plot",
      plot.margin = unit(rep(1, 4), "cm"),
      axis.title.y = element_text(
        size = 20, vjust = 1,
        angle = 0, color = "grey50"),
      axis.text.x = element_text(size = 20, color = "grey50", face = "bold"),
      axis.ticks.x = element_blank(),
      axis.text.y = element_text(size = 20, color = "grey50",
                                 family = "Numberpile Reversed"),
      axis.ticks.y = element_blank(),
      legend.position = "none"
    )
}


world_top10_cases <- prepare_df(df)
bp <- mk_bumpchart(world_top10_cases)
#bp
ggsave(
  bp,
  filename = glue::glue("plots/{Sys.Date()}-top10-ranking-countries-covid19.png"),
  width = 18,
  height = 11
)

# Americas ----------------------------------------------------------------

americas <- prepare_df(
  df %>% filter(continent == "Americas")
)
bp_americas <- mk_bumpchart(americas, "The Americas")
#bp_americas
ggsave(
  bp_americas,
  filename = glue::glue("plots/{Sys.Date()}-americas-top10-ranking-countries-covid19.png"),
  width = 18,
  height = 11
)


# Europe ------------------------------------------------------------------

europe <- prepare_df(
  df %>% filter(continent == "Europe")
)
bp_europe <- mk_bumpchart(europe, "Europe")
#bp_europe
ggsave(
  bp_europe,
  filename = glue::glue("plots/{Sys.Date()}-europe-top10-ranking-countries-covid19.png"),
  width = 18,
  height = 11
)


# Asia --------------------------------------------------------------------

asia <- prepare_df(
  df %>% filter(continent == "Asia")
)
bp_asia <- mk_bumpchart(asia, "Asia")
#bp_asia
ggsave(
  bp_asia,
  filename = glue::glue("plots/{Sys.Date()}-asia-top10-ranking-countries-covid19.png"),
  width = 18,
  height = 11
)


# Africa ------------------------------------------------------------------

africa <- prepare_df(
  df %>% filter(continent == "Africa")
)
bp_africa <- mk_bumpchart(africa, "Africa")
#bp_africa
ggsave(
  bp_africa,
  filename = glue::glue("plots/{Sys.Date()}-africa-top10-ranking-countries-covid19.png"),
  width = 18,
  height = 11
)

# Oceania -----------------------------------------------------------------
oceania <- prepare_df(
  df %>% filter(continent == "Oceania")
)
bp_oceania <- mk_bumpchart(oceania, "Oceania")
#bp_oceania
ggsave(
  bp_oceania,
  filename = glue::glue("plots/{Sys.Date()}-oceania-ranking-countries-covid19.png"),
  width = 19,
  height = 11
)




