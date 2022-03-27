library(tidyverse)
# get estimated R since Sep 2020
est_r <- readRDS("estimated-r-pone-0244474/data/est_r.rds")
  # %>% filter(date >= "2020-09-01")
today <- Sys.Date()

plots_df <- est_r %>%
  # remove entries w/o ISO 3-letter code, eg: "Summer Olympics 2020"
  filter(!is.na(iso3c)) %>%
  mutate(
    lbls = glue::glue("Days Infectious: {days_infectious}"),
    lbls = fct_reorder(lbls, days_infectious)
  ) %>%
  group_by(country_region) %>%
  mutate(
    min_date = min(date),
    max_date = max(date),
    filename = glue::glue("{str_to_lower(iso3c)}_estimated-r_{min_date}_{max_date}.png")
  ) %>%
  ungroup() %>%
  group_nest(country_region, iso3c, filename) %>%
  mutate(
    plot = map2(
      .y = country_region, .x = data,
      ~{
        min_date = min(.x$date)
        max_date = max(.x$date)
        ggplot(
          data = .x
        ) +
          geom_ribbon(
            aes(x = date, ymin = ci_95_l, ymax = ci_95_u),
            fill = "gray60",
            alpha = .5
          ) +
          geom_hline(yintercept = 1, color = "blue", linetype = "dashed") +
          geom_line(
            aes(x = date, y = r), color = "black", size = 1
          ) +
          facet_wrap(~lbls) +
          scale_x_date(
            date_breaks = "3 months",
            date_labels = "%b\n%Y"
          ) +
          ggthemes::theme_few(base_family = "Roboto") +
          theme(
            strip.text = element_text(face = "bold", size = 18),
            panel.spacing = unit(2, "lines"),
            axis.text = element_text(size = 14),
            plot.title.position = "plot",
            plot.title = element_text(family = "Ubuntu", size = 34),
            plot.subtitle = element_text(color = "grey50", size = 24),
            plot.caption = element_text(family = "Inconsolata", size = 16),
            plot.margin = unit(rep(1, 4), "cm")
          ) +
          labs(
            x = "",
            y = "",
            title = glue::glue("{.y}:\nEstimated R for COVID-19 under different scenarios"),
            subtitle = glue::glue("From {min_date} to {max_date} (Ref: https://doi.org/10.1371/journal.pone.0244474)"),
            caption = glue::glue("Data source: https://github.com/crondonm/TrackingR/tree/main/Estimates-Database\n@jmcastagnetto, Jesus M. Castagnetto ({today})")
          )
      }
    )
  ) %>%
  select(plot, filename)

pwalk(
  plots_df,
  ggsave,
  path = "estimated-r-pone-0244474/plots/",
  width = 16,
  height = 9
)
