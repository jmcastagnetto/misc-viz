library(tidyverse)

sam_df <- readRDS("estimated-r-pone-0244474/data/est_r.rds") %>%
  filter(date >= "2021-01-01") %>%
  mutate(
    region23 = countrycode::countryname(country_region, destination = "region23")
  ) %>%
  filter(
    region23 == "South America"
  ) %>%
  arrange(country_region, date) %>%
  group_by(country_region, days_infectious) %>%
  mutate(
    last_r = last(r),
    linetype = if_else(last_r < 1, "below", "above")
  )

today <- Sys.Date()
min_date <- min(sam_df$date)
max_date <- max(sam_df$date)
Sys.setlocale("LC_TIME", "es_PE.utf8")

psam <- ggplot(
  sam_df %>% filter(days_infectious == 10),
) +
  aes(x = date, y = r, color = country_region) +
  geom_hline(yintercept = 1, color = "grey70") +
  geom_line(aes(linetype = linetype)) +
  ggrepel::geom_label_repel(
    data = . %>%
      group_by(country_region) %>%
      summarise(
        date = first(date),
        r = first(r),
        lbl = paste0(unique(country_region), ": ", round(r, 2))
      ),
    aes(label = lbl),
    label.size = 0,
    direction = "y",
    nudge_x = -10,
    size = 6,
    seed = 123,
    arrow = arrow(type = "closed", length = unit(.2, "cm")),
    hjust = 1,
    max.overlaps = 15,
    show.legend = FALSE
  ) +
  ggrepel::geom_label_repel(
    data = . %>%
      group_by(country_region) %>%
      summarise(
        date = last(date),
        r = last(r),
        lbl = paste0(unique(country_region), ": ", round(r, 2))
      ),
    aes(label = lbl),
    label.size = 0,
    direction = "y",
    nudge_x = 10,
    size = 6,
    seed = 123,
    arrow = arrow(type = "closed", length = unit(.2, "cm")),
    hjust = 0,
    max.overlaps = 15,
    show.legend = FALSE
  ) +
  annotate(
    geom = "label",
    x = max_date,
    y = 1.7,
    label = max_date,
    label.size = 0,
    size = 6,
    fontface = "bold"
  ) +
  annotate(
    geom = "label",
    x = min_date,
    y = 1.7,
    label = min_date,
    label.size = 0,
    size = 6,
    fontface = "bold"
  ) +
  scale_y_continuous(
    limits = c(0.2, NA)
  ) +
  scale_x_date(
    limits = c(as.Date("2020-12-10"), as.Date("2021-04-15"))
  ) +
  scale_linetype_manual(
    name = "",
    values = c("above" = "dashed", "below" = "solid")
  ) +
  hrbrthemes::theme_ipsum_pub(
    base_size = 20,
    axis_text_size = 18,
    axis_title_size = 20,
    subtitle_size = 24,
    plot_title_size = 32,
    caption_family = "Inconsolata",
    caption_size = 18
  ) +
  theme(
    plot.subtitle = element_text(color = "grey50")
  ) +
  guides(
    color = guide_none(),
    linetype = guide_none()
  ) +
  labs(
    x = "",
    y = "",
    title = "Evolución del R efectivo estimado (COVID-19) en Sudamérica",
    subtitle = glue::glue("Del 2021-01-01 al {max_date} (Ref: https://doi.org/10.1371/journal.pone.0244474)"),
    caption = glue::glue("Fuente original de datos: https://github.com/crondonm/TrackingR/tree/main/Estimates-Database\nCódigo: https://github.com/jmcastagnetto/misc-viz/tree/main/estimated-r-pone-0244474 // @jmcastagnetto, Jesus M. Castagnetto ({today})")
  )

ggsave(
  plot = psam,
  filename = glue::glue("estimated-r-pone-0244474/sudamerica_r-efectivo-estimado-covid19-{min_date}_{max_date}.png"),
  width = 18,
  height = 12
)

