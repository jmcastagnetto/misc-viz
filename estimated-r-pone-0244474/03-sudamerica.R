library(tidyverse)

sam_df <- readRDS("estimated-r-pone-0244474/data/est_r.rds") %>%
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
lower_limit <- as.Date("2022-01-01")

Sys.setlocale("LC_TIME", "es_PE.utf8")

psam <- ggplot(
  sam_df %>%
    filter(days_infectious == 10) %>%
    filter(date >= lower_limit),
) +
  aes(x = date, y = r, color = country_region) +
  geom_hline(yintercept = 1,
             color = "grey80",
             alpha = 0.5,
             size = 2,
             linetype = "dashed") +
  geom_line(size = 1.5) +
  ggrepel::geom_text_repel(
    data = . %>%
      group_by(country_region) %>%
      summarise(
        date = first(date),
        r = first(r),
        lbl = paste0(unique(country_region), ": ", round(r, 2))
      ),
    aes(label = lbl),
    #label.size = 0,
    direction = "y",
    nudge_x = -10,
    size = 6,
    seed = 123,
    arrow = arrow(type = "closed", length = unit(.2, "cm")),
    hjust = 1,
    max.overlaps = 15,
    show.legend = FALSE
  ) +
  ggrepel::geom_text_repel(
    data = . %>%
      group_by(country_region) %>%
      summarise(
        date = last(date),
        r = last(r),
        lbl = paste0(unique(country_region), ": ", round(r, 2))
      ),
    aes(label = lbl),
    #label.size = 0,
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
    y = 3.5,
    label = max_date,
    label.size = 0,
    size = 9,
    fontface = "bold"
  ) +
  annotate(
    geom = "label",
    x = lower_limit,
    y = 3.5,
    label = lower_limit,
    label.size = 0,
    size = 9,
    fontface = "bold"
  ) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b\n%Y",
    limits = c(lower_limit - 30, max_date + 30)
  ) +
  scale_color_brewer(type = "qual", palette = "Set3") +
  # scale_linetype_manual(
  #   name = "",
  #   values = c("above" = "dotted", "below" = "solid")
  # ) +
  ggdark::dark_theme_classic(20) +
  theme(
    axis.text = element_text(color = "white", size = 18),
    plot.title = element_text(color = "white", size = 32),
    plot.subtitle = element_text(color = "white", size = 24),
    #plot.background = element_rect(fill = "grey20"),
    plot.caption = element_text(family = "Inconsolata", size = 18)
  ) +
  guides(
    color = guide_none(),
    linetype = guide_none()
  ) +
  labs(
    x = "",
    y = "",
    title = "Evolución del R efectivo estimado (COVID-19) en Sudamérica",
    subtitle = glue::glue("Del {lower_limit} al {max_date} (Ref: https://doi.org/10.1371/journal.pone.0244474)"),
    caption = glue::glue("Fuente original de datos: https://github.com/crondonm/TrackingR/tree/main/Estimates-Database\nCódigo: https://github.com/jmcastagnetto/misc-viz/tree/main/estimated-r-pone-0244474\n@jmcastagnetto, Jesus M. Castagnetto ({today})")
  )

ggsave(
  plot = psam,
  filename = glue::glue("estimated-r-pone-0244474/sudamerica_r-efectivo-estimado-covid19-{lower_limit}_{max_date}.png"),
  width = 22,
  height = 18
)

