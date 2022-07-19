library(tidyverse)

rds_url <- "https://github.com/jmcastagnetto/covid-19-peru-vacunas/raw/main/datos/vacunas_covid_rangoedad_quintiles.rds"

lastwk <- readRDS(url(rds_url)) %>%
  filter(complete_epi_week == 1) %>%
  filter(last_day_of_epi_week == max(last_day_of_epi_week)) %>%
  mutate(
    rango_edad = factor(
      rango_edad,
      levels = c(
        "0-4",
        "5-9",
        "10-14",
        "15-19",
        "20-24",
        "25-29",
        "30-34",
        "35-39",
        "40-44",
        "45-49",
        "50-54",
        "55-59",
        "60-64",
        "65-69",
        "70-74",
        "75-79",
        "80+"
      ),
      ordered = TRUE
    ),
    dosis = paste0("Dosis N° ", dosis)
  )

last_epiweek = unique(lastwk$epi_week)
last_epiyear = unique(lastwk$epi_year)
last_dow = unique(lastwk$last_day_of_epi_week)

p1 <- ggplot(
  lastwk,
  aes(x = rango_edad, y = pct_acum, group = dosis)
) +
  geom_col(position = "dodge", aes(fill = dosis), alpha = 1) +
  scale_y_continuous(
    labels = scales::percent,
    n.breaks = 10
  ) +
  colorblindr::scale_fill_OkabeIto(darken = 0.2) +
  ggthemes::theme_clean(base_size = 16) +
  theme(
    plot.title = element_text(size = 32, face = "bold"),
    plot.subtitle = element_text(size = 24, color = "gray50"),
    plot.caption = element_text(family = "Inconsolata", size = 14),
    panel.background = element_rect(color = "white", fill = "white"),
    panel.spacing = unit(2, "lines"),
    plot.margin = unit(rep(1, 4), "cm"),
    axis.title = element_text(size = 18),
    legend.position = "top"
  ) +
  labs(
    fill = "Dosis:",
    y = "Porcentaje de la población en el rango de edades",
    x = "Rango de edades",
    title = "Vacunación COVID-19 (Perú): Cobertura por rango de edades y dosis",
    subtitle = glue::glue("Fuente de datos: MINSA e INEI // Datos hasta la semana epidemiológica N° {last_epiweek} del {last_epiyear} ({last_dow})"),
    caption = glue::glue("@jmcastagnetto, Jesus M. Castagnetto ({Sys.Date()})")
  )

ggsave(
  plot = p1,
  filename = "covid19-vaccines/peru-vaccine-coverage-agegroup-dosis.png",
  width = 20,
  height = 13
)
