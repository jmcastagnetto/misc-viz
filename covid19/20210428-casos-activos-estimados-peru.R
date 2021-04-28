library(tidyverse)
library(vroom)
library(ggforce)

spec <- cols(
  .default = col_double(),
  country = col_character(),
  iso3c = col_character(),
  region = col_character(),
  date = col_date(format = "")
)

peru_df <- vroom(
  file = "https://github.com/jmcastagnetto/covid-19-peru-data/raw/main/datos/covid-19-peru-data.csv",
  col_types = spec
)

subset_df <- peru_df %>%
  select(
    date,
    confirmed,
    deaths,
    recovered,
    total_tests
  ) %>%
  drop_na() %>% # get all rows with complete data
  mutate(
    active_cases = confirmed - (recovered + deaths)
  )

Sys.setlocale("LC_TIME", "es_PE.utf8")
ggplot(
  subset_df,
  aes(x = date, y = active_cases)
) +
  geom_point(
    aes(
      color = (active_cases > 0),
      shape = (active_cases < 0),
      size = (active_cases < 0)
    ),
    show.legend = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  annotate(
    geom = "text",
    x = as.Date("2021-03-01"),
    y = 100000,
    label = "Valores negativos para el\nnúmero de casos activos el\n2021-04-26 y 2021-04-27",
    hjust = 1,
    vjust = 0,
    size = 6,
    color = "peru",
    fontface = "bold"
  ) +
  annotate(
    geom = "curve",
    x = as.Date("2021-03-01"),
    y = 100000,
    xend = as.Date("2021-04-26"),
    yend = -2100,
    curvature = -.2,
    color = "peru",
    size = 2,
    alpha = .7,
    arrow = arrow(length = unit(2, "mm"))
  ) +
  scale_color_manual(
    values = c("peru", "purple")
  ) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    x = "",
    y = "Número de casos",
    title = "COVID-19 - Perú: Casos activos estimados",
    subtitle = "Usando datos históricos publicados por MINSA - [Activos = Positivos - (Fallecidos + Recuperados)]\nDatos del repositorio: https://github.com/jmcastagnetto/covid-19-peru-data/",
    caption = "@jmcastagnetto, Jesus M. Castagnetto (2021-04-28)"
  ) +
  theme_minimal(18) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(size = 24, face = "bold"),
    plot.subtitle = element_text(color = "gray50"),
    plot.caption = element_text(family = "Inconsolata", size = 16)
  )

ggsave(
  filename = "covid19/20210428-casos-activos-estimados-peru.png",
  width = 12,
  height = 8
)

