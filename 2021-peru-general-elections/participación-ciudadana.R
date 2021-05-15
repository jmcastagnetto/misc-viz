library(tidyverse)
library(sf)
library(patchwork)

# partic_peru_fn <-"2021-peru-general-elections/Participacion_TODOS_DETALLE_undefined.csv"


participacion <- readRDS(url("https://github.com/jmcastagnetto/2021-elecciones-generales-peru-datos-de-onpe/raw/main/resultados-participacion-por-distrito.rds"))

partic_peru <- participacion %>%
  group_by(departamento) %>%
  summarise(
    ausentes = sum(total_ausentes, na.rm = TRUE),
    habiles = sum(electores_habiles, na.rm = TRUE)
  ) %>%
  mutate(
    pct_total_ausentes = 100 * ausentes / habiles
  )

upd_lbl <- glue::glue("{unique(participacion$date_upd)} {unique(participacion$hour_upd)}, al {unique(participacion$pct_avance)}% de avance")

# upd_lbl <- read_lines(
#   file = partic_peru_fn,
#   skip = 5,
#   n_max = 1
# ) %>%
#   str_remove_all('"') %>%
#   str_to_sentence()
#
# partic_peru <- read_csv(
#   partic_peru_fn,
#   skip = 9
# ) %>%
#   janitor::clean_names()

peru_map <- readRDS("2021-peru-general-elections/gadm36_PER_1_sf.rds")

peru_df <- peru_map %>%
  mutate(
    departamento = str_to_upper(NAME_1) %>%
      iconv(to='ASCII//TRANSLIT'),
    departamento = if_else(
      departamento == "LIMA PROVINCE",
      "LIMA",
      departamento
    )
  ) %>%
  left_join(
    partic_peru %>%
      select(departamento, pct_total_ausentes),
    by = "departamento"
  )

plot_map <- ggplot(
  data = peru_df
) +
  geom_sf(aes(fill = pct_total_ausentes), color = "white") +
  scale_fill_steps(
    low = "#56B1F7",
    high = "#132B43",
    n.breaks = 10
  ) +
  labs(
    fill = "Porcentaje\nde Ausentes"
  ) +
  theme_void(20) +
  theme(
    legend.key.height = unit(2, "line")
  )

tabaus <- partic_peru %>%
  select(departamento, pct_total_ausentes) %>%
  mutate(
    departamento = as.character(departamento)
  ) %>%
  arrange(pct_total_ausentes) %>%
  mutate(
    pct_total_ausentes = sprintf("%.1f%%", pct_total_ausentes)
  ) %>%
  rename(
    Departamento = 1,
    "% Ausentes" = 2
  )

p2 <- wrap_elements(gridExtra::tableGrob(tabaus)) + plot_map +
  plot_layout(
    widths = c(1, 2)
  ) +
  plot_annotation(
    title = "Perú: Ausentismo en las Elecciones Generales 2021",
    subtitle = glue::glue("Fuente: ONPE ({upd_lbl})"),
    caption = glue::glue("@jmcastagnetto, Jesus M. Castagnetto ({Sys.Date()})")
  ) &
  theme(
    plot.title = element_text(size = 20),
    plot.caption = element_text(family = "Inconsolata", size = 16)
  )

ggsave(
  plot = p2,
  filename = "2021-peru-general-elections/peru-2021-ausentismo-electoral.png",
  width = 10,
  height = 10
)
