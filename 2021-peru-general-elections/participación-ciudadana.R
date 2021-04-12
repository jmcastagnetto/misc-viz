library(tidyverse)
library(sf)
library(patchwork)

partic_peru_fn <-"2021-peru-general-elections/Participacion_TODOS_DETALLE_undefined.csv"

upd_lbl <- read_lines(
  file = partic_peru_fn,
  skip = 5,
  n_max = 1
) %>%
  str_remove_all('"') %>%
  str_to_sentence()

partic_peru <- read_csv(
  partic_peru_fn,
  skip = 9
) %>%
  janitor::clean_names()

# pct_peru <- partic_peru %>%
#   select(
#     departamento,percent_total_asistentes, percent_total_ausentes
#   ) %>%
#   mutate(
#     departamento = factor(departamento) %>%
#       fct_reorder(percent_total_asistentes)
#   ) %>%
#   pivot_longer(
#     cols = 2:3,
#     names_to = "metric",
#     values_to = "value",
#     names_prefix = "percent_total_"
#   ) %>%
#   mutate(
#     metric = str_to_title(metric) %>%
#       factor(
#         levels = c("Ausentes", "Asistentes")
#       ),
#     xlbl = if_else(metric == "Asistentes", 30, 90)
#   )
#
# ggplot(
#   pct_peru,
#   aes(y = departamento, x = value, fill = metric)
# ) +
#   geom_col() +
#   geom_text(
#     aes(label = sprintf("%.1f%%", value), x = xlbl),
#     color = "white",
#     fontface = "bold"
#   ) +
#   scale_fill_manual(
#     values = c("Asistentes" = "#7570b3", "Ausentes" = "#d95f02")
#   ) +
#   labs(
#     y = "",
#     x = "Porcentaje de electores hábiles",
#     fill = "",
#     title = "Perú: Participación ciudadadana en las Elecciones Generales 2021",
#     subtitle = glue::glue("Fuente: ONPE ({upd_lbl})"),
#     caption = ""
#   ) +
#   theme_minimal(20) +
#   theme(
#     plot.title.position = "plot",
#     legend.position = "bottom"
#   )

peru_map <- readRDS("~/Documents/datasets/peru-geodata/gadm_org/gadm36_PER_1_sf.rds")

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
      select(departamento, percent_total_ausentes),
    by = "departamento"
  )

plot_map <- ggplot(
  data = peru_df
) +
  geom_sf(aes(fill = percent_total_ausentes), color = "white") +
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
    legend.key.height = unit(2, "line"),

  )

tabaus <- partic_peru %>%
  select(departamento, percent_total_ausentes) %>%
  mutate(
    departamento = as.character(departamento)
  ) %>%
  arrange(percent_total_ausentes) %>%
  mutate(
    percent_total_ausentes = sprintf("%.1f%%", percent_total_ausentes)
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
    caption = "@jmcastagnetto, Jesus M. Castagnetto 2021-05-12"
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
