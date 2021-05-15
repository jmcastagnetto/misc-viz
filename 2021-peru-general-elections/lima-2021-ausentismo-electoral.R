library(tidyverse)
library(sf)
library(patchwork)

# Lima Departamento --------------------------------------------------------

participacion <- readRDS(url("https://github.com/jmcastagnetto/2021-elecciones-generales-peru-datos-de-onpe/raw/main/resultados-participacion-por-distrito.rds"))


lima_dpto <- readRDS("2021-peru-general-elections/gadm36_PER_2_sf.rds") %>%
  filter(
    NAME_1 %in% c("Lima", "Lima Province")
  )


# partic_lima_fn <- "2021-peru-general-elections/Participacion__LIMA_DETALLE_undefined.csv"

upd_lbl <- glue::glue("{unique(participacion$date_upd)} {unique(participacion$hour_upd)}, al {unique(participacion$pct_avance)}% de avance")

#
# upd_lbl <- read_lines(
#   file = partic_lima_fn,
#   skip = 5,
#   n_max = 1
# ) %>%
#   str_remove_all('"') %>%
#   str_to_sentence()

# lima_ausentismo <- read_csv(
#   partic_lima_fn,
#   skip = 9
# ) %>%
#   janitor::clean_names() %>%
#   select(
#     provincia, percent_total_ausentes
#   )

lima_ausentismo <- participacion %>%
  filter(
    departamento == "LIMA"
  ) %>%
  group_by(provincia) %>%
  summarise(
    ausentes = sum(total_ausentes, na.rm = TRUE),
    habiles = sum(electores_habiles, na.rm = TRUE)
  ) %>%
  mutate(
    pct_total_ausentes = 100 * ausentes / habiles
  )

lima_dpto_df <- lima_dpto %>%
  mutate(
    provincia = str_to_upper(NAME_2)
  ) %>%
  left_join(
    lima_ausentismo,
    by = "provincia"
  )


lima_map <- ggplot(lima_dpto_df) +
  geom_sf(aes(fill = pct_total_ausentes),
          color = "white") +
  scale_fill_steps(
    low = "#56B1F7",
    high = "#132B43",
    n.breaks = 5
  ) +
  labs(
    fill = "Porcentaje\nde Ausentes"
  ) +
  theme_void(20) +
  theme(
    legend.key.height = unit(2, "line"),

  )

tabaus <- lima_ausentismo %>%
  select(provincia, pct_total_ausentes) %>%
  arrange(pct_total_ausentes) %>%
  mutate(
    pct_total_ausentes = sprintf("%.1f%%", pct_total_ausentes)
  ) %>%
  rename(
    Provincia = 1,
    "% Ausentes" = 2
  )

p_lima <- wrap_elements(gridExtra::tableGrob(tabaus)) + lima_map +
  plot_layout(
    widths = c(1, 2)
  ) +
  plot_annotation(
    title = "Lima, Perú: Ausentismo en las Elecciones Generales 2021",
    subtitle = glue::glue("Fuente: ONPE ({upd_lbl})"),
    caption = glue::glue("@jmcastagnetto, Jesus M. Castagnetto ({Sys.Date()})")
  ) &
  theme(
    plot.title = element_text(size = 20),
    plot.caption = element_text(family = "Inconsolata", size = 16)
  )

ggsave(
  plot = p_lima,
  filename = "2021-peru-general-elections/lima-peru-2021-ausentismo-electoral.png",
  width = 10,
  height = 10
)

# Lima Provincia -------------------------------------------------------

lima_prov <- readRDS("2021-peru-general-elections/gadm36_PER_3_sf.rds") %>%
  filter(
    NAME_1 %in% c("Lima Province")
  )

# partic_limaprov_fn <- "2021-peru-general-elections/Participacion_LIMA_LIMA_DETALLE_undefined.csv"
#
# upd_lbl <- read_lines(
#   file = partic_limaprov_fn,
#   skip = 5,
#   n_max = 1
# ) %>%
#   str_remove_all('"') %>%
#   str_to_sentence()
#
# limaprov_ausentismo <- read_csv(
#   partic_limaprov_fn,
#   skip = 9
# ) %>%
#   janitor::clean_names() %>%
#   select(
#     distrito, percent_total_ausentes
#   )


limaprov_ausentismo <- participacion %>%
  filter(
    departamento == "LIMA" & provincia == "LIMA"
  ) %>%
  select(
    distrito,
    pct_total_ausentes
  )

lima_prov_df <- lima_prov %>%
  mutate(
    distrito = str_to_upper(NAME_3),
    distrito = if_else(
      distrito == "MAGDALENA VIEJA",
      "PUEBLO LIBRE",
      distrito
    )
  ) %>%
  left_join(
    limaprov_ausentismo,
    by = "distrito"
  )

limaprov_map <- ggplot(lima_prov_df) +
  geom_sf(aes(fill = pct_total_ausentes),
          color = "white") +
  scale_fill_steps(
    low = "#56B1F7",
    high = "#132B43",
    n.breaks = 9
  ) +
  labs(
    fill = "Porcentaje\nde Ausentes"
  ) +
  theme_void(20) +
  theme(
    legend.key.height = unit(3, "line")
  )

tabaus <- limaprov_ausentismo %>%
  arrange(pct_total_ausentes) %>%
  mutate(
    pct_total_ausentes = sprintf("%.1f%%", pct_total_ausentes)
  ) %>%
  rename(
    Distrito = 1,
    "% Ausentes" = 2
  )

p_limaprov <- wrap_elements(gridExtra::tableGrob(tabaus)) + limaprov_map +
  plot_layout(
    widths = c(1, 2)
  ) +
  plot_annotation(
    title = "Lima Provincia, Lima, Perú: Ausentismo en las Elecciones Generales 2021",
    subtitle = glue::glue("Fuente: ONPE ({upd_lbl})"),
    caption = "@jmcastagnetto, Jesus M. Castagnetto 2021-05-12"
  ) &
  theme(
    plot.title = element_text(size = 20),
    plot.caption = element_text(family = "Inconsolata", size = 16)
  )

ggsave(
  plot = p_limaprov,
  filename = "2021-peru-general-elections/lima_provincia-peru-2021-ausentismo-electoral.png",
  width = 12,
  height = 14
)


