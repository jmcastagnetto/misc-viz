library(tidyverse)
library(sf)

peru <- readRDS("2021-peru-general-elections/peru-geo.rds")

ubigeos <- readRDS(url("https://github.com/jmcastagnetto/ubigeo-peru-aumentado/raw/main/ubigeos_reniec_inei_aumentado.rds"))

participacion <- readRDS(url("https://github.com/jmcastagnetto/2021-elecciones-generales-peru-datos-de-onpe/raw/main/resultados-participacion-por-distrito.rds")) %>%
  left_join(
    ubigeos %>%
      select(reniec, inei),
    by = c("ubigeo_distrito" = "reniec")
  )

peru_map <- peru %>%
  left_join(
    participacion %>%
      select(inei, distrito, pct_total_ausentes),
    by = c("UBIGEO" = "inei")
  )

min_ausentes <- participacion %>%
  filter(pct_total_ausentes == min(pct_total_ausentes, na.rm = TRUE))

max_ausentes <- participacion %>%
  filter(pct_total_ausentes == max(pct_total_ausentes, na.rm = TRUE))

note <- glue::glue(
  "El ausentismo va de un {sprintf('%.1f%%', min_ausentes$pct_total_ausentes)} en el distrito de {str_to_title(min_ausentes$distrito)} ({str_to_title(min_ausentes$provincia)}, {str_to_title(min_ausentes$departamento)}), a un {sprintf('%.1f%%', max_ausentes$pct_total_ausentes)} en el distrito de {str_to_title(max_ausentes$distrito)} ({str_to_title(max_ausentes$provincia)}, {str_to_title(max_ausentes$departamento)})"
) %>%
  str_wrap(width = 24)

p1 <- ggplot(peru_map) +
  geom_sf(aes(fill = pct_total_ausentes),
          size = .05,
          color = "black") +
  scale_fill_steps(
    low = "#dadaeb",
    high = "#54278f",
    na.value = "white",
    n.breaks = 10
  ) +
  annotate(
    geom = "label",
    x = -73,
    y = -5.5,
    label = note,
    hjust = 0,
    vjust = 1,
    label.size = 0,
    size = 6
  ) +
  labs(
    title = "Perú: Ausentismo electoral a nivel de distritos",
    subtitle = "Elecciones Generales 2021. Fuente: ONPE (al 2021-04-16)",
    caption = "@jmcastagnetto, Jesus M. Castagnetto, 2021-04-16",
    fill = "% de ausentes"
  ) +
  theme_void() +
  theme(
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(size = 24),
    plot.subtitle = element_text(size = 18, color = "grey50"),
    plot.caption = element_text(family = "Inconsolata", size = 14),
    legend.key.height = unit(3, "lines"),
    legend.text = element_text(size = 14),
    legend.title = element_text(face = "bold"),
    legend.position = c(.2, .3)
  )

ggsave(
  filename = "2021-peru-general-elections/mapa_distrital_ausentismo_eegg2021.png",
  width = 10,
  height = 14
)

