library(tidyverse)

participacion <- readRDS(url("https://github.com/jmcastagnetto/2021-elecciones-generales-peru-datos-de-onpe/raw/main/resultados-participacion-por-distrito.rds")) %>%
  mutate(
    name = departamento
  )

act <- glue::glue("Fuente: ONPE. Datos a nivel distrital al {unique(participacion$date_upd)} {unique(participacion$hour_upd)} ({sprintf('%.1f%%', unique(participacion$pct_avance))} de avance)") %>% last()

p1 <- ggplot(
  participacion,
  aes(y = departamento,
      x = pct_total_ausentes / 100,
      fill = departamento)
) +
  ggridges::stat_density_ridges(
    quantile_lines = TRUE,
    quantiles = c(.5),
    alpha = .6,
    show.legend = FALSE,
    jittered_points = TRUE,
    scale = 1.1,
    point_shape = "|",
    point_alpha = 1,
    point_size = 3
  ) +
  scale_x_continuous(labels = scales::percent) +
  labs(
    x = "",
    y = "",
    title = "Elecciones Generales 2021 (Perú):\nDistribución del ausentismo electoral",
    subtitle = act,
    caption = "@jmcastagnetto, Jesus M. Castagnetto, 2021-04-13"
  ) +
  ggridges::theme_ridges() +
  theme(
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(size = 32),
    plot.subtitle = element_text(size = 24, color = "grey50"),
    plot.caption = element_text(family = "Inconsolata", size = 18)
  )

ggsave(
  plot = p1,
  filename = "2021-peru-general-elections/distribucion_ausentismo_electoral.png",
  height = 16,
  width = 12
)
