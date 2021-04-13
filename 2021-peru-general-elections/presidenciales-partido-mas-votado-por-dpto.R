library(tidyverse)
library(sf)
library(patchwork)

peru_map <- readRDS("2021-peru-general-elections/gadm36_PER_1_sf.rds") %>%
  mutate(
    departamento = str_to_upper(NAME_1) %>%
      iconv(to='ASCII//TRANSLIT'),
    departamento = if_else(
      departamento == "LIMA PROVINCE",
      "LIMA",
      departamento
    )
  )

partidos_dpto <- readRDS(
  "2021-peru-general-elections/resultados-partidos-dpto.rds"
)
top1_dpto <- partidos_dpto %>%
  arrange(departamento, partido, desc(votos)) %>%
  group_by(departamento) %>%
  top_n(n = 1, wt = votos)

peru_partidos_df <- peru_map %>%
  left_join(
    top1_dpto,
    by = "departamento"
  )


p1 <- ggplot(
  peru_partidos_df
) +
  geom_sf(aes(fill = partido), color = "white") +
  scale_fill_brewer(
    palette = "Paired",
    type = "qual"
  ) +
  labs(
    fill = ""
  ) +
  theme_void(18) +
  theme(
    legend.position = "bottom",
    legend.key.height = unit(2, "line"),
    legend.direction = "horizontal",
  ) +
  guides(
    fill = guide_legend(nrow = 2)
  )

tbl_df <- top1_dpto %>%
  select(departamento, partido, pct_validos) %>%
  mutate(
    pct_validos = sprintf("%.1f%%", pct_validos)
  ) %>%
  rename(
    Departamento = 1,
    "Partido Político" = 2,
    "% de Votos Válidos" = 3
  )

pcomb <- wrap_elements(
  gridExtra::tableGrob(
    tbl_df,
    theme = gridExtra::ttheme_default(
      base_size = 12
    )
    )
  ) + p1 +
  plot_layout(
    widths = c(1, 1),
    guides = "collect"
  ) +
  plot_annotation(
    title = "Perú: Elecciones Presidenciales 2021",
    subtitle = "Partido con mayor votación en cada Departamento (Fuente: ONPE)",
    caption = "@jmcastagnetto, Jesus M. Castagnetto 2021-05-12"
  ) &
  theme(
    plot.title = element_text(size = 24),
    plot.subtitle = element_text(size = 22, color = "grey50"),
    plot.caption = element_text(family = "Inconsolata", size = 16),
    legend.position = "bottom"
  )

ggsave(
  plot = pcomb,
  filename = "2021-peru-general-elections/presidenciales_partido_mayor_voto_dpto.png",
  width = 17,
  height = 12
)
