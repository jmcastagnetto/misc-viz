library(tidyverse)


por_edad_sexo <- readRDS("sinadef-evolucion-grupo-etareo/fallecidos_peru_grupo_etario_sexo.rds") %>%
  mutate(
    epi_year = factor(epi_year)
  ) %>%
  filter(
    paste0(epi_year, "-", epi_week) != "2021-15" # remover la semana incompleta mas reciente
  )


p1 <- ggplot(
  por_edad_sexo,
  aes(x = epi_week, y = n, group = epi_year, color = epi_year)
) +
  geom_line(size = 1) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(
    values = c(
      "2017" = "#fbb4ae",
      "2018" = "#b3cde3",
      "2019" = "#b2df8a",
      "2020" = "#377eb8",
      "2021" = "#e41a1c"
    )
  ) +
  labs(
    x = "Semana Epidemiológica",
    y = "Número de fallecidos por todas las causas",
    title = "Perú: Comparando fallecimientos semanales, por grupo etario y sexo",
    subtitle = "Fuente: SINADEF, al 2021-04-17 (registros con edad y sexo: ~96.7% del total)",
    caption = "@jmcastagnetto, Jesus M. Castagnetto",
    color = ""
  ) +
  ggdark::dark_theme_bw(base_family = "Roboto") +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(size = 28),
    plot.subtitle = element_text(size = 22, color = "grey50"),
    plot.caption = element_text(family = "Inconsolata", size = 18),
    legend.position = "bottom",
    legend.text = element_text(size = 14),
    legend.key.width = unit(1, "cm"),
    axis.title = element_text(size = 18, face = "bold", hjust = 1),
    axis.text = element_text(size = 14),
    strip.text = element_text(size = 18, face = "bold"),
    plot.margin = unit(rep(.5, 4), "cm")
  ) +
  facet_grid(sexo~grupo_etario)

ggsave(
  plot = p1,
  filename = "sinadef-evolucion-grupo-etareo/sinadef-comparacion-grupo-etario-sexo.png",
  width = 16,
  height = 9
)
