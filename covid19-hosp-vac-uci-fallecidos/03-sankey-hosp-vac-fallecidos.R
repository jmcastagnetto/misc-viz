library(tidyverse)
library(ggsankey)

sankey_df <- read_fst("covid19-hosp-vac-uci-fallecidos/hosp_vac_fallecido.rds") %>%
  mutate(
    sexo = case_when(
      sexo == "F" ~ "Femenino",
      sexo == "M" ~ "Masculino",
      TRUE ~ "No registrado"
    ) %>%
      factor(),
    flag_vacuna = if_else(
      flag_vacuna %in% 1:2,
      paste0("Dosis: ", flag_vacuna),
      "Sin vacuna") %>%
      factor(),
    cdc_fallecido_covid = if_else(
      cdc_fallecido_covid,
      "Fallecido",
      "No fallecido") %>%
      factor()
  ) %>%
  select(
    Sexo = sexo,
    Vacunado = flag_vacuna,
    Fallecido = cdc_fallecido_covid
  ) %>%
  make_long(
    Sexo,
    Vacunado,
    Fallecido
  )

ggplot(
  sankey_df,
  aes(
    x = x,
    next_x = next_x,
    node = node,
    next_node = next_node,
    fill = factor(node),
    label = node
  )) +
  geom_sankey(flow.alpha = .8,
              node.color = "gray30") +
  geom_sankey_label(size = 4, fontface = "bold",
                    color = "white", fill = "gray40",
                    width = .1)  +
  scale_fill_brewer(palette = "Paired") +
  theme_sankey(base_size = 18) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = .5, size = 24),
    plot.subtitle = element_text(hjust = .5, color = "grey40"),
    plot.caption = element_text(family = "Inconsolata", hjust = .5)
  ) +
  labs(
    x = "",
    y = "",
    title = "De los pacientes hospitalizados por COVID-19\naquellos vacunados fallecieron con menos frecuencia",
    subtitle = "Fuente: Datos abiertos del MINSA",
    caption = "@jmcastagnetto, Jesus M. Castagnetto (2021-09-05)"
  )

ggsave(
  "covid19-hosp-vac-uci-fallecidos/sankey_hosp_vacunados_fallecidos.png",
  width = 12,
  height = 10
)

