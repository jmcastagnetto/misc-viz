library(tidyverse)
library(sf)

primera <- readRDS("2021-peru-general-elections/onpe-datos-primera-vuelta.rds")
segunda <- readRDS("2021-peru-general-elections/onpe-datos-segunda-vuelta.rds")
peru <- readRDS("2021-peru-general-elections/peru-geo.rds")

por_distrito_df <- primera %>%
  filter(donde == "Perú") %>%
  group_by(inei) %>%
  summarise(
    aus_v1 = 1 - (sum(n_cvas, na.rm = TRUE) / sum(n_elec_habil, na.rm = TRUE))
  ) %>%
  left_join(
    segunda_por_distrito <- segunda %>%
      filter(donde == "Perú") %>%
      group_by(inei) %>%
      summarise(
        aus_v2 = 1 - (sum(n_cvas, na.rm = TRUE) / sum(n_elec_habil, na.rm = TRUE))
      ),
    by = "inei"
  ) %>%
  mutate(
    comp_v1_v2 = case_when(
      aus_v1 < aus_v2 ~ "Aumentó",
      near(aus_v1, aus_v2) ~ "Igual",
      aus_v1 > aus_v2 ~ "Disminuyó"
    ) %>%
      factor(
        levels = c("Disminuyó", "Igual", "Aumentó"),
        ordered = TRUE
      )
  )

map_df <- peru %>%
  left_join(
    por_distrito_df %>%
      select(UBIGEO = inei, comp_v1_v2),
    by = "UBIGEO"
  ) %>%
  filter(!is.na(comp_v1_v2))

p1 <- ggplot(map_df) +
  geom_sf(aes(fill = comp_v1_v2), size = .1, color = "white") +
  scale_fill_manual(
    values = c(
      "Disminuyó" = "#78c679",
      "Igual" = "#ffffb3",
      "Aumentó" = "#2c7fb8"
    )
  ) +
  theme_void(base_size = 16) +
  labs(
    x = "",
    y = "",
    fill = "",
    title = "Comparación del ausentismo electoral\nentre la Primera y Segunda vueltas",
    subtitle = "Elecciones Presidenciales 2021, Perú.\nFuente de datos: ONPE",
    caption = "@jmcastagnetto, Jesus M. Castagnetto (2021-06-19)"
  ) +
  theme(
    plot.title.position = "plot",
    plot.subtitle = element_text(color = "gray40"),
    legend.text = element_text(size = 12),
    plot.caption = element_text(family = "Inconsolata", size = 10)
  )

ggsave(
  plot = p1,
  filename = "2021-peru-general-elections/comparacion-ausentismo-electoral-1ra-2da-vueltas.png",
  width = 6,
  height = 8
)
