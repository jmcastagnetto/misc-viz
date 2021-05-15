library(tidyverse)
library(sf)
library(cartogram)
library(patchwork)

peru <- readRDS("2021-peru-general-elections/gadm36_PER_1_sf.rds") %>%
  mutate(
    departamento = str_to_upper(NAME_1) %>%
      iconv(to='ASCII//TRANSLIT') %>%
      str_replace("LIMA PROVINCE", "LIMA PROVINCIA")
  )

peru_habiles <- readRDS(url("https://github.com/jmcastagnetto/2021-elecciones-generales-peru-datos-de-onpe/raw/main/resultados-participacion-por-distrito.rds")) %>%
  mutate(
    departamento = if_else(
      departamento == "LIMA" &
        provincia == "LIMA",
      "LIMA PROVINCIA",
      departamento
    )
  ) %>%
  group_by(departamento) %>%
  summarise(
    total_habiles = sum(electores_habiles, na.rm = TRUE)
  )

peru_df <- peru %>%
  left_join(
    peru_habiles,
    by = "departamento"
  ) %>%
  st_transform(crs = 5389) # ref: https://epsg.io/5839

peru_carto <- cartogram_cont(peru_df, "total_habiles", itermax = 20)

pal_peru <- pals::alphabet2(n = 26) %>% as.character()

p1 <- ggplot(peru_df) +
  geom_sf(aes(fill = departamento)) +
  scale_fill_manual(
    values = pal_peru
  ) +
  labs(fill = "") +
  theme_void()

p2 <- ggplot(peru_carto) +
  geom_sf(aes(fill = departamento), show.legend = FALSE) +
  geom_sf_text(aes(label = departamento), show.legend = FALSE) +
  scale_fill_manual(
    values = pal_peru
  ) +
  labs(fill = "") +
  theme_void()

p12 <- (p1 + p2) +
 # plot_layout(guides = "collect") +
  plot_annotation(
    title = "Mapa y Cartograma de Electores Hábiles (Perú)",
    subtitle = "La desigual y extrema influencia de Lima Provincia es notoria.",
    caption = glue::glue("Fuente: ONPE, Elecciones Generales 2021 // @jmcastagnetto, Jesus M. Castagnetto ({Sys.Date()})")
  ) &
  theme(
    plot.title = element_text(size = 32),
    plot.subtitle = element_text(size = 24, color = "grey40"),
    plot.caption = element_text(family = "Inconsolata", size = 20),
    legend.text = element_text(size = 14),
    legend.key.height = unit(1, "cm")
  )
p12

ggsave(
  plot = p12,
  filename = "2021-peru-general-elections/mapa-cartograma-electores-habiles-2021.png",
  width = 16,
  height = 9
)
