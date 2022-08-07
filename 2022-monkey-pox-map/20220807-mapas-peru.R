library(tidyverse)
library(sf)
library(geodata)
library(patchwork)


# Datos de Monkeypox al 2022-08-07 ----------------------------------------
# CSV descargado de https://www.dge.gob.pe/sala-monkeypox/
mp_peru <- read_csv("2022-monkey-pox-map/20220807-monkeypox_peru.csv") %>%
  janitor::clean_names() %>%
  select(-diresa_geresa_diris) %>%
  mutate(
    # Normalize Departamento
    departamento = case_when(
      departamento == "LIMA METROPOLITANA" ~ "LIMA PROVINCE",
      departamento == "LIMA REGION" ~ "LIMA",
      TRUE ~ departamento
    )
  )

# Mapa a nivel de provincias

peru <- gadm(country = "Peru", level = 2, version = "4.1",
             path = "2022-monkey-pox-map/") %>%
  st_as_sf() %>%
  mutate(
    NAME_1 = str_to_upper(NAME_1) %>%
      iconv(to='ASCII//TRANSLIT'),
    NAME_2 = str_to_upper(NAME_2) %>%
      iconv(to='ASCII//TRANSLIT')
  )

mp_prov <- mp_peru %>%
  group_by(
    departamento,
    provincia
  ) %>%
  summarise(
    casos_prov = sum(casos, na.rm = TRUE)
  )

peru_map_df <- peru %>%
  left_join(
    mp_prov,
    by = c(
      "NAME_1" = "departamento",
      "NAME_2" = "provincia"
    )
  )

p_peru <- ggplot(peru_map_df) +
  geom_sf(aes(fill = casos_prov), color = "black", size = 0.1) +
  scale_fill_fermenter(
    direction = 1,
    palette = "YlOrRd",
    na.value = "white",
    n.breaks = 10,
    show.limits = TRUE
  ) +
  theme_void() +
  theme(
    legend.key.height = unit(2, "cm"),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16, face = "bold"),
    legend.title.align = 0.5
  ) +
  labs(
    fill = "Casos\n(Provincia)",
    subtitle = "A nivel de provincias"
  )

# A nivel de Lima provincia

lima <- gadm(country = "Peru", level = 3, version = "4.1",
                     path = "2022-monkey-pox-map/") %>%
  st_as_sf() %>%
  filter(NAME_1 == "Lima Province") %>%
  mutate(
    NAME_1 = str_to_upper(NAME_1) %>%
      iconv(to = 'ASCII//TRANSLIT'),
    NAME_2 = str_to_upper(NAME_2) %>%
      iconv(to = 'ASCII//TRANSLIT'),
    # Fix oustanding error in name for "PUEBLO LIBRE"
    NAME_3 = str_to_upper(NAME_3) %>%
      iconv(to = 'ASCII//TRANSLIT') %>%
      str_replace("MAGDALENA VIEJA", "PUEBLO LIBRE")
  )

lima_dist <- mp_peru %>%
  filter(departamento == "LIMA PROVINCE") %>%
  mutate(
    departamento = iconv(departamento, to = 'ASCII//TRANSLIT'),
    provincia = iconv(provincia, to = 'ASCII//TRANSLIT'),
    distrito = iconv(distrito, to = 'ASCII//TRANSLIT')
  )

lima_map_df <- lima %>%
  left_join(
    lima_dist,
    by = c(
      "NAME_1" = "departamento",
      "NAME_2" = "provincia",
      "NAME_3" = "distrito"
    )
  )

p_lima <- ggplot(lima_map_df) +
  geom_sf(aes(fill = casos), color = "black", size = 0.1) +
  scale_fill_fermenter(
    direction = 1,
    palette = "YlGnBu",
    na.value = "white",
    n.breaks = 10,
    show.limits = TRUE
  ) +
  theme_void() +
  theme(
    legend.key.height = unit(2, "cm"),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16, face = "bold"),
    legend.title.align = 0.5
  ) +
  labs(
    fill = "Casos",
    subtitle = "Provincia de Lima"
  )

# Combinar los mapas

p_combined <- (p_peru + p_lima) +
  plot_annotation(
    title = "Casos de 'Viruela del Mono' (Monkeypox) en Perú, 2022",
    caption = "Fuente: MINSA/DGE (al 2022-08-07) -- @jmcastagnetto, Jesus M. Castagnetto"
  ) &
  theme(
    text = element_text("EB Garamond SC"),
    plot.title = element_text(size = 38),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = 36, color = "darkblue", hjust = 0.5),
    plot.caption = element_text(size = 16, color = "grey30"),
    plot.background = element_rect(fill = "grey94", color = "grey94"),
    plot.margin = unit(rep(1, 4), "cm")
  )

ggsave(
  plot = p_combined,
  filename = "2022-monkey-pox-map/20220807-mapas-monkeypox-casos-peru.png",
  width = 14,
  height = 10
)
