library(tidyverse)
library(sf)

peru <- readRDS("2021-peru-general-elections/peru-geo.rds")
ubigeos <- readRDS(url("https://github.com/jmcastagnetto/ubigeo-peru-aumentado/raw/main/ubigeos_reniec_inei_aumentado.rds"))
pres <- readRDS(url("https://github.com/jmcastagnetto/2021-elecciones-generales-peru-datos-de-onpe/raw/main/presidencial-resultados-partidos.rds"))

pres_top1 <- pres %>%
  mutate(
    partido = as.character(partido)
  ) %>%
  arrange(
    ubigeo, departamento, provincia, distrito,
    partido, desc(pct_validos)
  ) %>%
  left_join(
    ubigeos %>% select(reniec, inei),
    by = c("ubigeo" = "reniec")
  ) %>%
  group_by(
    inei, departamento, provincia, distrito
  ) %>%
  top_n(n = 1, wt = pct_validos)


pct_completion <- pres_top1 %>%
  pull(pct_avance) %>%
  unique()
upd_date <- pres_top1 %>% pull(date_upd) %>% unique()
upd_time <- pres_top1 %>% pull(hour_upd) %>% unique()
upd_ts <- glue::glue("{upd_date} {upd_time}")


peru <- readRDS("2021-peru-general-elections/peru-geo.rds")

peru_map <- peru %>%
  mutate(
    CODIGO = case_when(
      NOMBPROV == "SAN ROMAN" & NOMBDIST == "SAN MIGUEL" ~ "211105",
      NOMBPROV == "SATIPO" & NOMBDIST == "MAZAMARI - PANGOA" ~ "120604",
      TRUE ~ CODIGO
    )
  ) %>%
  left_join(
    pres_top1 %>%
      ungroup() %>%
      select(inei, partido, pct_validos) %>%
      group_by(partido) %>%
      mutate(
        n_dist = n()
      ) %>%
      ungroup() %>%
      mutate(
        partido = fct_reorder(partido, desc(n_dist))
      ),
    by = c("CODIGO" = "inei")
  )

p1 <- ggplot(peru_map) +
  geom_sf(aes(fill = as.factor(partido)),
          size = .05,
          color = "black") +
  scale_fill_manual(
    values = c(
      "#A6CEE3",
      "#1F78B4",
      "#B2DF8A",
      "#33A02C",
      "#FB9A99",
      "#E31A1C",
      "#FDBF6F",
      "#FF7F00",
      "#333333",
      "#999999",
      "#FFFF99"
    )
  ) +
  labs(
    title = "Elección presidencial: Partido con mas votos por distrito",
    subtitle = glue::glue("Perú: Elecciones Generales 2021 - Primera vuelta\nFuente: ONPE (al {upd_ts}, {pct_completion} % de avance)"),
    caption = "@jmcastagnetto, Jesus M. Castagnetto, 2021-04-23",
    fill = ""
  ) +
  theme_void(18) +
  theme(
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(size = 24),
    plot.subtitle = element_text(size = 18, color = "grey50"),
    plot.caption = element_text(family = "Inconsolata", size = 14),
    plot.margin = unit(rep(.5, 4), "cm"),
    legend.key.height = unit(3, "lines")
  )

ggsave(
  plot = p1,
  filename = "2021-peru-general-elections/mapa_distrital_presidencial_partidomasvotado_eegg2021.png",
  width = 14,
  height = 14
)


# Seleccionando a los partidos que van a la 2da vuelta --------------------

pres_pl_fp_top1 <- pres %>%
  filter(partido %in% c("PARTIDO POLITICO NACIONAL PERU LIBRE", "FUERZA POPULAR")) %>%
  arrange(
    ubigeo, departamento, provincia, distrito,
    partido, desc(pct_validos)
  ) %>%
  left_join(
    ubigeos %>% select(reniec, inei),
    by = c("ubigeo" = "reniec")
  ) %>%
  group_by(
    inei, departamento, provincia, distrito
  ) %>%
  top_n(n = 1, wt = pct_validos)

peru_map2 <- peru %>%
  mutate(
    CODIGO = case_when(
      NOMBPROV == "SAN ROMAN" & NOMBDIST == "SAN MIGUEL" ~ "211105",
      NOMBPROV == "SATIPO" & NOMBDIST == "MAZAMARI - PANGOA" ~ "120604",
      TRUE ~ CODIGO
    )
  ) %>%
  left_join(
    pres_pl_fp_top1 %>%
      ungroup() %>%
      select(inei, partido, pct_validos) %>%
      group_by(partido) %>%
      mutate(
        n_dist = n()
      ) %>%
      ungroup() %>%
      mutate(
        partido = fct_reorder(partido, desc(n_dist))
      ),
    by = c("CODIGO" = "inei")
  )

p2 <- ggplot(peru_map2) +
  geom_sf(aes(fill = as.factor(partido)),
          size = .05,
          color = "black") +
  scale_fill_manual(
    values = c("#c2a5cf", "#fdb863")
  ) +
  labs(
    title = "Elección presidencial (Primera vuelta)\nVotos de los partidos que van a segunda vuelta",
    subtitle = glue::glue("Perú: Elecciones Generales 2021\nFuente: ONPE (al {upd_ts}, {pct_completion} % de avance)"),
    caption = "@jmcastagnetto, Jesus M. Castagnetto, 2021-04-23",
    fill = ""
  ) +
  theme_void(18) +
  theme(
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(size = 24),
    plot.subtitle = element_text(size = 18, color = "grey50"),
    plot.caption = element_text(family = "Inconsolata", size = 14),
    legend.position = "bottom",
    plot.margin = unit(rep(.5, 4), "cm")
  )

ggsave(
  plot = p2,
  filename = "2021-peru-general-elections/mapa_distrital_presidencial_partsegvuelta_eegg2021.png",
  width = 9,
  height = 14
)
