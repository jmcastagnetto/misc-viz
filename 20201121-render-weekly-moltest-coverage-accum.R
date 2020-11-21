library(tidyverse)
library(rnaturalearth)

pe <- readRDS("data/mapa_pe_prov.rds") %>%
  mutate(
    departamento = if_else(
      departamento == "LIMA PROVINCE",
      "LIMA",
      departamento
    ),
    provincia = str_replace_all(
      provincia,
      pattern = c(
        "ANTONIO RAYMONDI" = "ANTONIO RAIMONDI"
      )
    )
  )

pm <- readRDS("data/pm_covid19_ins_peru.rds")

pm_epiweek <- pm %>%
  mutate(
    epiweek = lubridate::epiweek(fechatomamuestra)
  ) %>%
  group_by(epiweek, dep_origen, prov_origen, pob2020) %>%
  tally(name = "tot_tests") %>%
  ungroup() %>%
  arrange(dep_origen, prov_origen, epiweek) %>%
  complete(
    epiweek,
    nesting(dep_origen, prov_origen, pob2020)
  ) %>%
  arrange(dep_origen, prov_origen, epiweek) %>%
  mutate(
    tot_tests = replace_na(tot_tests, 0)
  ) %>%
  group_by(
    dep_origen, prov_origen
  ) %>%
  mutate(
    cob_acum = 100 * cumsum(tot_tests) / pob2020
  ) %>%
  ungroup() %>%
  rename(
    departamento = dep_origen,
    provincia = prov_origen
  ) %>%
  select(
    departamento,
    provincia,
    epiweek,
    cob_acum
  )

min_wk <- min(pm_epiweek$epiweek, na.rm = TRUE)
max_wk <- max(pm_epiweek$epiweek, na.rm = TRUE)

for (wk in min_wk:(max_wk - 1)) {
  cat("Week: ", wk, "\n")
  tmp <- pe %>%
    left_join(
      pm_epiweek %>% filter(epiweek == wk),
      by = c("departamento", "provincia")
    )
  ptmp <- ggplot(tmp) +
    geom_sf(aes(fill = cob_acum)) +
    scale_fill_viridis_b(
      name = "",
      direction = -1,
      limits = c(0, 10),
      na.value = "white",
      breaks = c(2, 4, 6, 8),
      labels = c("2%", "4%", "6%", "8%")
    ) +
    labs(
      title = "Porcentaje de cobertura de pruebas moleculares",
      subtitle = glue::glue("COVID-19, Perú, a nivel de provincia - Semana: {wk}"),
      caption = "Fuente: INS (https://datos.ins.gob.pe/organization/covid-19)\n\nMusic: 'Reverie (small theme)' by _ghost\n(CC-BY, http://ccmixter.org/files/_ghost/25389)\n\n@jmcastagnetto, Jesus M. Castagnetto"
    ) +
    theme_minimal(12) +
    theme(
      plot.background = element_rect(fill = "white", color = "white"),
      panel.grid = element_blank(),
      axis.text = element_blank(),
      plot.caption = element_text(family = "Inconsolata")
    )
  ggsave(
    plot = ptmp,
    filename = glue::glue("tmp/pm_wk_{wk}.png"),
    width = 6,
    height = 9
  )
  rm(list = c("tmp", "ptmp"))
  gc(full = TRUE)
}
