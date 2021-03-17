library(tidyverse)
library(rnaturalearth)
library(ggpmisc)

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

pm_accum <- pm %>%
  group_by(dep_origen, prov_origen, pob2020) %>%
  tally(name = "tot_pm") %>%
  mutate(
    cobertura = 100 * tot_pm / pob2020,
    cob_grp = cut(cobertura,
                  breaks = c(0, 2, 4, 6, 8, 10),
                  labels = c(
                    "[0%, 2%)",
                    "[2%, 4%)",
                    "[4%, 6%)",
                    "[6%, 8%)",
                    "[8%, 10%)"
                  ),
                  right = FALSE)
  ) %>%
  rename(
    departamento = dep_origen,
    provincia = prov_origen
  )

map_accum <- pe %>%
  left_join(
    pm_accum,
    by = c("departamento", "provincia")
  )

tbl <- pm_accum %>%
  group_by(cob_grp) %>%
  tally(name = "núm. provincias") %>%
  rename("% de cobertura" = cob_grp)

min_date <- min(pm$fechatomamuestra, na.rm = TRUE)
max_date <- max(pm$fechatomamuestra, na.rm = TRUE)
ins_url <- "https://datos.ins.gob.pe/organization/covid-19"

p1 <- ggplot(map_accum) +
  geom_sf(aes(fill = cobertura)) +
  labs(
    title = "Porcentaje de la población tamizada con test moleculares",
    subtitle = glue::glue("COVID-19, Perú. Fuente: INS ({ins_url})\nRango de fechas: [{min_date}, {max_date}]"),
    caption = "2020-11-21, @jmcastagnetto, Jesus M. Castagnetto",
    fill = ""
  ) +
  scale_fill_viridis_b(
    direction = -1,
    na.value = "white",
    labels = c("2%", "4%", "6%", "8%")
  ) +
  theme_minimal(12) +
  theme(
    panel.grid = element_blank(),
    plot.title.position = "plot",
    plot.subtitle = element_text(family = "Inconsolata"),
    plot.caption = element_text(family = "Inconsolata"),
    legend.position = c(.1, .4),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  geom_table_npc(
    data = tbl,
    label = list(tbl),
    npcx = .02,
    npcy = .2,
    hjust = 0,
    vjust = 1
  )

ggsave(
  plot = p1,
  filename = "plots/20201121-pct-pm-prov.png",
  height = 9,
  width = 6
)
