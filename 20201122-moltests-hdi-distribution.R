library(tidyverse)
library(ggpirate)

pm <- readRDS("data/pm_covid19_ins_peru.rds")

hdi <- readxl::read_excel(
  "data/00.-Nacional-Matriz-Indicadores-Ceplan-Perú-2007-2019_Jun2020.xlsx",
  sheet = "Provincial",
  range = "B9:AD237"
) %>%
  janitor::clean_names() %>%
  mutate(
    indice_de_desarrollo_humano_idh_2019_11 = as.numeric(indice_de_desarrollo_humano_idh_2019_11)
  ) %>%
  select(
    ubigeo,
    departamento_provincia,
    indice_de_desarrollo_humano_idh_2019_11
  ) %>%
  filter(!str_ends(ubigeo, "0000") & !is.na(departamento_provincia)) %>%
  rename(
    idh2019 = indice_de_desarrollo_humano_idh_2019_11,
    provincia = departamento_provincia
  )

# plot(density(hdi$idh2019))
# Figure out the indexes for the bimodal hdi
d <- density(hdi$idh2019)
runs <- rle(sign(diff(d$y)))
# length(runs$lengths)
# [1] 4
mode1 <- runs$lengths[1] + 1
mode2 <- length(d$x) - runs$lengths[4]
max1 <- d$x[mode1]
max2 <- d$x[mode2]

hdi_df <- hdi %>%
  mutate(
    idh_grp = cut(idh2019,
                  breaks = c(0, max1, max2, 1),
                  labels = c("IDH: [0, 0.373)", "IDH: [0.372, 0.568)", "IDH: [0.568, 1)"),
                  ordered_result = TRUE,
                  right = FALSE)
  )

p0 <- ggplot(
  hdi,
  aes(x = idh2019)
) +
  geom_density(fill = "cyan") +
  geom_vline(xintercept = c(max1, max2),
             size = 1,
             color = "red",
             linetype = "dashed") +
  labs(
    x = "IDH (2019)",
    y = "",
    title = "Índice de Desarrollo Humano a nivel de provincia (Perú, 2019)",
    subtitle = "Fuente: https://www.ceplan.gob.pe/informacion-sobre-zonas-y-departamentos-del-peru/",
    caption = "2020-11-22, @jmcastagnetto, Jesus M. Castagnetto"
  ) +
  theme_classic(14) +
  theme(
    plot.title.position = "plot"
  )
p0

ggsave(
  plot = p0,
  filename = "plots/20201122-idh2019-peru-prov.png",
  width = 9,
  height = 5
)

pm_augmented <- pm %>%
  group_by(ubigeo) %>%
  summarise(
    cobertura = n() / pob2020
  ) %>%
  ungroup() %>%
  distinct() %>%
  left_join(
    hdi_df,
    by = "ubigeo"
  )

min_date <- min(pm$fechatomamuestra, na.rm = TRUE)
max_date <- max(pm$fechatomamuestra, na.rm = TRUE)
ins_url <- "https://datos.ins.gob.pe/organization/covid-19"

p1 <- ggplot(
  pm_augmented,
  aes(x = "", y = cobertura)
) +
  geom_pirate(
    aes(color = "", fill = ""),
    bars = FALSE
  ) +
  ggtext::geom_textbox(
    size = 5,
    x = .43,
    y = .07,
    vjust = 0,
    label = "Aproximadamente el **75% de las provincias han hechos pruebas moleculares a menos del 1% de su población**. Y el *50% de las provinicias no llega ni al 0.35%* de la población tamizada."
  ) +
  labs(
    x = "",
    y = "",
    title = "Distribución del porcentaje de la población tamizada con pruebas moleculares",
    subtitle = glue::glue("COVID-19, Perú. Fuente: INS ({ins_url}) - Rango de fechas: [{min_date}, {max_date}]"),
    caption = "2020-11-22, @jmcastagnetto, Jesus M. Castagnetto"
  ) +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  theme_classic(14) +
  theme(
    plot.title.position = "plot"
  )

ggsave(
  plot = p1,
  filename = "plots/20201122-dist-moltest-prov-total.png",
  width = 11,
  height = 9
)

p2 <- ggplot(
  pm_augmented,
  aes(x = idh_grp, y = cobertura)
) +
  geom_pirate(
    aes(color = idh_grp, fill = idh_grp),
    bars = FALSE
  ) +
  labs(
    x = "",
    y = "",
    title = "Distribución del porcentaje de la población tamizada con pruebas moleculares\npor Indice de Desarrollo Humano (2019)",
    subtitle = glue::glue("COVID-19, Perú. Fuente: INS ({ins_url}) - Rango de fechas: [{min_date}, {max_date}]"),
    caption = "2020-11-22, @jmcastagnetto, Jesus M. Castagnetto"
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_brewer(palette = "Dark2") +
  coord_flip() +
  ggtext::geom_textbox(
    size = 5,
    x = 1,
    y = .07,
    label = "Las provincias con **menor IDH**, son aquellas que también tienen **menor cobertura** de tamizaje con pruebas moleculares"
  ) +
  theme_classic(14) +
  theme(
    plot.title.position = "plot"
  )

ggsave(
  plot = p2,
  filename = "plots/20201122-dist-moltest-prov-por-idh.png",
  width = 11,
  height = 9
)
